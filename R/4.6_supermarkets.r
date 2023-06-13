###########################################################################
###########################################################################
###                                                                     ###
###                            SECTION 4.6:                             ###
###                    ACCESSIBILITY TO SUPERMARKETS                    ###
###                                                                     ###
###########################################################################
###########################################################################

# Date: 2022-02-24 

# Packages ----------------------------------------------------------------

library(tidyverse)
library(mapview)
library(sf)

##---------------------------------------------------------------
##                         Format data                         --
##---------------------------------------------------------------


# Read data ---------------------------------------------------------------

# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')

# Get OSM supermarket data ------------------------------------------------

library(osmdata)

# Bounding box
uk <- opq_osm_id(id = 62149, type = "relation") %>%
  opq_string() %>%
  osmdata_sf()
bb <- getbb("United Kingdom", featuretype = "country")

# # Download supermarket data
# supermarket_all <- bb %>% 
#   opq(timeout = 60*30) %>% 
#   add_osm_feature(key = 'shop', value = 'supermarket') %>% 
#   osmdata_sf()
# # Save raw OSM data as RDS
# dir.create('data/shops/osm')
# saveRDS(supermarket_all, 'data/shops/osm/supermarket_raw.rds')

# Read raw OSM data
supermarket_all <- readRDS('data/shops/osm/supermarket_raw.rds')

# Filter points including names only
supermarket_points <- 
  supermarket_all$osm_points %>% 
  filter(!is.na(name)) %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

# Supermarket polygon as centroid
supermarket_poly <- 
  supermarket_all$osm_polygons %>% 
  st_centroid(.) %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])
supermarket_poly$area_sqm <- 
  supermarket_all$osm_polygons %>% 
  st_transform(27700) %>% 
  st_area(.) %>% 
  as.numeric

# Filter 'large' shop > 280 sqm.
# Ref: The Sunday Trading Act 1994 (the STA 1994)
# https://commonslibrary.parliament.uk/research-briefings/sn05522/
supermarket_poly <- supermarket_poly %>% 
  filter(area_sqm > 280)

# Map supermarket centroids
supermarket_poly %>% 
  filter(lat > 55.6 & lat < 56) %>% 
  mapview(col.region = "red")

# Bind rows
supermarket_bind <- 
  bind_rows(supermarket_points, supermarket_poly)
# Subset amenity is NA
supermarket_bind <- filter(supermarket_bind, is.na(amenity))
# Select relevant variables
supermarket_bind <- select(supermarket_bind, osm_id, name, brand, area_sqm, long, lat)
summary(supermarket_bind)

# Show the most frequent brands
brand_freq <- sort(table(supermarket_bind$brand), decreasing = TRUE)
brand_freq[1:20]

# Filter larger brands
brands <- c('Lidl', 'ALDI', 'Co-op', 'Sainsbury', 'Tesco','Asda','Morrisons')
supermarket_bind <- supermarket_bind %>% 
  filter(grepl(paste(brands, collapse = '|'), name, ignore.case = TRUE) |
           grepl(paste(brands, collapse = '|'), brand, ignore.case = TRUE) )

# Filter objects within the UK
supermarkets_gb <-supermarket_bind %>%
  filter(st_intersects(., uk$osm_multipolygons, sparse = FALSE))
# View
View(supermarkets_gb)

# Spatially join LSOA/DZ
supermarkets_gb <- supermarkets_gb %>% 
  st_transform(st_crs(lsoa_gb)) %>% 
  st_join(., select(lsoa_gb, geo_code))

# Check for missing LSOA
supermarkets_gb %>% 
  filter(!grepl("^[A-z]", geo_code)) %>% 
  mapview()

# Keep Supermarkets within GB only
supermarkets_gb <- supermarkets_gb %>% 
  filter(grepl("^(E|S|W)", geo_code))

# Save data
supermarkets_gb <- st_set_geometry(supermarkets_gb, NULL)
write_csv(supermarkets_gb, 'data/shops/supermarkets_osm.csv')


##----------------------------------------------------------------
##                    Estimate accessibility                    --
##----------------------------------------------------------------

# Read data ---------------------------------------------------------------

library(data.table)
setDTthreads(0)

# Supermarkets
supermarkets <- read_csv('data/shops/supermarkets_osm.csv')

# Read TT by PT
ttm_pt <- fread('output/ttm/ttm_pt_20211122.csv')

# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')

# Load accessibility function
source('R/00_fn_accessibility.R')
# Fn format big number
format2 <- function(x, digits = 2)  formatC(x, format="f", big.mark=" ", digits=digits)


# Nearest supermarket -----------------------------------------------------

# Count supermarkets by LSOA/DZ
supermarket_count <- count(supermarkets, geo_code)

## PT
# Join data
ttm_pt[setDT(supermarket_count), on=c(toId = "geo_code"), n_supermkt := n]
# Find nearest supermarket
nearestSuper_pt <- 
  ttm_pt[!is.na(n_supermkt), 
         .(nearest_supermarket = min(travel_time_p050, na.rm = TRUE)),
         by=.(fromId)]
# Transform Inf values to NA
nearestSuper_pt[,nearest_supermarket := fifelse(is.infinite(nearest_supermarket), NA_integer_, nearest_supermarket)]


# Cumulative accessibility to supermarkets --------------------------------


## PT
# Access multiple time-cuts
# According to TT percentile 50
time_cuts <- seq(15, 120, 15)
access_pt <- 
  lapply(time_cuts, function(x){
    access <- accessibility(ttm_pt, tt = "travel_time_p050", w = "n_supermkt", beta = x)
    access[,access_pc := (accessibility / sum(supermarket_count$n)) * 100]
    access[,time_cut := x]
  })
# Show summary
lapply(access_pt, summary)
# Bind estimates in a single DF
access_pt <- rbindlist(access_pt)



##----------------------------------------------------------------
##                        Save estimates                        --
##----------------------------------------------------------------

# Save data ---------------------------------------------------------------

# Create dir
dir.create('output/accessibility/supermarket')
# Read centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')
centroids_gb <- select(centroids_gb, -easting, -northing)

## PT
# Restructure data
access_ptF <- access_pt %>% 
  mutate(access_pc = round(access_pc, 4)) %>% 
  rename(geo_code = fromId, 
         supermarket = accessibility,
         supermarket_pct = access_pc) %>% 
  pivot_wider(id_cols = geo_code, names_from = time_cut, values_from = supermarket:supermarket_pct)

# Join nearest supermarket
access_ptF <- 
  left_join(centroids_gb, access_ptF, by = 'geo_code') %>% 
  left_join(nearestSuper_pt, by = c('geo_code' = 'fromId'))
summary(access_ptF)
# Save PT accessibility estimates
write_csv(access_ptF, 'output/accessibility/supermarket/access_supermarkets_pt.csv')


# Clean env.
rm(list = ls())



