
###########################################################################
###########################################################################
###                                                                     ###
###                            SECTION 4.5:                             ###
###                       ACCESS TO URBAN CENTRES                       ###
###                                                                     ###
###########################################################################
###########################################################################

# Date: 2022-03-16

##----------------------------------------------------------------
##                        1. Format data                        --
##----------------------------------------------------------------

# Load packages
library(tidyverse)
library(sf)
library(mapview)

# References
# Article: https://doi.org/10.1177%2F0042098019860776
# GitHub repository: https://github.com/MengLeZhang/decentralisationPaper2


# Read data ---------------------------------------------------------------

# LSOA-TTWA-centres lookup - Meng Le Zhang github (nearest main/sub-centre)
df_ttwa_read <- read_csv("https://raw.githubusercontent.com/MengLeZhang/decentralisationPaper2/master/Saved%20generated%20data/Distance%20from%20nearest%20centre%20for%20zones%20and%20TTWA%20lkp.csv")
# Full list of locations. Imputed centres based on osmdata for England and Scotland.
inputed_centres <- read_csv('https://raw.githubusercontent.com/MengLeZhang/decentralisationPaper2/master/Saved%20generated%20data/Imputed%20centres%20based%20on%20osmdata%20for%20England%20and%20Scotland.csv')

# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011/infuse_lsoa_lyr_2011.shp')


# Identify main/secondary BUA ---------------------------------------------

# Full list of centres as SF according to imputed coordinates
inputed_centres <- inputed_centres %>%
  st_as_sf(coords = c('imputed_easting', 'imputed_northing'), crs = 27700) 
# Assign LSOA/DZ code to imputed locations
inputed_centres <- inputed_centres %>% 
  st_join(., select(lsoa_gb, geo_code))

# There are 4 that could not be spatially joined
inputed_centres %>% 
  filter(is.na(geo_code)) %>% 
  mapview()
# Assign nearest when code is missing
inputed_centres <- inputed_centres %>% 
  filter(is.na(geo_code)) %>% 
  select(-geo_code) %>% 
  st_join(., select(lsoa_gb, geo_code), join = st_nearest_feature) %>% 
  bind_rows(filter(inputed_centres, !is.na(geo_code)))


# Filter ttwa to 2011 zones
df_ttwa_lookup <- df_ttwa_read %>% 
  filter(zone_type == "lsoa11" | zone_type == "dz11") %>% 
  rename(lsoa_code = zone)
summary(df_ttwa_lookup)

# Create list of unique names of main and secondary BUA
main_bua <- unique(df_ttwa_lookup$main_bua)
nearest_bua <- unique(df_ttwa_lookup$nearest_bua)

# Identify main BUA form full list of imputed locations
main_bua <- inputed_centres %>% 
  filter(name %in% main_bua)
# Identify secondary BUA
secondary_bua <- inputed_centres %>% 
  filter(name %in% nearest_bua)

# Select relevant variables
main_bua <- select(main_bua, name, pop11, geo_code)
secondary_bua <- select(secondary_bua, name, pop11, geo_code)


# Save locations
st_write(main_bua, 'data/urban_centres/main_bua.gpkg')
st_write(secondary_bua, 'data/urban_centres/sub_bua.gpkg')

# Clean env.
rm(list = ls())


##----------------------------------------------------------------
##                  2. Access to urban centres                  --
##---------------------------------------------------------------

library(data.table)
setDTthreads(0)

# Read data ---------------------------------------------------------------

# Cities
cities_uk <- 
  list.files('data/urban_centres/', pattern = 'gpkg', full.names = TRUE) %>% 
  lapply(st_read) %>% 
  setNames(c("main_bua", "sub_bua"))

# Read TT by PT
ttm_pt <- fread('output/ttm/ttm_pt_20211122.csv')
# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')


# Find nearest main urban centre by PT ------------------------------------

## PT
# Join data
ttm_pt[as.data.table(cities_uk$main_bua), on=c(toId = "geo_code"), main_bua := name]
ttm_pt[as.data.table(cities_uk$sub_bua), on=c(toId = "geo_code"), sub_bua := name]

# Number of main centres
n_distinct(ttm_pt$main_bua, na.rm = TRUE)
# Number of sub-centres
n_distinct(ttm_pt$sub_bua, na.rm = TRUE)


# Find nearest main_bua
nearestMain_bua_pt1 <- 
  ttm_pt[!is.na(main_bua), 
         .(nearest_main_bua = min(travel_time_p050, na.rm = TRUE)),
         by=.(fromId)]
# Transform Inf values to NA
nearestMain_bua_pt1[,nearest_main_bua := fifelse(is.infinite(nearest_main_bua), NA_integer_, nearest_main_bua)]

# Find nearest sub_bua
nearestSub_bua_pt2 <- 
  ttm_pt[!is.na(sub_bua), 
         .(nearest_sub_bua = min(travel_time_p050, na.rm = TRUE)),
         by=.(fromId)]
# Transform Inf values to NA
nearestSub_bua_pt2[,nearest_sub_bua := fifelse(is.infinite(nearest_sub_bua), NA_integer_, nearest_sub_bua)]
# Join
nearestBua_pt <- full_join(nearestMain_bua_pt1, nearestSub_bua_pt2, by = "fromId")
# Summary
summary(nearestBua_pt)



##----------------------------------------------------------------
##                        Save estimates                        --
##----------------------------------------------------------------

# Save estimates ----------------------------------------------------------


dir.create('output/accessibility/urban_centre')

# Read centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')
centroids_gb <- select(centroids_gb, -easting, -northing)

# PT
nearestCity_ptF <- centroids_gb %>%
  left_join(nearestBua_pt, by = c('geo_code' = 'fromId'))
summary(nearestCity_ptF)
# Write csv
write_csv(nearestCity_ptF, 'output/accessibility/urban_centre/access_cities_pt.csv')


# Clean env.
rm(list = ls())



# -------------------------------------------------------------------------







