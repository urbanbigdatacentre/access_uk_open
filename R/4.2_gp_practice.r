############################################################################
############################################################################
###                                                                      ###
###                             SECTION 4.2:                             ###
###                     ACCESSIBILITY TO GP PRACTICE                     ###
###                                                                      ###
############################################################################
############################################################################


# Packages ----------------------------------------------------------------

library(tidyverse)
library(sf)


##---------------------------------------------------------------
##                      1. Format GP data                      --
##---------------------------------------------------------------

# Read data ---------------------------------------------------------------

# Scotland
isd_scotland <- readxl::read_xlsx('data/gp surgery/isd_scotland/Practice_ContactDetails_Oct2021.xlsx', sheet = 2, skip = 4)
# England and Wales
nhs_eng <- read_csv('data/gp surgery/nhs_england_wales/epraccur/epraccur.csv', col_names = FALSE)

# UK post codes
postcodes_all <- read_csv('data/uk post codes/ONSPD_AUG_2021_UK/Data/ONSPD_AUG_2021_UK.csv')

# Format GP data and assign location by postcode --------------------------

## Post codes
# Subset 'live' post codes in GB
postcodes <- filter(postcodes_all, is.na(doterm) & grepl("^(E|S|W)", oscty))
# Select relevant columns
postcodes <- select(postcodes, pcds, lsoa11, lat, long)
# Save reduced version of post codes
write_csv(postcodes, 'data/uk post codes/postcodes_reduced.csv')


## GP practice
# Subset active GP practices in England and Wales
nhs_eng <- filter(nhs_eng, X13 == "A" & X26 == "4")
# Select variables
nhs_eng <- select(nhs_eng, c(1:2, 10))
nhs_eng <- rename(nhs_eng, practice_code = X1, practice_name = X2, postcode = X10)

# Scotland
isd_scotland <- isd_scotland %>% 
  rename_all(~gsub(' ', "_", tolower(.))) %>% 
  select(practice_code, practice_name, postcode) %>% 
  mutate(practice_code = as.character(practice_code))

# Bind all practices
gp_practices <- bind_rows(nhs_eng, isd_scotland)

# Get location from postcode data
gp_practices <- left_join(gp_practices, postcodes, by = c('postcode' = "pcds"))
# NA matches
sum(is.na(gp_practices$lat))
# Which postcode
gp_practices[is.na(gp_practices$lat),]$postcode

# Use terminated post codes data where there were not matches
# assuming post code is outdated from source
gp_practices <- gp_practices %>%
  filter(is.na(lat)) %>%
  select(practice_code:postcode) %>%
  left_join(postcodes_all, by = c('postcode' = "pcds")) %>%
  select(practice_code:postcode, lsoa11, lat, long) %>%
  bind_rows(gp_practices[!is.na(gp_practices$lat), ])

# Save GP practice data
write_csv(gp_practices, 'data/gp surgery/gp_practiceGB.csv')

# Clean env.
rm(list = ls())


##----------------------------------------------------------------
##               2. Compute access to GP practice               --
##----------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(data.table)
setDTthreads(0)

# Load accessibility function
source('R/00_fn_accessibility.R')
# Fn format big number
format2 <- function(x, digits = 2)  formatC(x, format="f", big.mark=" ", digits=digits)

# Read data ---------------------------------------------------------------

# LSOA polygon
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011/infuse_lsoa_lyr_2011.shp')
# GP location
gp_practice <- fread('data/gp surgery/gp_practiceGB.csv')
# Read TT by PT
ttm_pt <- fread('output/ttm/ttm_pt_20211122.csv')

# Find closest GP practice -------------------------------------------------

# Count number of practices by LSOA
gp_count <- gp_practice[, .N, by=.(lsoa11)]

## PT
# Join data
ttm_pt[gp_count, on=c(toId = "lsoa11"), n_gps := N]
# Find nearest GP
nearestGP_pt <- 
  ttm_pt[!is.na(n_gps), 
       .(nearest_gp = min(travel_time_p050, na.rm = TRUE)),
       by=.(fromId)]
# Transform Inf values to NA
nearestGP_pt[,nearest_gp := fifelse(is.infinite(nearest_gp), NA_integer_, nearest_gp)]


# Cumulative accessibility ------------------------------------------------


## PT
# Access multiple time-cuts
# According to TT percentile 50
time_cuts <- seq(15, 120, 15)
access_pt <- 
  lapply(time_cuts, function(x){
    access <- accessibility(ttm_pt, tt = "travel_time_p050", w = "n_gps", beta = x)
    access[,access_pc := (accessibility / sum(gp_count$N)) * 100]
    access[,time_cut := x]
  })

# Bind estimates in a single DF
access_pt <- rbindlist(access_pt)


##----------------------------------------------------------------
##                 Save accessibility estimates                 --
##----------------------------------------------------------------

# Save estimates ----------------------------------------------------------


# Create directory
dir.create('output/accessibility/gp/')
# Read centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')
centroids_gb <- select(centroids_gb, -easting, -northing)


## PT
# Restructure data
access_ptF <- access_pt %>% 
  mutate(access_pc = round(access_pc, 4)) %>% 
  rename(geo_code = fromId, 
         gp_number = accessibility,
         gp_pct = access_pc) %>% 
  pivot_wider(id_cols = 1, names_from = time_cut, values_from = 2:3)
# Join nearest gp
access_ptF <- 
  left_join(centroids_gb, access_ptF, by = 'geo_code') %>% 
  left_join(nearestGP_pt, by = c('geo_code' = 'fromId'))

# Save accessibility estimates
write_csv(access_ptF, 'output/accessibility/gp/access_gp_pt.csv')


#Clean env.
rm(list = ls())

