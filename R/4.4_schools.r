############################################################################
############################################################################
###                                                                      ###
###                             SECTION 4.4:                             ###
###                       ACCESSIBILITY TO SCHOOLS                       ###
###                                                                      ###
############################################################################
############################################################################

# Date: 2022-02-09

# Packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)


##---------------------------------------------------------------
##               Import and format schools' data               --
##---------------------------------------------------------------


# Read data ---------------------------------------------------------------

# Read post codes
postcodes <- data.table::fread('data/uk post codes/postcodes_reduced.csv')
# UK post codes
postcodes_all <- data.table::fread('data/uk post codes/ONSPD_AUG_2021_UK/Data/ONSPD_AUG_2021_UK.csv')
# LSOA/DZ geometries
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011/infuse_lsoa_lyr_2011.shp')


# Format data England -----------------------------------------------------

# Filter criteria follows Journey Time Statistics methodology (2019)

# Read England schools - All database
schools_eng <- read_csv('data/schools/england/extract/edubasealldata20211101.csv')
# Names
names(schools_eng) <- gsub(" | \\(|\\)", "_", tolower(names(schools_eng)))   
names(schools_eng) <- sub("_$", "",  names(schools_eng))
# Status is 'Open
schools_eng <- filter(schools_eng, establishmentstatus_name == 'Open')
# Admission policy is not 'Selective
schools_eng <- filter(schools_eng, admissionspolicy_name != 'Selective')

# See number of records for each type
as.data.frame(count(schools_eng, typeofestablishment_name))

# Keep type of establishment according to JTS.
type_establishment <-
  c(
    "Community school",
    "Voluntary aided school",
    "Voluntary controlled school",
    "Foundation school",
    "Academy sponsor led",
    "Academy converter",
    "Free schools",
    "University technical college",
    "Studio schools"
  )
schools_eng <- filter(schools_eng, typeofestablishment_name %in% type_establishment)


# Classify primary schools, secondary schools, based on JTS:
phase_primary <- c('Primary', 'Middle deemed primary', 'All-through')
phase_secondary <- c('Secondary', 'Middle deemed secondary', 'All-through')
schools_eng <- schools_eng %>% 
  mutate(
    type_primary = ifelse(phaseofeducation_name %in% phase_primary, TRUE, FALSE),
    type_secondary = ifelse(phaseofeducation_name %in% phase_secondary & statutorylowage < 16 & statutoryhighage >= 16, TRUE, FALSE)
  )

# Exclude if it is not primary or secondary
schools_eng <- schools_eng %>% 
  filter(type_primary == TRUE | type_secondary == TRUE)

## Location
# check if LSOA is included
schools_eng %>% 
  filter(is.na(lsoa_code) | !grepl("[A-z]", lsoa_code))

# Spatially join LSOA code if missing
schools_eng <- schools_eng %>% 
  filter(!grepl("[A-z]", lsoa_code)) %>% 
  select(-lsoa_code) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = 27700, remove = FALSE) %>% 
  st_join(., select(lsoa_gb, geo_code)) %>% 
  st_set_geometry(NULL) %>% 
  rename(lsoa_code = geo_code) %>% 
  bind_rows(filter(schools_eng, grepl("[A-z]", lsoa_code)))

# Transform CRS
schools_eng <- schools_eng %>% 
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% 
  st_transform(4326) %>% 
  mutate(
    long = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>% 
  st_set_geometry(NULL)

# Select variables
schools_eng <-  schools_eng %>% 
  mutate(
    source_id = urn,
    name = establishmentname,
    address = paste(street, locality, address3, postcode, town, sep = " | "),
    lsoa11 = lsoa_code) %>% 
  select(source_id:address, type_primary:type_secondary, lsoa11, postcode, long:lat)

# Glimpse 
glimpse(schools_eng)


# Format Wales data -------------------------------------------------------

# Read data
schools_wal <- readODS::read_ods('data/schools/wales/address-list-schools-wales.ods', sheet = 'Maintained')

# Names
names(schools_wal) <- gsub("-| | \\(|\\)", "_", tolower(names(schools_wal)))   
names(schools_wal) <- sub("_+", "_",  names(schools_wal))
glimpse(schools_wal)

# Define NAs
schools_wal <- schools_wal %>% 
  mutate(across(everything(), na_if, y = '---'))

# Classify whether they provide primary, secondary or equivalent education
# Considering that middle covers a range from 3 to 19
schools_wal <- schools_wal %>% 
  mutate(
    type_primary = ifelse(sector == 'Primary' | sector == 'Middle', TRUE, FALSE),
    type_secondary = ifelse(sector == 'Secondary' | sector == 'Middle', TRUE, FALSE)
  )

# Exclude if it is not primary or secondary
schools_wal <- schools_wal %>% 
  filter(type_primary == TRUE | type_secondary == TRUE)

# Select variables
schools_wal <-  schools_wal %>% 
  mutate(
    source_id = school_number,
    name = school_name,
    address = paste( address_1,  address_2,  address_3,  address_4, postcode, sep = " | "),
    postcode = str_squish(postcode)) %>% 
  select(source_id:address, type_primary:type_secondary, postcode)

# Input LSOA/DZ and lat/lon based on post code
schools_wal <- left_join(schools_wal, postcodes, by = c("postcode" = "pcds"))

# Some records did not match a post code
filter(schools_wal, is.na(lat))

# Use terminated post codes data where there were not matches,
# assuming post code is outdated from source
schools_wal <- schools_wal %>% 
  filter(is.na(lat)) %>% 
  select(source_id:postcode) %>% 
  left_join(postcodes_all, by = c('postcode' = "pcds")) %>% 
  select(source_id:postcode, lsoa11, lat, long) %>% 
  bind_rows(schools_wal[!is.na(schools_wal$lat),])


# Format Scotland data ----------------------------------------------------

## Read data
# Public schools
schools_sc <- readxl::read_xlsx('data/schools/scotland/school+contact+list+31+July+2021+final.xlsx', skip = 5, sheet = 3)

# Fix col names in data source
names(schools_sc) <- gsub("-| | \\(|\\)", "_", tolower(names(schools_sc))) 

# Classify primary and secondary establishments
schools_sc <- schools_sc %>% 
  mutate(
    type_primary = ifelse(primary_department == 'Yes', TRUE, FALSE),
    type_secondary = ifelse(secondary_department == 'Yes', TRUE, FALSE)
    ) 
# Subset primary or secondary establishments  
schools_sc <- schools_sc %>% 
  filter(type_primary == TRUE | type_secondary == TRUE)


# Select variables
schools_sc <- schools_sc %>% 
  mutate(source_id = seed_code,
         name = school_name,
         address = paste(address_line1, address_line2, address_line3, sep = " | "),
         postcode = str_squish(post_code)) %>% 
  select(source_id:postcode, type_primary:type_secondary)

# Input LSOA/DZ and lat/lon based on post code
schools_sc <- left_join(schools_sc, postcodes, by = c("postcode" = "pcds"))

# Some records did not match a post code
filter(schools_sc, is.na(lat))

# Use terminated post codes data where there were not matches,
# assuming post code is outdated from source
schools_sc <- schools_sc %>% 
  filter(is.na(lat)) %>% 
  select(source_id:type_secondary) %>% 
  left_join(postcodes_all, by = c('postcode' = "pcds")) %>% 
  select(source_id:type_secondary, lsoa11, lat, long) %>%
  bind_rows(schools_sc[!is.na(schools_sc$lat),])


# Bind school data and save result ----------------------------------------

# Bind rows
schools_gb <- bind_rows(schools_eng, schools_wal, schools_sc)
# Glimpse
glimpse(schools_gb)
summary(schools_gb)
# Check LSOA code
schools_gb %>% 
  filter(!grepl("^[A-z]", lsoa11))

# Save data
write_csv(schools_gb, 'data/schools/schools_gb.csv')

# Clean env.
rm(list = ls())


##----------------------------------------------------------------
##                       Access to schools                      --
##----------------------------------------------------------------

# Load packages -----------------------------------------------------------

# Packages
library(tidyverse)
library(data.table)
setDTthreads(0)


# Read data ---------------------------------------------------------------

# Schools in GB
schools_gb <-  fread('data/schools/schools_gb.csv')

# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')
# Centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')

# Read TT by PT
ttm_pt <- fread('output/ttm/ttm_pt_20211122.csv')


# Closest school by PT ----------------------------------------------------


# Count primary schools by LSOA/DZ
primary_count <- schools_gb[type_primary == TRUE, .N, by=.(lsoa11)]
# Count secondary schools by LSOA/DZ
secondary_count <- schools_gb[type_secondary == TRUE, .N, by=.(lsoa11)]


## PT
# Join data
ttm_pt[primary_count, on=c(toId = "lsoa11"), n_primary := N]
ttm_pt[secondary_count, on=c(toId = "lsoa11"), n_secondary := N]
summary(ttm_pt)
# Find nearest primary school by PT
nearestSchool_pt1 <- 
  ttm_pt[!is.na(n_primary), 
         .(nearest_primary = min(travel_time_p050, na.rm = TRUE)),
         by=.(fromId)]
# Transform Inf values to NA
nearestSchool_pt1[,nearest_primary := fifelse(is.infinite(nearest_primary), NA_integer_, nearest_primary)]
# Find nearest secondary school by PT
nearestSchool_pt2 <- 
  ttm_pt[!is.na(n_secondary), 
         .(nearest_secondary = min(travel_time_p050, na.rm = TRUE)),
         by=.(fromId)]
# Transform Inf values to NA
nearestSchool_pt2[,nearest_secondary := fifelse(is.infinite(nearest_secondary), NA_integer_, nearest_secondary)]
# Join
nearestSchool_pt <- full_join(nearestSchool_pt1, nearestSchool_pt2, by = "fromId")


# Cumulative accessibility ------------------------------------------------

# Load accessibility function
source('R/00_fn_accessibility.R')

## PT
# Primary schools
# Access multiple time-cuts to 
# According to TT percentile 50
time_cuts <- seq(15, 120, 15)
access_primary_pt <- 
  lapply(time_cuts, function(x){
    access <- accessibility(ttm_pt, tt = "travel_time_p050", w = "n_primary", beta = x)
    access[,access_pc := (accessibility / sum(primary_count$N)) * 100]
    access[,time_cut := x]
  })
# Show summary
lapply(access_primary_pt, summary)
# Bind estimates in a single DF
access_primary_pt <- rbindlist(access_primary_pt)


# Secondary schools
# Access multiple time-cuts to 
# According to TT percentile 50
access_secondary_pt <- 
  lapply(time_cuts, function(x){
    access <- accessibility(ttm_pt, tt = "travel_time_p050", w = "n_secondary", beta = x)
    access[,access_pc := (accessibility / sum(secondary_count$N)) * 100]
    access[,time_cut := x]
  })
# Show summary
lapply(access_secondary_pt, summary)
# Bind estimates in a single DF
access_secondary_pt <- rbindlist(access_secondary_pt)

# Bind rows
access_pt <-
  list(access_primary_pt, access_secondary_pt) %>% 
  setNames(c('primary', 'secondary')) %>% 
  rbindlist(., idcol = 'type')

##----------------------------------------------------------------
##                 Save accessibility estimates                 --
##----------------------------------------------------------------

# Save data ---------------------------------------------------------------

# Create directory
dir.create('output/accessibility/schools/')
# Read centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')
centroids_gb <- select(centroids_gb, -easting, -northing)

## PT
# Restructure data
access_ptF <- access_pt %>% 
  mutate(access_pc = round(access_pc, 4)) %>% 
  rename(geo_code = fromId, 
         school = accessibility,
         school_pct = access_pc) %>% 
  pivot_wider(id_cols = geo_code, names_from = c(type, time_cut), values_from = school:school_pct)
# Join nearest School
access_ptF <- 
  left_join(centroids_gb, access_ptF, by = 'geo_code') %>% 
  left_join(nearestSchool_pt, by = c('geo_code' = 'fromId'))
summary(access_ptF)
# Save PT accessibility estimates
write_csv(access_ptF, 'output/accessibility/schools/access_school_pt.csv')


# Clean env.
rm(list = ls())









