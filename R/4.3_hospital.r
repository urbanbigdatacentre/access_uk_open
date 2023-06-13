
############################################################################
############################################################################
###                                                                      ###
###                             SECTION 4.3:                             ###
###                      ACCESSIBILITY TO HOSPITALS                      ###
###                                                                      ###
############################################################################
############################################################################

# Date: 2022-01-27

# Packages
library(tidyverse)
library(data.table)
setDTthreads(0)


##----------------------------------------------------------------
##                  1. Format hospitals' data                   --
##----------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(tidyverse)
library(mapview)
library(sf)
library(rvest)

# Read post codes
postcodes <- data.table::fread('data/uk post codes/postcodes_reduced.csv')
# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011/infuse_lsoa_lyr_2011.shp')

# Key words for specialist hospitals:
# Journey time statistics: 2019
key_specialist <- 
  c(
    'birth', 'maternity', ' eye', 'rheumatic', 'throat', ' nose ', ' ear ',
    'neurology', 'neurosurgery', 'specialist emergency care', 'orthopaedic',
    'heart hospital', 'Children', 'Dental'
  )
# Key for Mental health,  psychiatric, learning disability, or Elderly
key_psy <- c( 'Mental',  'Psychiatr(y|ic)', 'Elder(|ly)', 'learning disabili(ty|ties)', 'Psychogeriatric')
# Day hospital
key_day <- c('Day (hospital|care)')

# Objects to keep 
obj_keep <- c(ls(), "obj_keep")

# Get data from Wales -----------------------------------------------------

# URL
base_url <- "https://www.wales.nhs.uk/ourservices/directory/Hospitals/"

# Fn: to get data form NHS's web page
get_walesData <- 
  function(x){
    # Get data
    hosp1 <- read_html(paste0(base_url, x))
    # Extract text from main content
    hosp1_text <- hosp1 %>% 
      html_nodes(xpath = '//*[@id="centercontent"]/div') %>%
      html_text()
    
    # Extract data only if contains data about hospital
    if(grepl("Type of Hospital", hosp1_text)){
      # Format text
      hosp1_text <- str_split(hosp1_text, pattern = '\\r')[[1]]
      hosp1_text <- gsub("\n\t\t", "", hosp1_text)
      # Name
      name <- hosp1_text[4]
      # Address
      address <- gsub("Type of Hospital:.*", "", hosp1_text[5])
      # Type
      type <- grep('Type of Hospital', hosp1_text, value = TRUE)
      type <- gsub(".*\\Type of Hospital:", "", type)
      type <- gsub("http.*", "", type)
      type <- str_trim(type)
      # Create DF
      output <- data.frame(name, address, type)
      # Results
      return(output)
      
    } else {
      return(NA)
    }
  }


# Apply function
# Consulted on: 2022-02-24
wales_data <- lapply(1:250, get_walesData)
# Save raw data
dir.create('data/hospitals/')
dir.create('data/hospitals/wales/')
saveRDS(wales_data, 'data/hospitals/wales/hospitals_raw.rds')

# Process Wales data ------------------------------------------------------

# Read data
wales_data <- readRDS('data/hospitals/wales/hospitals_raw.rds')

## Format output
# Remove NAs
wales_data <- wales_data %>%
  map(discard, is.na) %>%
  compact()
# Bind rows
wales_data <- bind_rows(wales_data) 
# Clean type
wales_data$type <- str_trim(sub("-$", "", wales_data$type))
# Clean address
wales_data$address <- str_trim(gsub("Tel:.*", "", wales_data$address))
wales_data$address <- str_squish(wales_data$address)
# Extract post code
wales_data$postcode <-  word(wales_data$address, -2, -1)
wales_data$postcode <- sub('Cardiff', '', wales_data$postcode)
sum(is.na(wales_data$postcode))

# View data
glimpse(wales_data)
#View(wales_data)
# Count
count(wales_data, type)


## Filter records to keep key hospital services
# Exclude the following types:
# CHC Local Committee, Psychiatric, Elderly Mental
key_type <- c('CHC Local Committee', 'Psychiatric', 'Elderly Mental', 'Day Hospital') 
wales_data <- wales_data %>% 
  filter(!grepl(paste(key_type, collapse = '|'), type, ignore.case = TRUE)) 

# Exclude specialists according to key words
wales_data <- wales_data %>% 
  filter(!grepl(paste(key_specialist, collapse = '|'), name, ignore.case = TRUE))
# Exclude Mental health or psychiatric
wales_data <- wales_data %>% 
  filter(!grepl(paste(key_psy, collapse = '|'), name, ignore.case = TRUE))
# Exclude Day hospital by name
wales_data <- wales_data %>% 
  filter(!grepl(paste(key_day, collapse = '|'), name, ignore.case = TRUE))

## Location
# Assign LSOA and Lat/Lon based on post code
wales_data <- left_join(wales_data, postcodes, by = c('postcode' = 'pcds'))
# Note: Some locations are in England.
# Check NAs
wales_data %>% filter(is.na(lsoa11))

# Save data
write_csv(wales_data, 'data/hospitals/wales/hospitals_wales.csv')

# Clean env.
rm(list = setdiff(ls(), obj_keep))


# Hospitals data in England -----------------------------------------------

## Read data
# England
hosp_eng <- data.table::fread('data/hospitals/england/Hospital.csv', sep = "¬")

## Filter key hospitals
count(hosp_eng, SubType)
# Exclude psychiatric or mental hospitals using label
hosp_eng <- filter(hosp_eng, SubType != 'Mental Health Hospital')
# Exclude establishments containing key words related to mental health
hosp_eng <- hosp_eng %>% 
  filter(!grepl(paste(key_psy, collapse = '|'), OrganisationName, ignore.case = TRUE))
# Exclude establishments containing key words related specialist units
hosp_eng %>% 
  filter(grepl(paste(key_specialist, collapse = '|'), OrganisationName, ignore.case = TRUE)) %>% 
  pull(OrganisationName)
hosp_eng <- hosp_eng %>% 
  filter(!grepl(paste(key_specialist, collapse = '|'), OrganisationName, ignore.case = TRUE))
# Exclude Local Committee: 0 in England
hosp_eng <- hosp_eng %>% 
  filter(!grepl('Committee', OrganisationName, ignore.case = TRUE))
# Exclude Day hospital
hosp_eng <- hosp_eng %>% 
  filter(!grepl(key_day, OrganisationName, ignore.case = TRUE))


## Missing coordinates
hosp_eng[is.na(Longitude),] # Located in Jersey
# Exclude overseas
hosp_eng <- hosp_eng[OrganisationName != 'Jersey General Hospital',]

# Input LSOA based on post code
hosp_eng <- 
  left_join(hosp_eng, select(postcodes, -lat:-long), by = c('Postcode' = 'pcds'))
# Not matched
hosp_eng[is.na(lsoa11),]

# Spatially join LSOA
hosp_eng <- hosp_eng %>% 
  filter(is.na(lsoa11)) %>% 
  select(-lsoa11) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = FALSE) %>% 
  st_transform(st_crs(lsoa_gb)) %>% 
  st_join(., select(lsoa_gb, geo_code)) %>% 
  rename(lsoa11 = geo_code) %>%
  st_set_geometry(NULL) %>% 
  bind_rows(hosp_eng[!is.na(hosp_eng$lsoa11)])
# Set DT
setDT(hosp_eng)

# Missing LSOA?
hosp_eng[is.na(lsoa11) | lsoa11 == "NA",]

glimpse(hosp_eng)


# Hospitals data in Scotland ----------------------------------------------

# Data from Scotland
hosp_sco <-  data.table::fread('data/hospitals/scotland/c698f450-eeed-41a0-88f7-c1e40a568acc.csv')

## Filter key hospitals
# Exclude establishments containing key words related to mental health
hosp_sco <- hosp_sco %>% 
  filter(!grepl(paste(key_psy, collapse = '|'), LocationName, ignore.case = TRUE))
# Exclude establishments containing key words related specialist units
hosp_sco %>% 
  filter(grepl(paste(key_specialist, collapse = '|'), LocationName, ignore.case = TRUE)) %>% 
  pull(LocationName)
hosp_sco <- hosp_sco %>% 
  filter(!grepl(paste(key_specialist, collapse = '|'), LocationName, ignore.case = TRUE))
# Exclude Local Committee: 0 in Scotland
hosp_sco <- hosp_sco %>% 
  filter(!grepl('Committee', LocationName, ignore.case = TRUE))
# Exclude Day hospital
hosp_sco <- hosp_sco %>% 
  filter(!grepl(key_day, LocationName, ignore.case = TRUE))


# Assign DZ if missing 
hosp_sco <- hosp_sco %>% 
  filter(DataZone == "") %>% 
  st_as_sf(coords = c("XCoordinate", "YCoordinate"), crs = 27700, remove = FALSE) %>% 
  st_join(., select(lsoa_gb, geo_code)) %>% 
  st_set_geometry(NULL) %>% 
  bind_rows(hosp_sco[DataZone != ""]) %>% 
  mutate(geo_code = coalesce(geo_code, DataZone)) 

# Extract Lat/Lon coordinates
hosp_sco <- hosp_sco %>%
  st_as_sf(coords = c('XCoordinate', 'YCoordinate'), crs = 27700) %>% 
  st_transform(4326) %>% 
  cbind(st_coordinates(.)) %>% 
  rename(Longitude = X, Latitude = Y) %>% 
  st_set_geometry(NULL)


# Merge data in GB -------------------------------------------------------

# Read Wales data
wales_data <- read_csv('data/hospitals/wales/hospitals_wales.csv')

## make variables' names compatible
# England:
hosp_engF <- hosp_eng %>%
  mutate(address = paste(Address1, Address2, Address3, sep = '|')) %>% 
  select(OrganisationID, OrganisationName, address, Postcode, Latitude, Longitude, lsoa11) %>% 
  rename_all(tolower) %>% 
  rename(source_id = organisationid, name = organisationname, lsoa = lsoa11)
# Scotland:
hosp_scoF <- hosp_sco %>% 
  select(X_id, LocationName:AddressLine, geo_code, Longitude:Latitude) %>% 
  rename_all(tolower) %>% 
  rename(source_id = x_id,  name = locationname, address = addressline, lsoa = geo_code)
# Wales: (remove CHC Local Committee)
wales_dataF <- wales_data %>% 
  filter(type != 'CHC Local Committee') %>% 
  select(-type) %>% 
  rename(latitude = lat, longitude = long, lsoa = lsoa11)

# Bind list
hops_gb <- bind_rows(hosp_engF,  hosp_scoF, wales_dataF)

# Save data
write_csv(hops_gb, 'data/hospitals/hospitals_gb.csv')

# Clean env. 
rm(list = ls())
gc()



##----------------------------------------------------------------
##                2. Compute access to hospitals                --
##----------------------------------------------------------------


# Load packages -----------------------------------------------------------

# Packages
library(sf)
library(tidyverse)
library(data.table)
setDTthreads(0)

# Load accessibility function
source('R/00_fn_accessibility.R')
# Fn format big number
format2 <- function(x, digits = 2)  formatC(x, format="f", big.mark=" ", digits=digits)

# Read data ---------------------------------------------------------------

# Hospitals in GB
hospitals_gb <- fread('data/hospitals/hospitals_gb.csv')
# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')
# Centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')

# Read TT by PT
ttm_pt <- fread('output/ttm/ttm_pt_20211122.csv')

# Find closest hospital ---------------------------------------------------

# Count number of hospitals by LSOA
hosp_count <- hospitals_gb[, .N, by=.(lsoa)]

## PT
# Join data
ttm_pt[hosp_count, on=c(toId = "lsoa"), n_hosp := N]
# Find nearest hospital
nearestHosp_pt <- 
  ttm_pt[!is.na(n_hosp), 
         .(nearest_hosp = min(travel_time_p050, na.rm = TRUE)),
         by=.(fromId)]
# Transform Inf values to NA
nearestHosp_pt[,nearest_hosp := fifelse(is.infinite(nearest_hosp), NA_integer_, nearest_hosp)]


# Cumulative accessibility  -----------------------------------------------

## PT
# Access multiple time-cuts
# According to TT percentile 50
time_cuts <- seq(15, 120, 15)
access_pt <- 
  lapply(time_cuts, function(x){
    access <- accessibility(ttm_pt, tt = "travel_time_p050", w = "n_hosp", beta = x)
    access[,access_pc := (accessibility / sum(hosp_count$N)) * 100]
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
dir.create('output/accessibility/hospitals')
# Read centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')
centroids_gb <- select(centroids_gb, -easting, -northing)


## PT
# Restructure data
access_ptF <- access_pt %>% 
  mutate(access_pc = round(access_pc, 4)) %>% 
  rename(geo_code = fromId, 
         hospitals = accessibility,
         hospitals_pct = access_pc) %>% 
  pivot_wider(id_cols = geo_code, names_from = time_cut, values_from = hospitals:hospitals_pct)

# Join nearest hospital
access_ptF <- 
  left_join(centroids_gb, access_ptF, by = 'geo_code') %>% 
  left_join(nearestHosp_pt, by = c('geo_code' = 'fromId'))
summary(access_ptF)
# Save PT accessibility estimates
write_csv(access_ptF, 'output/accessibility/hospitals/access_hospital_pt.csv')

# Clean env.
rm(list = ls())


# -------------------------------------------------------------------------



