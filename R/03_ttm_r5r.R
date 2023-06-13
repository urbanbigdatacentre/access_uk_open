###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 3:                             ###
###                             COMPUTE TTM                             ###
###                                                                     ###
###########################################################################
###########################################################################


# Set r5r and java pars ---------------------------------------------------

options(java.parameters = "-Xmx62G")
library(r5r)


# Build/Load network ------------------------------------------------------

# R5R Directory
r5r_dir <- 'data/ttm_r5r'

# # Copy input files to create R5 network
# input_files <- list.files('data', recursive = TRUE, pattern = 'gtfs\\.zip$|pbf$', full.names = TRUE)
# lapply(input_files, function(x) file.copy(x, paste0(r5r_dir, '/', basename(x)), overwrite = TRUE))

# Create/read Network (takes long)
# Indicate the path where OSM and GTFS data are stored
r5r_core <- setup_r5(data_path = r5r_dir, verbose = TRUE)
gc(reset = TRUE)


# Routing parameters ------------------------------------------------------

library(sf)
library(tidyverse)
library(data.table)
setDTthreads(0)

# Read LSOA centroids
centroids <- st_read('data/centroids/gb_lsoa_centroid2011.gpkg')
centroids <- st_transform(centroids, 4326)
centroids <- rename(centroids, id = geo_code)

# Route details
# Reference
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853603/notes-and-definitions.pdf#page=6
# https://www.sthelens.gov.uk/media/331745/cd-2229-wyg_how-far-do-people-walk.pdf

# Routing inputs
mode <-c("WALK", "TRANSIT")
# Max trip duration
max_trip_duration <- 120L
# Max walking distance
# As default:  no restrictions as long as max_trip_duration is respected
max_walk <- Inf
# Walk speed 
walk_speed <- 4.8
# Tuesday 7 am
departure_datetime <- as.POSIXct("22-11-2021 07:00:00", format = "%d-%m-%Y %H:%M:%S", tz = 'Europe/London')
# Time window
time_window <- 3*60
# Variation in time window
percentiles <- c(25, 50, 75)


# TT public transport -----------------------------------------------------

gc()

start_time <- Sys.time()
# Calculate a travel time matrix. (Takes long)
ttm <-
  travel_time_matrix(r5r_core = r5r_core,
                     origins = centroids,
                     destinations = centroids,
                     mode = mode,
                     departure_datetime = departure_datetime,
                     max_trip_duration = max_trip_duration,
                     max_walk_dist = max_walk,
                     max_rides = 3,
                     walk_speed = walk_speed,
                     n_threads = Inf,
                     percentiles = percentiles,
                     time_window = time_window)
end_time <- Sys.time()
end_time - start_time
summary(ttm)

# Save ttm
date_s <- format(departure_datetime, "%Y%m%d")
write_csv(ttm, paste0('output/ttm/ttm_pt_', date_s, '.csv'))

# Clean env.
rm(ttm)
gc()


# -------------------------------------------------------------------------










 