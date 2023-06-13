###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                          TRANSFORM TO GTFS                          ###
###                                                                     ###
###########################################################################
###########################################################################


# Load packages -----------------------------------------------------------


  remotes::install_github("ITSleeds/UK2GTFS")
  library(UK2GTFS)
  library(tidyverse)
  library(sf)
  library(mapview)


# ATOC files to GTFS - (rail) ---------------------------------------------

  # Transform atoc to gtfs
  path_in <- "data/atoc/ttis194.zip"
  ttis194 <- atoc2gtfs(path_in = path_in, shapes = TRUE, ncores = 14)
  
  ## Inspect output
  # Calendar
  glimpse(ttis194$calendar)
  summary(parse_date(ttis194$calendar$start_date, format = "%Y%m%d"))
  summary(parse_date(ttis194$calendar$end_date, format = "%Y%m%d"))
  barplot(table(ttis194$calendar$end_date))
  
  # Stops
  ttis194$stops %>%
    st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326) %>%
    mapview()
  
  # Check internal validity
  UK2GTFS::gtfs_validate_internal(ttis194)
  # Warning messages:
  # 1: In UK2GTFS::gtfs_validate_internal(ttis194) : NA values in stops
  # 2: In UK2GTFS::gtfs_validate_internal(ttis194) :
  #   Unknown stop_id in stop_times
  
  ## Force valid. This function does not fix problems it just removes them
  ttis194_gtfs <- UK2GTFS::gtfs_force_valid(ttis194)
  ## Compare original and valid
  # Find difference
  map2(ttis194, ttis194_gtfs, identical)
  # Stop times not included in GTFS version
  anti_join(ttis194$stop_times, ttis194_gtfs$stop_times)
  # trip_id arrival_time departure_time stop_id stop_sequence pickup_type drop_off_type
  # 1   12400     21:38:00       21:40:00 RESTSTN            12           0             0
  # 2   66575     16:45:00       16:49:00 SOHA491             4           0             0
  # 3   63914     12:45:00       12:49:00 SOHA491             4           0             0
  # 4   52802     31:45:00       32:37:00 WMBYEFR            17           0             0

  ## Write as GTFS
  UK2GTFS::gtfs_write(ttis194_gtfs, folder = 'data/atoc/', name = 'ttis194.gtfs')
  
  # Clean env.
  rm(list = ls())
  gc(reset = TRUE)

# Check GTFS ------------------------------------------------------------
  
  library(tidytransit)
  library(lubridate)
  
  # Read ATOC GTFS
  train_gtfs <- read_gtfs('data/atoc/ttis194.gtfs.zip')
  summary(train_gtfs)
  
  # Validation status
  atoc_valid <- attr(train_gtfs, "validation_result")
  write_csv(atoc_valid, 'data/atoc/ttis194_validation.csv')
  
  # Clean
  rm(train_gtfs)
  gc()
  
  
  # Read transxchange GTFS
  bus_gtfs <- read_gtfs('data/transxchange/itm_all.gtfs.zip', quiet = FALSE)
  summary(bus_gtfs)
  
  # Validation status
  bus_valid <- attr(bus_gtfs, "validation_result")
  write_csv(bus_valid, 'data/transxchange/itm_validation.csv')
  
  # Clean env.
  rm(list = ls())
  gc(reset = TRUE)
  
  
