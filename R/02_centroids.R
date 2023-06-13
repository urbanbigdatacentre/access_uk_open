
############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                   DEFINE OD POINTS. LSOA CENTROIDS                   ###
###                                                                      ###
############################################################################
############################################################################



# Packages ----------------------------------------------------------------

  library(sf)
  library(tidyverse)
  library(mapview)


# Read data ---------------------------------------------------------------


  # LSOA polygons
  lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011/infuse_lsoa_lyr_2011.shp')
  # Scotland pop. weighted centroids
  scot_cent <- st_read('data/scottish_gov/SG_DataZoneCent_2011/SG_DataZone_Cent_2011.shp')
  # England pop. weighted centroids 
  engl_cent <- st_read('data/uk_gov/Lower_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids/Lower_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids.shp')
  

# Make sure points correspond to geometries for the whole UK --------------

  # Spatial join to get geo_code from geometries
  # Scotland
  scot_cent2 <- st_join(scot_cent, lsoa_gb)
  # Are geo_code in cent the same as in polygons?
  sum(scot_cent2$DataZone != scot_cent2$geo_code)
  # England
  engl_cent2 <- st_join(engl_cent, lsoa_gb)
  # Are geo_code in cent the same as in polygons?
  sum(engl_cent2$lsoa11cd != engl_cent2$geo_code)


# Merge and save pop. w. centroids ----------------------------------------
  
  # Rename name/code to make compatible
  scot_cent <- scot_cent %>% 
    rename(geo_code = DataZone,
           geo_label = Name)
  engl_cent <- engl_cent %>% 
    rename(geo_code = lsoa11cd,
           geo_label = lsoa11nm)
  
  # Bind Scotland and England centroids
  gb_cent <- bind_rows(scot_cent, engl_cent)
  
  # Select variables
  gb_cent <- gb_cent %>% 
    select(-TotPop2011:-objectid)

  # Save GB centroids 
  dir.create('data/centroids')
  # As GPKG
  st_write(gb_cent, 'data/centroids/gb_lsoa_centroid2011.gpkg')
  # As csv
  gb_centdf <- gb_cent %>% 
    cbind(st_coordinates(.)) %>% 
    rename(northing = Y,
           easting = X) %>% 
    st_set_geometry(NULL)
  write_csv(gb_centdf, 'data/centroids/gb_lsoa_centroid2011.csv')
  
  
  # Clean env.
  rm(list = ls())
  gc(reset = TRUE)
    
