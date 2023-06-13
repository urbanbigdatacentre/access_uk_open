############################################################################
############################################################################
###                                                                      ###
###                              SECTION 6:                              ###
###                          DATA INPUT SUMMARY                          ###
###                                                                      ###
############################################################################
############################################################################


library(tidyverse)
library(sf)

# Origins -----------------------------------------------------------------

# Read centroids
centriods <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')
# Read population 
pop_gb <- read_csv('data/population/population_gb.csv')
# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011/infuse_lsoa_lyr_2011.shp')

# Polygon's area
lsoa_gb$area_sqk <- as.numeric(st_area(lsoa_gb)) / 1e6
# Join pop/area data
centriods <- centriods %>% 
  left_join(st_set_geometry(lsoa_gb, NULL), by = "geo_code") %>% 
  left_join(pop_gb, by = c("geo_code" = "datazone2011code"))
# Assign names 
centriods <- centriods %>% 
  mutate(country = str_sub(geo_code, 0, 1),
         country = factor(country, labels = c("England", "Scotland", "Wales")))
# GB summary
sum_gb <- centriods %>% 
  summarise(
    N = n(),
    mean_pop = mean(total_population),
    mean_area = mean(area_sqk)
  ) %>% 
  mutate(country = 'Great Britain')
# Summary by region
origins_summary <- centriods %>% 
  group_by(country) %>% 
  summarise(
    N = n(),
    mean_pop = mean(total_population),
    mean_area = mean(area_sqk)
  ) %>% 
  bind_rows(sum_gb) %>% 
  mutate(across(mean_pop:mean_area, round, 2))
# Format summary
origins_summary <- 
  data.frame(t(origins_summary[,-1])) %>% 
  setNames(origins_summary$country) %>% 
  rownames_to_column(var = 'LSOA/DZ')
# Print summary
origins_summary
# Save table
# New folder
dir.create('supplemet')
write_csv(origins_summary, 'supplemet/origins_summary.csv')


# Destinations ------------------------------------------------------------


## Destinations summary 
dest <- c("employment", "gp surgery", "hospitals", "schools", "shop", "urban_centres")
data_f <- list.dirs('data', recursive = FALSE)
data_f <- grep(paste(dest, collapse = '|'), data_f, value = TRUE)
data_f <- lapply(data_f, list.files, full.names = TRUE, pattern = "csv$|gpkg$")
names(data_f) <- dest
data_f <- unlist(data_f)
# Exclude convenience shops
data_f <- grep('convenience', data_f, invert= TRUE, value = TRUE)

# Read data
dest_data <- lapply(data_f, st_read)
dest_data <- lapply(dest_data, as.data.frame)

# Split Primary and secondary schools
dest_data$schools_secondary <- filter(dest_data$schools, type_secondary == TRUE)
dest_data$schools <- filter(dest_data$schools, type_primary == TRUE)



# Fn: summarise count or aggregated data
summary_region <- function(dataset, operation = 'count', var = NULL) {
  # Extract region
  if(!'geo_code' %in% names(dataset)){
    geocode_index <- grep('lsoa', names(dataset))
    names(dataset)[geocode_index] <- 'geo_code'
  }
  dataset$region <- str_sub(dataset$geo_code, 0, 1)
  dataset$region <- factor(dataset$region, c("E", "S", "W"), c('England', "Scotland", "Wales"))
  
  # Summarise by region 
  if(operation == 'count'){
    summary <- count(dataset, region)
  } else if (operation == 'sum'){
    summary <- dataset %>% 
      group_by(region) %>% 
      summarise(n = sum({{var}}))
  } else {
    cat("Use appropiate operation")
    stop()
  }
  # Result
  return(summary)
}


# Summary destinations
dest_summ <- lapply(dest_data[-1], summary_region)
dest_summ$employment <- summary_region(dest_data$employment, operation = "sum", var = as.numeric(employment))
dest_summ <- bind_rows(dest_summ, .id = 'destination')
dest_summ <- pivot_wider(dest_summ, values_from = n, names_from = region)
dest_summ$Wales <- replace_na(dest_summ$Wales, 0)
# GB summary
dest_summ <- dest_summ %>% 
  rowwise() %>% 
  mutate(`Great Britain` = sum(England, Scotland, Wales))
# Name of destinations
dest_lev <- c('employment', 'gp surgery', 'hospitals', 'schools', 'schools_secondary', 'urban_centres1', 'urban_centres2', 'shop')
dest_lab <- c("Employment", "GPs", "Hospitals", "Education: Primary schools", "Education: Secondary schools",  "Urban centre: Main", "Urban centre: Subcentre", "Supermarkets")
dest_or <- match(dest_lev, dest_summ$destination)
dest_summ <- dest_summ[dest_or,]
dest_summ$destination <- dest_lab
# Print summary
dest_summ

# Save destinations summary
write_csv(dest_summ, 'supplemet/destinations_summary.csv')
