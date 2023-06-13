###########################################################################
###########################################################################
###                                                                     ###
###                            SECTION 4.1:                             ###
###                     ACCESSIBILITY TO EMPLOYMENT                     ###
###                                                                     ###
###########################################################################
###########################################################################

# Date: 2022-01-17 

# 1. Inspect and format employment data for GB (source: nomis, year of ref. 2020)
  # These figures exclude farm agriculture (SIC subclass 01000).
# 2. Compute accessibility to employment


# Packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(data.table)
setDTthreads(0)

# Fn format big number
format2 <- function(x, digits = 2)  formatC(x, format="f", big.mark=" ", digits=digits)


##---------------------------------------------------------------
##            1. Inspect and format employment data            --
##---------------------------------------------------------------

# Read data ---------------------------------------------------------------

# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')
# Employment (NOMIS) data
employment_gb <- read_csv("data/employment/nomis/14410461610488130.csv", skip = 7)

# Format employment data  -------------------------------------------------

# Employment
employment_gb <- employment_gb %>%
  rename_all(tolower) %>% 
  rename(geo_code = mnemonic, 
         employment = total) %>% 
  select(geo_code, area, employment) %>% 
  filter(!is.na(employment))
summary(employment_gb)

# Save employment data
write_csv(employment_gb, 'data/employment/employment_gb.csv')


##----------------------------------------------------------------
##            2. Compute accessibility to employment            --
##----------------------------------------------------------------

# Accessibility PT --------------------------------------------------------

# Load accessibility function
source('R/00_fn_accessibility.R')

# Read employment data
employment_gb <- fread('data/employment/employment_gb.csv')
# LSOA/DZ polygons
lsoa_gb <- st_read('data/uk_dataservice/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp')

# Read TT
ttm_pt <- fread('output/ttm/ttm_pt_20211122.csv')
# Join data (employment in destination)
ttm_pt[employment_gb, on=c(toId = "geo_code"), employment := employment]


# Access multiple time-cuts
# According to TT percentile 50
time_cuts <- seq(15, 120, 15)
access_pt <- 
  lapply(time_cuts, function(x){
    access <- accessibility(ttm_pt, tt = "travel_time_p050", w = "employment", beta = x)
    access[,access_pc := (accessibility / sum(employment_gb$employment)) * 100]
    access[,time_cut := x]
  })
# Show summary
lapply(access_pt, summary)
# Bind estimates in a single DF
access_pt <- rbindlist(access_pt)


##----------------------------------------------------------------
##              3. Map accessibility to employment              --
##----------------------------------------------------------------


# Access Map 2: Multiple time-cuts by PT ----------------------------------

# Join geometries to data
lsoa_access <- dplyr::left_join(access_pt, lsoa_gb, by = c("fromId" = "geo_code"))
lsoa_access <- sf::st_as_sf(lsoa_access)

# Time cuts in map
time_cuts <- c('90 minutes' = 90, '120 minutes' = 120)

# Jenk breaks
n_breaks <- 21
breaks <- BAMMtools::getJenksBreaks(lsoa_access[lsoa_access$time_cut %in% time_cuts,]$access_pc, n_breaks)
br_labs <- paste0(format2(breaks[-n_breaks]), '-', format2(breaks[-1]))
# Labels
caption_m1 <-
  c('Source: 1) Employment, Business Register and Employment Survey (2020) via Nomis;',
    '2) PT timetable: ATOC and Bus Open Data Service (BODS)',
    '3) Contains National Statistics data © Crown copyright and database right 2022')


# Map multiple time-cuts
emp_access_m <- 
  lsoa_access %>%
  filter(time_cut %in% time_cuts) %>% 
  mutate(acces_br = cut(access_pc, breaks, br_labs, include.lowest = TRUE),
         time_cut = factor(time_cut, time_cuts, names(time_cuts))) %>% 
  ggplot() +
  geom_sf(aes(fill = acces_br), col = NA) +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~time_cut, ncol = 2) +
  labs(
    title = 'Access to employment by public transport in Great Britain',
    subtitle = 'Travel time by public transport departing at 7 a.m. considering a 3 hrs. time-window.\nModes include: train, bus, ferry, tram (does not include intercity coaches).\nModel considers 3 rides maximum.',
    fill = 'Employment (%)',
    caption = caption_m1) +
  coord_sf(ylim = c(6000, 1.1e+6)) + # xlim = c(0, 6.5e+5),
  guides(fill=guide_legend(ncol=1)) +
  theme_void() +
  theme(
    plot.title = element_text(colour = 'black', hjust = 0), 
    plot.subtitle = element_text(colour = 'gray15', hjust = 0), 
    plot.caption =  element_text(colour = 'gray15', hjust = 0, size = 7),
    legend.text = element_text(colour = 'gray15', size = 7.5),
    legend.title = element_text(colour = 'black', size = 8.5),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.22, "cm"), 
    legend.position = c(0.1, 0.35),
    strip.text = element_text(colour = 'gray15', size = 9.5))

ggsave('plots/accessibility/empl_PTmult.png', emp_access_m,
       width = 12, height = 10, dpi = 600, bg = 'white')


##----------------------------------------------------------------
##                        Save estimates                        --
##----------------------------------------------------------------

# Save access estimates ---------------------------------------------------

dir.create('output/accessibility')
dir.create('output/accessibility/employment')

# Read centroids
centroids_gb <- read_csv('data/centroids/gb_lsoa_centroid2011.csv')
centroids_gb <- select(centroids_gb, -easting, -northing)

## PT
# Restructure data
access_ptF <- access_pt %>% 
  rename(geo_code = fromId, 
         employment = accessibility,
         employment_pct = access_pc) %>% 
  pivot_wider(id_cols = 1, names_from = time_cut, values_from = 2:3)
# Join geo code label
access_ptF <- left_join(centroids_gb, access_ptF, by = 'geo_code')
# Save accessibility estimates
write_csv(access_ptF, 'output/accessibility/employment/access_employment_pt.csv')

# Clean env.
rm(list = ls())

# -------------------------------------------------------------------------


