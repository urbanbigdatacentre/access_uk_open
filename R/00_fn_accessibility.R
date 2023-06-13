# ACCESSIBILITY FUNCTION

# Set environment ---------------------------------------------------------
  
  # Load packages
  library(data.table)
  setDTthreads(0) # Max threads
  library(tidyverse)

# Decaying functions ------------------------------------------------------

  # Cumulative function
  cum_fn <- function(d_ij, w, beta){
      w * fifelse(d_ij <= beta, 1, 0)
    }
  
# Generic accessibility function ------------------------------------------
  
  # Load function to compute access by origin 
  accessibility <- 
    function(ttm, beta = 60, tt = "travel_time", w = "employment") {
    # TTM as data table
    if(!is.data.table(ttm)){
      ttm <- as.data.table(ttm)
    }
    # Rename variable containing weights
    names(ttm)[which(names(ttm) == w)] <- "dest_w"
    # Travel time column
    if(tt != "travel_time"){
      # Rename variable containing travel time
      names(ttm)[which(names(ttm) == tt)] <- "travel_time"
    }
    
    # Compute access weights for each OD pair
    ttm[ ,access_w := cum_fn(d_ij=travel_time, w=dest_w, beta = beta)]
    # Summarise access by origin
    access_dt <- ttm[,list(accessibility = sum(access_w, na.rm = T)), by = .(fromId)]
    
    # Return results
    return(access_dt)
  }
  
