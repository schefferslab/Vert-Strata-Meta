## David Klinges
## Recoding forest type classifications

## Pseudo-code

## Because each of the rasters is so large, hitting memory limitations. So, to
## address this, am running the code as a loop through each of the raster files
## (each one corresponding to a forest type) and then removing the raster objects
## from the workspace at the end of each iteration to save memory

# 1. Organize land cover layers
# - load in R just those layers that correspond to forests
# - Recode continuous values as categorical
# - recode NA values if necessary
# - merge the rasters together, excluding NAs

# 2. Join the vert community database
# - Extract coordinates of each site from vert community database
# - For these coordinates, extract forest category values


extract_forest_types <- function(sites_raw) {

  ## 1. Workspace prep ----------------
  
  library(tidyverse)
  library(raster)
  
  # In case there are repeat rows (as of 2020-01-19, there are many)
  
  sites_raw <- distinct(sites_raw)
  
  # Create a new object, as we'll want the raw input version later
  sites <- sites_raw  %>%
    # Only will be useful (and only will work) for sites for which we have coords
    filter(complete.cases(latitude) & complete.cases(longitude))
  
  ## Create dataframe to loop through
  raster_file_df <- data.frame(
    forest_type = c("dry_forest", "moist_lowland_forest", 
                    "mangrove_forest", # excluding mangrove for now
                    "swamp_forest", "moist_montane_forest"),
    file_name = c("iucn_habitatclassification_fraction_lvl2__105_Forest – Subtropical-tropical dry__ver002.tif",
                  "iucn_habitatclassification_fraction_lvl2__106_Forest – Subtropical-tropical moist lowland__ver002.tif",
                  "iucn_habitatclassification_fraction_lvl2__107_Forest – Subtropical-tropical mangrove vegetation__ver002.tif",
                  "iucn_habitatclassification_fraction_lvl2__108_Forest – Subtropical-tropical swamp__ver002.tif",
                  "iucn_habitatclassification_fraction_lvl2__109_Forest – Subtropical-tropical moist montane__ver002.tif")
  )
  
  ## 2. Data curation --------------

  # Create SpatialPoints objects of site coordinates
  sites_coords <- sites %>% 
    dplyr::select(longitude, latitude)
  
  CRS <- CRS("+proj=longlat +datum=WGS84") # add projection
  sites_coords <- SpatialPoints(sites_coords) # turn into sp object
  proj4string(sites_coords) <- CRS # Assign projection to sp object
  
  ## 2. Prep forest layers -------------
  
  for (i in 1:nrow(raster_file_df)) {
    
    ## ....Import raster into R ------------------
    
    cat("Reading in forest raster", i, "\n")
    jung2020v002_forest <- raster(paste0("data/remote_sensing/iucn_categories/jung_2020_v002/", 
                                         raster_file_df$file_name[i]))
    
    ## ....Extract site coordinates from forest raster ----------------
    
    if (i == 1) {
      sites <- sites %>% 
        mutate(forest_type_iucn = NA)
    }
    
    cat("Beginning extraction of forest raster", i, "\n")
    extract_time <- system.time({
      sites <- sites %>% 
        mutate(forest_type_iucn = raster::extract(jung2020v002_forest, sites_coords))
      # mutate(forest_type_iucn = ifelse(complete.cases(forest_type_iucn), 
      #                                  as.character(raster_file_df$forest_type[i]),
      #                                  forest_type_iucn))
    })
    cat("Extract time:", extract_time[3], "\n")
    
    ## ....Save raster values to distinct column -----------------------
    # Not the most elegant approach, but a memory-efficient approach: depending on
    # what iteration it is, save the contents of the raster to its own column.
    # Note: this means the number of forest categories is HARD-CODED. Code would
    # need to be edited if additional categories are added (if this synthesis 
    # stays in the tropics, then  I don't know what other IUCN categories would 
    # be added...but if includes temperate than certainly other forest categories
    # could be included)
    if (i == 1) {
      sites$dry_forest <- sites$forest_type_iucn
    }
    if (i == 2) {
      sites$moist_lowland_forest <- sites$forest_type_iucn
    }
    if (i == 3) {
      sites$mangrove_forest <- sites$forest_type_iucn
    }
    if (i == 4) {
      sites$swamp_forest <- sites$forest_type_iucn
    }
    if (i == 5) {
      sites$moist_montane_forest <- sites$forest_type_iucn
    }
    
    # Remove the raster object to save memory (this way only one raster object is
    # loaded at any given point)
    rm(jung2020v002_forest)
  }
  
  ## Gather forest category columns into one column, join back to sites data --------------
  sites_iucn_categories <- sites %>% 
    dplyr::select(-forest_type_iucn) %>% 
    # Pivot the data to long form, so that all forest categories are in one column,
    # and the corresponding values (n/1000) are in another column
    pivot_longer(cols = c(dry_forest, moist_lowland_forest, mangrove_forest, 
                          swamp_forest, moist_montane_forest),
                 names_to = "forest_type_iucn", values_to = "value") %>% 
    # Remove all the NA rows (for when a site had 0% of a type of forest)
    filter(complete.cases(value))
  
  sites_filter <- sites_iucn_categories %>% 
    # Group by the linking keys
    group_by(study_id, sites, latitude, longitude) %>% 
    # Only keep the forest category that was most represented at that site, 
    # determined by the row (of that set of linking keys) with the highest `value`
    summarize(value = max(value, na.rm = T))
    
  # Now join back to sites
  sites_iucn_categories <- sites_iucn_categories %>% 
    right_join(sites_filter) %>% 
    # But, if a site had <500 for its value (presumably, less than 50% for even
    # the most well-represented category), instead designate it as "mixed forest"
    mutate(forest_type_iucn = ifelse(value < 500, "mixed_forest", forest_type_iucn)) %>% 
    dplyr::select(-value)
  
  # sites_iucn_categories <- sites %>% 
  #   dplyr::select(-forest_type_iucn) %>% 
  #   # Paste together the contents of the category columns, so that if a single
  #   # row had >1 category designation this would be captured in a single row
  #   mutate(forest_type_iucn = paste(dry_forest, moist_lowland_forest, mangrove_forest, 
  #                                   swamp_forest, moist_montane_forest),
  #          sep = " ") %>% 
  #   mutate(forest_type_iucn = gsub("NA ", "", forest_type_iucn)) %>% 
  #   mutate(forest_type_iucn = gsub(" NA", "", forest_type_iucn))
  
  sites_out <- sites_raw %>% 
    left_join(sites_iucn_categories) %>% 
    distinct()
  
  # Return output data
  return(sites_out)
}
## RECYCLING BIN --------------

# ## ....Merge together layers -----------------
# 
# ## Join into a list
# 
# mosaic_time <- system.time({
#   rfiles <- list(jung2020v002_dry_forest, jung2020v002_moist_lowland_forest, jung2020v002_mangrove_forest,
#                  jung2020v002_swamp_forest, jung2020v002_moist_montane_forest)
#   
#   jung2020v002_trop_forest_mosaic <-  do.call(mosaic, c(rfiles, fun = mean))
# })
# mosaic_time
# 
# merge_time <- system.time({
#   jung2020v002_trop_forest_merge <- raster::merge(jung2020v002_dry_forest, jung2020v002_moist_lowland_forest, 
#                                                   jung2020v002_mangrove_forest, jung2020v002_swamp_forest, 
#                                                   jung2020v002_moist_montane_forest)
# })
# merge_time
# 
# ## Write out merged layer
# writeRaster(jung2020v002_trop_forest_mosaic)
# 
