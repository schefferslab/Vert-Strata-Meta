## David Klinges
## This script curates drivers data - i.e. splitting up the categories and joining into a new dataframe for plotting


## 1. Workspace prep ------------

## ....Load libraries ------------

library(tidyverse)
library(readxl)
library(raster)

## ....Load in data -----------
# the first row (column names) first time through....
sites <- read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Site Info",
                    skip = 1)
# ....then overwrite the colnames here
colnames(sites) <- colnames(read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Site Info"))

## ....Load in and merge Simard raster --------------

## BUT only if we haven't done so already
if (!file.exists("data/remote_sensing/canopy_height/global_coverage/global_canopy_height.tif")) {
  tree_height_NW <- raster("data/remote_sensing/canopy_height/original/sdat_10023_1_20190902_191339657.asc")
  tree_height_NE <- raster("data/remote_sensing/canopy_height/original/sdat_10023_1_20190902_191159982.asc")
  tree_height_SE <- raster("data/remote_sensing/canopy_height/original/sdat_10023_1_20190902_191423576.asc")
  tree_height_SW <- raster("data/remote_sensing/canopy_height/original/sdat_10023_1_20190902_191431012.asc")
  
  ## Curate Simard raster: merge --------------
  
  # Overwrite default NA value in tree heights (which were set to -Inf upon import)
  NAvalue(tree_height_NW) <- -Inf
  NAvalue(tree_height_NE) <- -Inf
  NAvalue(tree_height_SE) <- -Inf
  NAvalue(tree_height_SW) <- -Inf
  
  time_iterated <- system.time({
    tree_height_north <- merge(tree_height_NW, tree_height_NE)
    tree_height_south <- merge(tree_height_SW, tree_height_SE)
    global_canopy_height <- merge(tree_height_north, tree_height_south)
  })
  time_iterated
} else {
  global_canopy_height <- raster("data/remote_sensing/canopy_height/global_coverage/global_canopy_height.tif")
}

## 2. Data curation ------------

## ....A. Column renaming -------------

colnames(sites) <- tolower(colnames(sites))
colnames(sites) <- gsub(" ", "_", colnames(sites))


## ....B. Variable recoding ------------

sites <- sites %>% 
  mutate(treatment = dplyr::recode(treatment, "Old-regrowth" = "Old regrowth"))
# Extract out sites without canopy heights

sites <- sites %>% 
  # filter(is.na(canopy_height)| canopy_height == "Unknown") %>% 
  mutate(longitude = as.double(longitude),
         latitude = as.double(latitude)) %>% 
  
  mutate(canopy_height = as.double(canopy_height)) %>% 
  ## Fill in NA canopy heights with Simard et al 2011 estimates
  # no_heights <- no_heights %>% 
  mutate(canopy_height = ifelse(is.na(canopy_height), raster::extract(global_canopy_height, 
                                                                      data.frame(x = longitude, y = latitude), 
                                                                      method = 'simple'),
                                canopy_height))

## 3. Write out files ------------

if (exists("global_canopy_height") & !file.exists("data/remote_sensing/canopy_height/global_coverage/global_canopy_height.tif")) {
  writeRaster(global_canopy_height, "data/remote_sensing/canopy_height/global_coverage/global_canopy_height",
              format = "GTiff")
}

write_csv(sites, "data/stripped_data/intermediate/intermediate_sites.csv")
