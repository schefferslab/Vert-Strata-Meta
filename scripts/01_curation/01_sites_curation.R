## David Klinges
## This script curates site data, notably:
## - estimating canopy height from Simard et al 2011 imagery when can height not provided


## 1. Workspace prep ------------

## ....Load libraries ------------

library(tidyverse)
library(googlesheets4)
library(readxl)
library(raster)

## ....Load in data -----------

## If googlesheets4 is installed....
if("googlesheets4" %in% rownames(installed.packages()) == TRUE) {
  big_data_sites_url <- "https://docs.google.com/spreadsheets/d/1-kY3Ono0ypzgahjbH8YyaZwFqB6zTwXkiQ96zOPixCw/edit?usp=sharing"
  sites <- try(read_sheet(big_data_sites_url, sheet = "2020 Site Info"))
}

if ("googlesheets4" %in% rownames(installed.packages()) == FALSE | any(class(sites) == "try-error")){
  # the first row (column names) first time through....
  sites <- read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Site Info",
                      skip = 1)
  # ....then overwrite the colnames here
  colnames(sites) <- colnames(read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Site Info"))
}

sites <- sites %>%
  mutate(Year = sapply(Year, toString)) %>%
  mutate(`Data Quality`  = sapply(`Data Quality`, toString)) %>%
  mutate(`Data Quality explanation`  = sapply(`Data Quality explanation`, toString)) %>%
  mutate(`Sum Total Richness` = sapply(`Sum Total Richness`, toString)) %>%
  mutate(`Sum Total Abundance`  = sapply(`Sum Total Abundance`, toString)) %>%
  mutate(`Canopy height` = sapply(`Canopy height`, toString)) %>%
  mutate(Latitude = sapply(Latitude, toString)) %>%
  mutate(Longitude = sapply(Longitude, toString)) %>%
  mutate(`Study start date` = sapply(`Study start date`, toString)) %>%
  mutate(`Study end date` = sapply(`Study end date`, toString))
sites[sites == ""] <- NA
sites[sites == " "] <- NA
sites[sites == "NA"] <- NA

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

sites_view <- sites %>% dplyr::select(study_id, canopy_height)

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

## ....C. Extract IUCN fores types ----------

## Only perform this step if there are new coordinates added to the synthesis.
## Determine this by checking coords in `sites` compared to coords in `final_sites.csv`

input_coords <- dplyr::select(sites, latitude, longitude) %>% distinct()
output_coords <- dplyr::select(read_csv("data/stripped_data/intermediate/intermediate_sites.csv"),
                               latitude, longitude) %>% distinct()

if (any(!input_coords %in% output_coords)) {
  source("scripts/00_source/extract_forest_types.R")
  sites <- extract_forest_types(sites)
} else { # If no new coords, then just pull in the IUCN forest types from the
  # already-curated data
  output_forest_types <- dplyr::select(read_csv("data/stripped_data/intermediate/intermediate_sites.csv"),
                                       latitude, longitude, forest_type_iucn_new) %>% distinct()
  
  sites <- sites %>% 
    left_join(output_forest_types)
}


## 3. Write out files ------------

if (exists("global_canopy_height") & !file.exists("data/remote_sensing/canopy_height/global_coverage/global_canopy_height.tif")) {
  writeRaster(global_canopy_height, "data/remote_sensing/canopy_height/global_coverage/global_canopy_height",
              format = "GTiff")
}

write_csv(sites, "data/stripped_data/intermediate/intermediate_sites.csv")
