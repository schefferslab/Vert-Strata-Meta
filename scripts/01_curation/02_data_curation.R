## David Klinges
## This script is a first draft of QA/QC of vert strat meta data at all levels

## 1. Workspace prep ---------------------

## ....Load packages -------------
library(tidyverse)
library(readxl)

## ....Load in data ---------------

papers <- read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Papers")
sites <- suppressMessages(read_csv(Sys.glob("data/*/*/intermediate_sites.csv")))
plots <- suppressMessages(read_csv(Sys.glob("data/*/*/intermediate_plots.csv")))

## 2. Data curation -------------

## ....A. Check to confirm  that the right sheets were imported -------------

if (!"Person Data Stripped" %in% colnames(papers)) {
  stop("Papers sheet was not imported in properly.")
}

if (!"forest_type" %in% colnames(sites)) {
  stop("Sites sheet was not imported in properly.")
}

if (!"min_strata_height" %in% colnames(plots)) {
  stop("Raw data sheet was not imported in properly.")
}
  
## ....B. Join data --------------

data_joined <- sites %>% 
  full_join(plots)

## ....C. Filter down data --------------

data_joined <- data_joined %>% 
  filter(taxa %in% c("Small mammals", "Birds", "Small mammals/marsupials", "Bats",
                     "Primates")) %>% 
  filter(treatment %in% c("Unlogged", "NA", "Old-regrowth", "Unlogged (with some degraded forset)",
                          "Old regrowth"))
# Filter by season?


## ....D. Manipulation --------------

data_joined <- data_joined %>% 
  # Convert heights to proportions of max forest height
  mutate(min_strata_height = as.double(min_strata_height) / canopy_height) %>% 
  mutate(max_strata_height = as.double(max_strata_height) / canopy_height) %>% 
  mutate(mean_strata_height = as.double(mean_strata_height) / canopy_height)

## Determine total abundance/richness at the plot level, for standardizing
## abundance/richness at the strata level (i.e. strata richness / total richness)
# plots <- data_joined %>% 
#   group_by(linking_id, study_id) %>% 
#   summarize(total_biodiversity_metric = sum(biodiversity_metric_value, na.rm = TRUE))

## ....Recode categorical vars -------------
## 3. QA/QC -------------

if (nrow(foo <- filter(data_joined, canopy_height > 70)) > 0) {
  cat("Maximum canopy height is above 70 meters? Investigate: ", unique(foo$study_id))
  stop("\nSee above error")
  }

# if (nrow(foo <- filter(data_joined, canopy_height > 30)) > 0) {
#   cat("Maximum canopy height is above 70 meters? Investigate: ", unique(foo$study_id))
#   stop("\nSee above error")
# }


## ....E. Unjoin for writing out ---------------

plots <- data_joined %>% 
  dplyr::select(colnames(plots))

sites <- data_joined %>% 
  dplyr::select(colnames(sites))

## 3. Quick viz ---------------

ggplot(data_joined, aes(canopy_height)) +
  geom_histogram()

## 4. Write out data ---------------------

write_csv(plots, "data/stripped_data/final/final_plots.csv")

write_csv(sites, "data/stripped_data/final/final_sites.csv")

write_csv(data_joined, "data/stripped_data/final/data_joined.csv")
