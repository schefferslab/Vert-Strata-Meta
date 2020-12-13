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

## A. Check to confirm that the right sheets were imported -------------

if (!"Person Data Stripped" %in% colnames(papers)) {
  stop("Papers sheet was not imported in properly.")
}

if (!"forest_type" %in% colnames(sites)) {
  stop("Sites sheet was not imported in properly.")
}

if (!"min_strata_height" %in% colnames(plots)) {
  stop("Raw data sheet was not imported in properly.")
}
  
## B. Join data --------------

data_joined <- sites %>% 
  full_join(plots)

## C. Filter down data according to study bounds ------------

data_joined <- data_joined %>% 
  # Has a data quality flag
  filter(!is.na(data_quality), 
         # Is in the tropics or sub-tropics
         latitude >= -30 & latitude <= 30, 
         # Only undisturbed forest
         treatment %in% c("Unlogged", "Old regrowth", "Unlogged and secondary", "Unlogged (with some degraded forest)", "100 ha fragments",
                          "10 ha fragments", "1 ha fragments", "Fragment"),
         # Birds or mammals
         taxa %in% c("Birds", "Small mammals", "Bats"))

## D. Corrections to biodiversity metric values --------------
## ....Gather abundance data into 1-m bins -----------

# Do this for just point counts and human observation
# Have different rows for each 1-meter bin. The abundance of those rows
# will be the total divided by the bin width
data_joined <- data_joined %>% 
  mutate(min_strata_height = as.double(min_strata_height), 
         max_strata_height = as.double(max_strata_height)) %>% 
  mutate(strata_width = max_strata_height - min_strata_height) %>% 
  # In case strata_width is 0, change to 1
  mutate(strata_width = ifelse(strata_width < 1, 1, strata_width)) %>% 
  mutate(biodiversity_metric_value = ifelse(biodiversity_metric == "abundance" & 
                                              correction_performed_on_data_in_table %in% 
                                              c("needs 1m intervals", "needs 1m height intervals"),
                                            biodiversity_metric_value / strata_width, 
                                            biodiversity_metric_value))

## ....Filter to just previously corrected, or use values corrected on Google Sheet --------

## If biodiver metric corrected == N but subsequent correction is not NA, 
## use subsequent correcion as corrected abundance

data_joined <- data_joined %>% 
  mutate(corrected_biodiversity_metric_value = 
           # If metric was correcteed by study authors...
           ifelse(biodiversity_metric_corrected == "Y" | 
                    # OR does not need to be corrected....
                    grepl("equal|well sampled|species accumulation is saturated|very similar survey effort",
                          correction_performed),
                  # Carry forward the biodiversit_metric_value
                  biodiversity_metric_value,
                  # If correction was performed on the Google Sheet...
                  ifelse(!is.na(subsequent_correction),
                         # Carry that forward
                         subsequent_correction,
                         # Otherwise, set corrected_biodiversity_metric_value to NA
                         NA)))
# Now keep just rows with corrected values
data_joined <- data_joined %>% 
  filter(complete.cases(corrected_biodiversity_metric_value))

## ....Calculate biodiversity as proportion of max biodiversity ---------

site_max_richness <- data_joined %>% 
  filter(biodiversity_metric == "richness") %>% 
  group_by(study_id, sites) %>% 
  summarize(max_richness = max(corrected_biodiversity_metric_value, na.rm = TRUE))
  
site_max_abundance = data_joined %>% 
  filter(biodiversity_metric == "abundance") %>% 
  group_by(study_id, sites) %>% 
  summarize(max_abundance = max(corrected_biodiversity_metric_value, na.rm = TRUE))

data_joined <- data_joined %>% 
  left_join(site_max_richness) %>% 
  left_join(site_max_abundance) %>% 
  mutate(corrected_biodiversity_metric_value = 
           # If it's a richness metric, and therefore we identied the strata with
           # highest richness...
           ifelse(biodiversity_metric == "richness" & complete.cases(max_richness),
                  # make corrected biodiverse value a proportion of this max richness
                    corrected_biodiversity_metric_value / max_richness * 100,
                  # Otherwise keep as-is
                  corrected_biodiversity_metric_value)) %>% 
  # Do the same thing for abundance
  mutate(corrected_biodiversity_metric_value = 
           # If it's a richness metric, and therefore we identied the strata with
           # highest richness...
           ifelse(biodiversity_metric == "abundance" & complete.cases(max_abundance),
                  # make corrected biodiverse value a proportion of this max richness
                    corrected_biodiversity_metric_value / max_abundance * 100,
                  # Otherwise keep as-is
                  corrected_biodiversity_metric_value))
  
## E. Corrections to canopy heights ------------

## ....Adjust canopy and strata height metrics -------------

data_joined <- data_joined %>% 
  # If highest max_strata_height value is greater than canopy_height value, 
  # then replace canopy_height value with max_strata_height
  mutate(canopy_height = ifelse(max_strata_height > canopy_height, 
                                max_strata_height, canopy_height)) %>% 
  # Convert heights to proportions of max forest height
  mutate(min_strata_height = as.double(min_strata_height) / canopy_height) %>% 
  mutate(max_strata_height = as.double(max_strata_height) / canopy_height) %>% 
  mutate(mean_strata_height = as.double(mean_strata_height) / canopy_height)

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

ggplot(data_joined, aes(corrected_biodiversity_metric_value)) +
  geom_histogram() +
  facet_wrap(~biodiversity_metric)

ggplot(filter(data_joined, biodiversity_metric == "abundance"), 
       aes(corrected_biodiversity_metric_value)) +
  geom_histogram() +
  facet_wrap(~biodiversity_metric)

ggplot(data_joined, aes(mean_strata_height, corrected_biodiversity_metric_value)) +
  geom_point() +
  facet_wrap(~biodiversity_metric)

## 4. Write out data ---------------------

write_csv(plots, "data/stripped_data/final/final_plots.csv")

write_csv(sites, "data/stripped_data/final/final_sites.csv")

write_csv(data_joined, "data/stripped_data/final/data_joined.csv")
