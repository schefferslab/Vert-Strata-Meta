## David Klinges 
## This script is a 89th draft of QA/QC of vert strat meta data at all levels

## 1. Workspace prep ---------------------

## ....Load packages -------------

library(tidyverse)
library(data.table)

## ....Load in data ---------------

sites <- read_csv("data/stripped_data/curation/sites.csv") 
plots <- read_csv("data/stripped_data/curation/plots.csv")

site_plot_merge <- merge(sites, plots, by = "link")
site_plot_merge$study_id <- site_plot_merge$study_id.x

## C. Filter down data according to study bounds ------------

site_plot_filter <- site_plot_merge %>% 
  # Has a data quality flag
  filter(!is.na(spatial_rank), 
         # Is in the tropics or sub-tropics
         latitude >= -30 & latitude <= 30, 
         # Only undisturbed forest
         treatment %in% c("Unlogged", "Old regrowth", "Unlogged and secondary", "Unlogged (with some degraded forest)", "100 ha fragments", "Multiple regrowth stages",
                          "10 ha fragments", "1 ha fragments", "Fragment"),
         # Birds or mammals
         taxa %in% c("Birds", "Small mammals", "Bats", "Amphibians", "Primates", "All mammals"))

## D. Corrections to biodiversity metric values --------------
## ....Gather abundance data into 1-m bins -----------

site_plot_binned <- site_plot_filter %>% 
  mutate(min_strata_height = as.double(min_strata_height), 
         max_strata_height = as.double(max_strata_height)) %>% 
  mutate(strata_width = max_strata_height - min_strata_height) %>% 
  # In case strata_width is 0, change to 1
  mutate(strata_width = ifelse(strata_width < 1, 1, strata_width)) %>% 
  mutate(biodiversity_metric_value_divided = ifelse(biodiversity_metric == "abundance" & 
                                                      correction_performed_on_data_in_table %in% 
                                                      c("needs 1m intervals", "needs 1m height intervals"),
                                                    biodiversity_metric_value / strata_width, 
                                                    biodiversity_metric_value)) %>% 
  # Add weight according to whether, and by how much, the stratum was broken up
  # into bins
  mutate(bin_weight = ifelse(biodiversity_metric == "abundance" & 
                               correction_performed_on_data_in_table %in% 
                               c("needs 1m intervals", "needs 1m height intervals"),
                             1 / strata_width, 1))


## ....Filter to just previously corrected, or use values corrected on Google Sheet --------

site_plot_binned <- site_plot_binned[site_plot_binned$correction_performed_on_data_in_table %like% "intervals", ]

## If biodiver metric corrected == N but subsequent correction is not NA, 
## use subsequent correcion as corrected abundance


## ....Calculate biodiversity as proportion of max biodiversity ---------


site_max_abundance_normal <- site_plot_binned %>% 
  filter(biodiversity_metric == "abundance") %>% 
  group_by(study_id, sites) %>% 
  dplyr::summarise(max_abundance_normal = max(biodiversity_metric_value, na.rm = TRUE))

site_max_abundance_divided <- site_plot_binned %>% 
  filter(biodiversity_metric == "abundance") %>% 
  group_by(study_id, sites) %>% 
  dplyr::summarise(max_abundance_divided = max(biodiversity_metric_value_divided, na.rm = TRUE))

abund_join <- site_plot_binned %>% 
  left_join(site_max_abundance_normal) %>% 
  left_join(site_max_abundance_divided) %>% 
  # Do the same thing for abundance
  mutate(corrected_biodiversity_metric_divided_value = 
           # If it's a richness metric, and therefore we identied the strata with
           # highest richness...
           ifelse(biodiversity_metric == "abundance" & complete.cases(max_abundance_divided),
                  # make corrected biodiverse value a proportion of this max richness
                  biodiversity_metric_value_divided / max_abundance_divided,
                  # Otherwise keep as-is
                  biodiversity_metric_value_divided),
         corrected_biodiversity_metric_value = 
           # If it's a richness metric, and therefore we identied the strata with
           # highest richness...
           ifelse(biodiversity_metric == "abundance" & complete.cases(max_abundance_normal),
                  # make corrected biodiverse value a proportion of this max richness
                  biodiversity_metric_value / max_abundance_normal,
                  # Otherwise keep as-is
                  biodiversity_metric_value))



## E. Corrections to canopy heights ------------

## ....Adjust canopy and strata height metrics -------------

data_joined_raw <- abund_join %>%
  # If highest max_strata_height value is greater than canopy_height value,     < we got rid of this
  # then replace canopy_height value with max_strata_height
  # mutate(canopy_height = ifelse(max_strata_height > canopy_height,
  #                               max_strata_height, canopy_height)) %>%
  # Convert heights to proportions of max forest height
  mutate(min_strata_height = as.double(min_strata_height) / canopy_height) %>%
  mutate(max_strata_height = as.double(max_strata_height) / canopy_height) %>%
  mutate(mean_strata_height_p = as.double(mean_strata_height) / canopy_height)

data_joined_raw_sel <- data_joined_raw  %>% 
  select(link, biodiversity_metric_value, corrected_biodiversity_metric_value,  corrected_biodiversity_metric_divided_value, mean_strata_height_p) %>%
  rename(stage_1 = corrected_biodiversity_metric_value) %>%
  rename(stage_2 = corrected_biodiversity_metric_divided_value) %>%
  rename(strata = mean_strata_height_p)

stages_data <- list()
stages_data[["stage_1"]] <- data_joined_raw_sel %>%
  select(link, stage_1, strata)
stages_data[["stage_2"]] <- data_joined_raw_sel %>%
  select(link, stage_2, strata)

write_rds(stages_data, "data/stages.rds")

