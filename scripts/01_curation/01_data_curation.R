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


## A. Filter down data according to study bounds ------------

## B. Corrections to biodiversity metric values --------------
## ....Gather abundance data into 1-m bins -----------

site_plot_binned <- site_plot_merge %>% 
  mutate(min_strata_height = as.double(min_strata_height), 
         max_strata_height = as.double(max_strata_height)) %>% 
  mutate(strata_width = max_strata_height - min_strata_height) %>% 
  # In case strata_width is 0, change to 1
  mutate(strata_width = ifelse(strata_width < 1, 1, strata_width)) %>% 
  mutate(biodiversity_metric_value = ifelse(biodiversity_metric == "abundance" & 
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

ints <- site_plot_binned[site_plot_binned$correction_performed_on_data_in_table %like% "intervals" & site_plot_binned$strata_width != 1, ]
ints$seq <- seq(1:nrow(ints))

dups <- ints$strata_width
idx <- rep(1:nrow(ints), dups)

# Use that index to genderate your new data frame of repeated rows
dupdf <- ints[idx,]

#each row is a duplicate and there are duplicates equal to the strata width - thus, the minimum and maxiumum height need to change to relect each 1 m interval

data <- dupdf %>%
  dplyr::group_by(link, min_strata_height) %>%
  dplyr::mutate(new_min_strata_height = ifelse(row_number()==1, min_strata_height, # If first row in group, keep min_strata_height as the same
                                        min_strata_height + row_number() - 1))  %>% # otherwise add the row_number to the min_height
           mutate(new_max_strata_height = max_strata_height - n() + row_number()) %>% # Subtract number of rows in the group, add the row number
  dplyr::ungroup() %>%
  dplyr::mutate(min_strata_height = new_min_strata_height, max_strata_height = new_max_strata_height) %>%
  dplyr::select(-new_min_strata_height, -new_max_strata_height, -seq)

site_plot_data_join <- rbind(site_plot_binned, data)
site_plot_data_join$mean_strata_height <- (site_plot_data_join$min_strata_height + site_plot_data_join$max_strata_height) /2


## If biodiver metric corrected == N but subsequent correction is not NA, 
## use subsequent correcion as corrected abundance

site_plot_joined <- site_plot_data_join %>% 
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
site_plot_corrected <- site_plot_joined %>% 
  filter(complete.cases(corrected_biodiversity_metric_value))

## ....Calculate biodiversity as proportion of max biodiversity ---------

site_max_richness <- site_plot_corrected %>% 
  filter(biodiversity_metric == "richness") %>% 
  group_by(study_id, sites) %>% 
  dplyr::summarise(max_richness = max(corrected_biodiversity_metric_value, na.rm = TRUE))
  
site_max_abundance <- site_plot_corrected %>% 
  filter(biodiversity_metric == "abundance") %>% 
  group_by(study_id, sites) %>% 
  dplyr::summarise(max_abundance = max(corrected_biodiversity_metric_value, na.rm = TRUE))

rich_abund_join <- site_plot_corrected %>% 
  left_join(site_max_richness) %>% 
  left_join(site_max_abundance) %>% 
  mutate(corrected_biodiversity_metric_value = 
           # If it's a richness metric, and therefore we identified the strata with
           # highest richness...
           ifelse(biodiversity_metric == "richness" & complete.cases(max_richness),
                  # make corrected biodiverse value a proportion of this max richness
                    corrected_biodiversity_metric_value / max_richness,
                  # Otherwise keep as-is
                  corrected_biodiversity_metric_value)) %>% 
  # Do the same thing for abundance
  mutate(corrected_biodiversity_metric_value =
           # If it's a richness metric, and therefore we identied the strata with
           # highest richness...
           ifelse(biodiversity_metric == "abundance" & complete.cases(max_abundance),
                  # make corrected biodiverse value a proportion of this max richness
                    corrected_biodiversity_metric_value / max_abundance,
                  # Otherwise keep as-is
                  corrected_biodiversity_metric_value)) 

## E. Corrections to canopy heights ------------

## ....Adjust canopy and strata height metrics -------------

data_joined_raw <- rich_abund_join %>%
  # If highest max_strata_height value is greater than canopy_height value,     < we got rid of this
  # then replace canopy_height value with max_strata_height
  # mutate(canopy_height = ifelse(max_strata_height > canopy_height,
  #                               max_strata_height, canopy_height)) %>%
  # Convert heights to proportions of max forest height
  mutate(min_strata_height = as.double(min_strata_height) / canopy_height) %>%
  mutate(max_strata_height = as.double(max_strata_height) / canopy_height) %>%
  mutate(mean_strata_height_p = as.double(mean_strata_height) / canopy_height)

data_joined_raw %>% 
  filter(mean_strata_height_p > 1)
  #view()

data_joined <- data_joined_raw %>%
  filter(!mean_strata_height_p > 1)

## 3. QA/QC -------------

if (nrow(foo <- filter(data_joined, canopy_height > 70)) > 0) {
  cat("Maximum canopy height is above 70 meters? Investigate: ", unique(foo$study_id))
  stop("\nSee above error")
  }

# if (nrow(foo <- filter(data_joined, canopy_height > 30)) > 0) {
#   cat("Maximum canopy height is above 70 meters? Investigate: ", unique(foo$study_id))
#   stop("\nSee above error")
# }


###########   Averaging Derlindati_2005


derlin <- data_joined[data_joined$link %like% "Derlindati", ]
derlin$link

derlin$corrected_biodiversity_metric_value[1] = (derlin$corrected_biodiversity_metric_value[1] + derlin$corrected_biodiversity_metric_value[5]) / 2
derlin$corrected_biodiversity_metric_value[2] = (derlin$corrected_biodiversity_metric_value[2] + derlin$corrected_biodiversity_metric_value[8]) / 2
derlin$corrected_biodiversity_metric_value[3] = (derlin$corrected_biodiversity_metric_value[3] + derlin$corrected_biodiversity_metric_value[7]) / 2
derlin$corrected_biodiversity_metric_value[4] = (derlin$corrected_biodiversity_metric_value[4] + derlin$corrected_biodiversity_metric_value[6]) / 2

derlin = derlin[1:4,]
derlin$method = "Mist nets + Human Observation"

derlin$link = "Derlindati_2005"
derlin$linking_id = 1
derlin$sites = 1
data_joined = data_joined %>%
  filter(study_id.x != "Derlindati_2005")

data_joined = rbind(data_joined, derlin)

## scaling biodiversity metrics to remove 0's and 1's 
data_joined$corrected_biodiversity_metric_value[data_joined$corrected_biodiversity_metric_value == 0] <- 1e-05
data_joined$corrected_biodiversity_metric_value[data_joined$corrected_biodiversity_metric_value == 1] <- 0.99999


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

ggplot(data_joined, aes(mean_strata_height_p, corrected_biodiversity_metric_value)) +
  geom_point() +
  facet_wrap(~biodiversity_metric)

## 4. Write out data ---------------------

write_csv(plots, "data/stripped_data/analysis/plots.csv")

write_csv(sites, "data/stripped_data/analysis/sites.csv")

write_csv(data_joined, "data/stripped_data/analysis/data_joined.csv")

