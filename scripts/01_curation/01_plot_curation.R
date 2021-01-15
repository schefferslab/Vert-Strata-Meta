## David Klinges
## This script curates plot data, notably:
## - estimating canopy height from Simard et al 2011 imagery when can height not provided

## 1. Workspace prep ------------

## ....Load libraries ------------

library(tidyverse)
library(readxl)
library(googlesheets4)

## ....Load in data -----------

## If googlesheets4 is installed....
if("googlesheets4" %in% rownames(installed.packages()) == TRUE) {
  big_data_plots_url <- "https://docs.google.com/spreadsheets/d/1-kY3Ono0ypzgahjbH8YyaZwFqB6zTwXkiQ96zOPixCw/edit?usp=sharing"
  plots <- read_sheet(big_data_plots_url, sheet = "2020 Raw Data", na = c(""," ","NA"))[-1,]  # removing the second row, which appears to be an explanation of the data(?)...
} else {
  plots <- read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Raw Data",
                      skip = 1)
  colnames(plots) <- colnames(read_excel("data/stripped_data/original/Big data.xlsx", 
                                         sheet = "2020 Raw Data"))
}

plots <- plots %>%
  mutate(`Biodiversity metric value` = sapply(`Biodiversity metric value`, toString)) %>%
  mutate(`Var Biodiversity metric` = sapply(`Var Biodiversity metric`, toString)) %>%
  mutate(`Var Biodiversity metric min`  = sapply(`Var Biodiversity metric min`, toString)) %>%
  mutate(`Var Biodiversity metric max`  = sapply(`Var Biodiversity metric max`, toString)) %>%
  mutate(`Proportion of max biodiversity`  = sapply(`Proportion of max biodiversity`, toString)) %>%
  mutate(`Min Strata height` = sapply(`Min Strata height`, toString)) %>%
  mutate(`Max Strata height` = sapply(`Max Strata height`, toString)) %>%
  mutate(`Mean Strata height` = sapply(`Mean Strata height`, toString)) %>%
  mutate(`P value` = sapply(`P value`, toString)) %>%
  mutate(`Temporal effort at each height` = sapply(`Temporal effort at each height`, toString)) %>%
  mutate(`Spatial effort` = sapply(`Spatial effort`, toString))
plots[plots == ""] <- NA

## 2. Data curation -------------
## Rename cols
colnames(plots) <- tolower(colnames(plots))
colnames(plots) <- gsub("/", "_", colnames(plots))
colnames(plots) <- gsub(" ", "_", colnames(plots))

plots <- plots %>% 
  dplyr::mutate(min_strata_height = as.double(min_strata_height),
         max_strata_height = as.double(max_strata_height)) %>% 
  dplyr::mutate(mean_strata_height = (min_strata_height + max_strata_height) / 2)

## 3. Write out data --------------

write_csv(plots, "data/stripped_data/intermediate/intermediate_plots.csv")
  