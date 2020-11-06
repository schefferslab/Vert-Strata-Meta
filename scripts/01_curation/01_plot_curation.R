## David Klinges
## This script curates plot data, notably:
## - estimating canopy height from Simard et al 2011 imagery when can height not provided

## 1. Workspace prep ------------

## ....Load libraries ------------

library(tidyverse)
library(readxl)

## ....Load in data -----------

plots <- read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Raw Data",
                    skip = 1)
# ....then overwrite the colnames here
colnames(plots) <- colnames(read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Raw Data"))


## 2. Data curation -------------

## Rename cols
colnames(plots) <- tolower(colnames(plots))
colnames(plots) <- gsub(" ", "_", colnames(plots))
colnames(plots) <- gsub("/", "_", colnames(plots))

plots <- plots


## 3. Write out data --------------

write_csv(plots, "data/stripped_data/intermediate/intermediate_plots.csv")
  