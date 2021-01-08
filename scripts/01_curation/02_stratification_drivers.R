## David Klinges
## This script curates drivers data - i.e. splitting up the categories and joining into a new dataframe for plotting


## 1. Workspace prep ------------

## ....Load libraries ------------

library(tidyverse)
library(readxl)


## ....Load in data -----------
# the first row (column names) first time through....
drivers <- read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Drivers",
                    skip = 1)
# ....then overwrite the colnames here
colnames(drivers) <- colnames(read_excel("data/stripped_data/original/Big data.xlsx", sheet = "2020 Drivers"))

## 2. Data curation ------------
# remove the first few unneeded columns - the drive link 
drivers = drivers[,-c(3:5)]


## ....A. Column renaming -------------

colnames(drivers) <- tolower(colnames(drivers))
colnames(drivers) <- gsub(" ", "_", colnames(drivers))
colnames(drivers)

## ....B. Variable recoding ------------

## selecting 2 colum chunks to represent each factor - then adding a column to each one which will be the name of that factor

climate = drivers[,c(2,3:4)] ; climate = cbind(climate,factor=c("climate")) ; climate = as.data.frame(climate)
climate <- climate %>% rename(theorised= climate , investigated = investigated...7)

food = drivers[,c(2,5:6)] ; food = cbind(food,factor=c("food")) ; food = as.data.frame(food)
food <- food %>% rename(theorised= "food/foraging" , investigated = investigated...9)

structure = drivers[,c(2,7:8)] ; structure = cbind(structure,factor=c("structure")) ; structure = as.data.frame(structure)
structure <- structure %>% rename(theorised= "vegetation_struture_(foliage_density)" , investigated = investigated...11)

microhabitat = drivers[,c(2,9:10)] ; microhabitat = cbind(microhabitat,factor=c("microhabitat")) ; microhabitat = as.data.frame(microhabitat)
microhabitat <- microhabitat %>% rename(theorised= "microhabitats" , investigated = investigated...13)

sex = drivers[,c(2,11:12)] ; sex = cbind(sex,factor=c("sex")) ; sex = as.data.frame(sex)
sex <- sex %>% rename(theorised= "sex" , investigated = investigated...15)

guilds = drivers[,c(2,13:14)] ; guilds = cbind(guilds,factor=c("guilds")) ; guilds = as.data.frame(guilds)
guilds <- guilds %>% rename(theorised= "guilds/phylogenetic_constraints" , investigated = investigated...17)

seasonality = drivers[,c(2,15:16)] ; seasonality = cbind(seasonality,factor=c("seasonality")) ; seasonality = as.data.frame(seasonality)
seasonality <- seasonality %>% rename(theorised= "seasonality" , investigated = investigated...19)

diurnality = drivers[,c(2,17:18)] ; diurnality = cbind(diurnality,factor=c("diurnality")) ; diurnality = as.data.frame(diurnality)
diurnality <- diurnality %>% rename(theorised= "diurnality" , investigated = investigated...21)

predation = drivers[,c(2,19:20)] ; predation = cbind(predation,factor=c("predation")) ; predation = as.data.frame(predation)
predation <- predation %>% rename(theorised= "predation_avoidance" , investigated = investigated...23)

light = drivers[,c(2,21:22)] ; light = cbind(light,factor=c("light")) ; light = as.data.frame(light)
light <- light %>% rename(theorised= "light" , investigated = investigated...25)

detection_bias = drivers[,c(2,23:24)] ; detection_bias = cbind(detection_bias,factor=c("detection_bias")) ; detection_bias = as.data.frame(detection_bias)
detection_bias <- detection_bias %>% rename(theorised= "detection_bias" , investigated = investigated...27)

reproduction = drivers[,c(2,25:26)] ; reproduction = cbind(reproduction,factor=c("reproduction")) ; reproduction = as.data.frame(reproduction)
reproduction <- reproduction %>% rename(theorised= "reproductive_activity" , investigated = investigated...29)

age = drivers[,c(2,27:28)] ; age = cbind(age,factor=c("age")) ; age = as.data.frame(age)
age <- age %>% rename(theorised= "age" , investigated = investigated...31)

morphology = drivers[,c(2,29:30)] ; morphology = cbind(morphology,factor=c("morphology")) ; morphology = as.data.frame(morphology)
morphology <- morphology %>% rename(theorised= "morphology/body_size" , investigated = investigated...33)

competition = drivers[,c(2,31:32)] ; competition = cbind(competition,factor=c("competition")) ; competition = as.data.frame(competition)
competition <- competition %>% rename(theorised= "competition" , investigated = investigated...35)

shelter = drivers[,c(2,33:34)] ; shelter = cbind(shelter,factor=c("shelter")) ; shelter = as.data.frame(shelter)
shelter <- shelter %>% rename(theorised= "shelter_(roosting,_nesting,_sleeping)" , investigated = investigated...37)

coexistance = drivers[,c(2,35:36)] ; coexistance = cbind(coexistance,factor=c("coexistance")) ; coexistance = as.data.frame(coexistance)
coexistance <- coexistance %>% rename(theorised= "niche_packing/partitioning/coexistence" , investigated = tested...39)

species_interactions = drivers[,c(2, 37:38)] ; species_interactions = cbind(species_interactions,factor=c("species_interactions")) ; species_interactions = as.data.frame(species_interactions)
species_interactions <- species_interactions %>% rename(theorised= "species_interactions_(combine_predation/avoidance,_competition,_territory,_singing,__mixed_flocks)" , investigated = tested...41)



#####
#######    selecting all factors
#####
all_factors = rbind(climate, light, food, structure,microhabitat, competition, predation, reproduction, shelter,species_interactions, sex, morphology, age, seasonality, diurnality, coexistance, guilds)

### merge with correct list of studies that are used in the analysis 


dat <- read_csv("data/stripped_data/final/data_joined.csv") %>%
  dplyr::filter(taxa != "All mammals") %>%
  dplyr::filter(taxa != "Primates") %>%
  dplyr::mutate(weight = spatial_rank + temporal_bredth_rank + temporal_resolution_rank,
                taxa = as.factor(taxa),
                taxa_order = factor(taxa, levels = c("Birds","Bats","Small mammals","Amphibians")),
                elevation = as.numeric(elevation),
                scaled_met = as.numeric(rescale(corrected_biodiversity_metric_value, to = c(0.00001, 0.99999))),
                link = as.factor(link), 
                method = as.factor(method), 
                treatment = as.factor(treatment),
                continent = as.factor(continent),
                biodiversity_metric = as.factor(biodiversity_metric), 
                season = as.factor(season), 
                forest_type = as.factor(forest_type),  
                mean_strata_height_p = as.numeric(mean_strata_height_p)) %>%
  dplyr::select(link, study_id.x, method, taxa, continent, biodiversity_metric, taxa_order,
                treatment, season, forest_type, elevation, canopy_height, latitude, longitude, 
                scaled_met, strata = mean_strata_height_p) 

#     unique(study.id)   from data including abundance and richness

studies = unique(dat[,c("study_id.x", "taxa")])
studies = studies %>% rename(study_id= study_id.x) ; studies = as.data.frame(studies)

#merge them across so there is a full dataset with what study with what taxa looked at what factors

all_factors_taxa = merge(all_factors, studies, by = "study_id")

## 3. Write out files ------------


#write_csv(drivers, "data/stripped_data/final/drivers.csv")
