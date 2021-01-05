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

coexistence = drivers[,c(2,35:36)] ; coexistence = cbind(coexistence,factor=c("coexistence")) ; coexistence = as.data.frame(coexistence)
coexistence <- coexistence %>% rename(theorised= "niche_packing/partitioning/coexistence" , investigated = tested...39)

species_interactions = drivers[,c(2, 37:38)] ; species_interactions = cbind(species_interactions,factor=c("species_interactions")) ; species_interactions = as.data.frame(species_interactions)
species_interactions <- species_interactions %>% rename(theorised= "species_interactions_(combine_predation/avoidance,_competition,_territory,_singing,__mixed_flocks)" , investigated = tested...41)



#####
#######    selecting all factors
#####
all_factors = rbind(climate, light, food, structure,microhabitat, competition, predation, reproduction, shelter,species_interactions, sex, morphology, age, seasonality, diurnality, coexistence, guilds)

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

# here we are just only looking at which ones theorised - so select only the Y for theorised
grouping = all_factors_taxa %>% filter(theorised == "Y") %>% count(factor, taxa, sort = TRUE)
# here we are just looking at the ones which are investigated so we remove the NAs and the X's 
grouping_investigated = all_factors_taxa %>% filter(!is.na(investigated) , investigated != "X" ) %>% count(factor, taxa, sort = TRUE)      ####  this could be chaned to YX or YY for specifics as to if they did or didnt find a relationship

grouping_totals = all_factors_taxa %>% count(factor, taxa, sort = TRUE)


### create a link variable to merge them together

grouping$link = paste(grouping$factor, grouping$taxa) ;  grouping = grouping %>% rename(n.theorised= n)
grouping_investigated$link = paste(grouping_investigated$factor, grouping_investigated$taxa) ; grouping_investigated = grouping_investigated %>% rename(n.investigated= n)
grouping_totals$link = paste(grouping_totals$factor, grouping_totals$taxa)

double_groups = merge(grouping, grouping_totals, by = "link", all = TRUE)
double_groups = merge(double_groups, grouping_investigated, by = "link", all = TRUE)

# selecting the right variables and renaming where necessary
triple_groups = double_groups %>% dplyr::select(factor.y, taxa.y, n.theorised, n.investigated, n) %>%
  rename(factors = factor.y, taxa = taxa.y, n.total = n)
#turn the NA's into 0's (they are true 0's)
triple_groups[is.na(triple_groups)] <- 0

#calculating the n's as percentages of the total studies for that taxa
triple_groups$theorised_percent = (triple_groups$n.theorised / triple_groups$n.total) * 100
triple_groups$investigated_percent = (triple_groups$n.investigated / triple_groups$n.total) * 100




## 3. make some plots

###   good place to select only the factors you want

# climate, light, food, structure,microhabitat, competition, predation,
#reproduction, shelter,species_interactions, sex, morphology, age, seasonality, diurnality, coexistence, guilds)


specifics = triple_groups %>% filter(factors %in% c("food", "structure","climate", "morphology", "shelter", "species_interactions", "sex","age",  "seasonality", "diurnality")
                                     #,taxa != "Amphibians"
                                     )

specifics_tall <- specifics %>% gather(key = Level, value = Value, theorised_percent:investigated_percent)

#create a list which sums the values and orders them to get a nice list of which factors should go in which order - this will still work if you choose different factors above
list_not_in_order = aggregate(specifics_tall$Value, by=list(specifics_tall$factors), FUN=sum)
list_in_order = list_not_in_order[order(-list_not_in_order$x),]

specifics_tall <- within(specifics_tall, 
                   factors <- factor(factors, levels=list_in_order$Group.1))

ggplot(specifics_tall, aes(x = factors, y = Value, fill = Level)) + 
  geom_col(position = "identity") +
  facet_wrap(~taxa, ncol = 1) +
  ylab("Percent of Papers") + xlab("Stratification Factors") +
  scale_fill_discrete(name = "Level", labels = c("Investigated", "Referenced"), guide = guide_legend(reverse=TRUE)) +
  ######  scale x discrete labels will need to be changed if the plot changes is updated!    or it will show the wrong labels because it is overriting thrm
  scale_x_discrete(labels=c("Habitat Structure", "Food / Foraging", "Climate", "Seasonality", "Species Interactions", "Morphology", "Nesting / Roosting", "Age", "Diurnality", "Sex")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("analysis/figures/drivers.jpeg", width = 3.5, height = 10, units = "in", dpi = 300)

## 3. Write out files ------------


#write_csv(drivers, "data/stripped_data/final/drivers.csv")
