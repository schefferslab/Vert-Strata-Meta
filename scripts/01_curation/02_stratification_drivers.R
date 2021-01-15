## David Klinges
## This script curates drivers data - i.e. splitting up the categories and joining into a new dataframe for plotting


## 1. Workspace prep ------------

## ....Load libraries ------------

library(tidyverse)
library(readxl)
library(scales)

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
  #dplyr::filter(taxa != "Primates") %>%
  dplyr::mutate(weight = spatial_rank + temporal_bredth_rank + temporal_resolution_rank,
                taxa = as.factor(taxa),
                taxa_order = factor(taxa, levels = c("Birds","Bats","Small mammals","Amphibians", "Primates")),
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
  dplyr::select(link, study_id.x,year, method, taxa, continent,country, biodiversity_metric, taxa_order,
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

##   justify inclusions 
##

specifics = triple_groups %>% filter(factors %in% c("food", "structure","climate", "morphology", "shelter", "species_interactions", "sex","age",  "seasonality", "diurnality", "light")
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
  scale_fill_brewer(palette = "Set1", labels = c("Investigated", "Referenced"), guide = guide_legend(reverse=TRUE)) +
  ######  scale x discrete labels will need to be changed if the plot changes is updated!    or it will show the wrong labels because it is overriting thrm
  scale_x_discrete(labels=c("Habitat Structure", "Food / Foraging", "Morphology", "Species Interactions", "Climate",  "Nesting / Roosting",  "Seasonality","light", "Diurnality",   "Age",  "Sex")) +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(vjust= -4, size =  rel(1.5)),
        axis.title.y = element_text(vjust = 5, size =  rel(1.5)),
        strip.text.x = element_text(size = 11, face = "bold"),
        axis.text.y=element_text(colour="black", size = 11),
        legend.text=element_text(colour="black",size = 11, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"))

ggsave("analysis/figures/drivers_all_taxa.jpeg", width = 7, height = 10, units = "in", dpi = 300)



#
#
#
#     without primates 

specifics_select = triple_groups %>% filter(factors %in% c("food", "structure","climate", "morphology", "shelter", "species_interactions", "sex","age",  "seasonality", "diurnality", "light")
                                     ,taxa !="Primates"
)

specifics_tall_select <- specifics_select %>% gather(key = Level, value = Value, theorised_percent:investigated_percent)

#create a list which sums the values and orders them to get a nice list of which factors should go in which order - this will still work if you choose different factors above
list_not_in_order_select = aggregate(specifics_tall_select$Value, by=list(specifics_tall_select$factors), FUN=sum)
list_in_order_select = list_not_in_order_select[order(-list_not_in_order_select$x),]

specifics_tall_select <- within(specifics_tall_select, 
                         factors <- factor(factors, levels=list_in_order_select$Group.1))

ggplot(specifics_tall_select, aes(x = factors, y = Value, fill = Level)) + 
  geom_col(position = "identity") +
  facet_wrap(~taxa, ncol = 1) +
  ylab("Percent of Papers") + xlab("Stratification Factors") +
  scale_fill_brewer(palette = "Set1", labels = c("Investigated", "Referenced"), guide = guide_legend(reverse=TRUE)) +
  ######  scale x discrete labels will need to be changed if the plot changes is updated!    or it will show the wrong labels because it is overriting thrm
  scale_x_discrete(labels=c("Habitat Structure", "Food / Foraging","Climate", "Species Interactions","Morphology", "Seasonality", "Nesting / Roosting","light",  "Diurnality",  "Age", "Sex")) +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(colour="black", angle = 45, hjust = 1),
        axis.title.x = element_text(colour="black", vjust= -4, size =  rel(1.5)),
        axis.title.y = element_text(colour="black", vjust = 5, size =  rel(1.5)),
        strip.text.x = element_text(size = 11, face = "bold"),
        axis.text.y=element_text(colour="black", size = 11),
        legend.text=element_text(colour="black",size = 11, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"))

ggsave("analysis/figures/drivers_3_taxa.jpeg", width = 5, height = 10, units = "in", dpi = 300)

## 3. Draw a world map
###

# load in packages

library("sf")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
theme_set(theme_bw())

#### load in the world dataset 'the basic map'
world <- ne_countries(scale = "medium", returnclass = "sf")

# select out each individual study location - along with useful columns for plotting later

locations = dat[, c("latitude", "longitude", "taxa", "continent","country", "link", "method", "elevation", "year")]
locations = unique(locations)
locations_np = locations %>%
  filter(taxa != "Primates")
# count how many studies occured across different taxa and continents
continent_taxa = as.data.frame(locations) %>%
  group_by(continent, taxa) %>%
  tally()

#here making a primate based dataset map thing
# primates were not recorded in africa and asia so these rows need to get added and then re- ordered
continent_add = continent_taxa[c(4,13),]
continent_mix = rbind(continent_taxa, continent_add)
continent_mix[14:15,2] =  "Primates" ; continent_mix[14:15,3] =  0
continent_mix = continent_mix[
  with(continent_mix, order(continent, taxa)),
  ]

### here adding a new list of coordinates for the plotting of labels on the map - which match the continent and taxa of the first data
continent_mix$longitude = c(rep(-15, 5), rep(-115, 5), rep(140, 5))
continent_mix$latitude =  c(   0,-6,-12,-18,-24,   6, 0, -6, -12,-18,     24,18,12,6,0)
continent_mix$inset = paste("n = ", continent_mix$n)

##
## here making one without primates
continent_np = continent_taxa[-8,]
continent_np$longitude = c(rep(-15, 4), rep(-115, 4), rep(140, 4))
continent_np$latitude =  c(0,-6,-12,-18,   6, 0, -6, -12,    24, 18,12,6)
continent_np$inset = paste("n = ", continent_np$n)


### select the lists of countries in each continent
africa = c(unique(subset(locations_np, continent == "Africa")$country))
asia =   c(unique(subset(locations_np, continent == "Asia and Oceania")$country))
neo =    c(unique(subset(locations_np, continent == "Americas")$country))


chop_africa <- ne_countries(scale = "medium", returnclass = "sf", country = africa)
chop_asia <- ne_countries(scale = "medium", returnclass = "sf", country = asia)
chop_neo <- ne_countries(scale = "medium", returnclass = "sf", country = neo)
french_gui <- ne_countries(scale = "medium", returnclass = "sf", geounit = "french guiana", sovereignty = "France", country = "France", type= "map_units")


### plot the whole damned thing
ggplot(data = world) +
  geom_sf(fill = "grey94")+
  geom_sf(data = french_gui, fill = "yellowgreen") + 
  geom_sf(data = chop_africa, fill = "orangered1") + 
  geom_sf(data = chop_asia, fill = "deepskyblue1") + 
  geom_sf(data = chop_neo, fill = "yellowgreen") + 
  geom_point(data = locations_np, aes(x = longitude, y = latitude, fill = taxa), size = 2, shape = 21)+
  geom_point(data = continent_np, aes(x = longitude, y = latitude, fill = taxa), size = 2, shape = 23, show.legend = FALSE)+
  geom_text(data = continent_np,  aes(x = longitude, y = latitude, label = inset, fontface = "bold"), nudge_x = 5, hjust = "left") +
  geom_hline(yintercept = 30) +
  geom_hline(yintercept = -30) +
  coord_sf(ylim= c(60, -60), xlim= c(165, -140) ) +  # 
  xlab("") + 
  ylab("")+
  guides(fill=guide_legend(title=""))+
  theme(legend.position = "top", legend.text = element_text(size = 13, face = "bold"), 
        panel.grid.major = element_line(linetype = "dashed"))

ggsave("analysis/figures/world_map.jpeg", width = 11, height = 5, units = "in", dpi = 350)




###
###    plot basic plots of elevation distributions 
###

theme_set(theme_bw())

locations = locations %>%
  filter(taxa != "Primates")

elevs = ggplot(locations, aes(x = taxa, y = elevation, fill = taxa)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(width = 0.1, show.legend = FALSE) +
  xlab("Taxa") + 
  ylab("Elevation (m)")+
  theme(axis.title.x = element_text(vjust= -4, size =  rel(1.5)),
        axis.title.y = element_text(vjust = 5, size =  rel(1.5)),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(colour="black", vjust = 1, hjust = 1, size = 11, angle = 45),
        axis.text.y=element_text(colour="black", size = 11),
        axis.line = element_line(size=0.5, colour = "black"), 
        plot.margin = margin(1,1,1,1, "cm"))
elevs
ggsave("analysis/figures/elevation_taxa.jpeg", width = 5, height = 6.5, units = "in", dpi = 350)


###
###    plot basic plots of richness and abundance counts  
###

locations_split = dat[, c("latitude", "longitude", "taxa", "continent","country", "link", "method", "elevation", "biodiversity_metric")]
locations_split = unique(locations_split)

locations_split = locations_split %>%
  filter(taxa != "Primates")

# count how many studies occured across different taxa and continents
continent_taxa_metric = as.data.frame(locations_split) %>%
  group_by(taxa, biodiversity_metric) %>%
  tally()

t_m = ggplot(continent_taxa_metric, aes(x = taxa, y = n, fill = biodiversity_metric))+
  geom_col(position = "dodge") +
  xlab("Taxa") + 
  ylab("Number of Studies")+
  scale_y_continuous(limits = c(0,30))+
  scale_fill_brewer(palette = "Dark2", name = "", labels = c("Abundance", "Richness"))+
  theme(legend.position = "top", legend.title = element_blank(),
        axis.title.x = element_text(vjust= -4, size =  rel(1.5)),
        axis.title.y = element_text(vjust = 5, size =  rel(1.5)),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(colour="black", vjust = 1, hjust = 1, size = 11, angle = 45),
        axis.text.y=element_text(colour="black", size = 11),
        axis.line = element_line(size=0.5, colour = "black"), 
        legend.text=element_text(colour="black",size = 11, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"))
t_m
ggsave("analysis/figures/studies_metrics.jpeg", width = 5, height = 6.5, units = "in", dpi = 350)


###
###    plot basic plots of methods
###

# count how many studies occured across different taxa and continents
method_breakdown = as.data.frame(locations) %>%
  filter(taxa != "Primates") %>%
  group_by(method) %>%
  tally()

meth = ggplot(method_breakdown, aes(x = reorder(method,-n), y = n, fill = method))+
  geom_col(position = "dodge", show.legend = FALSE) +
  xlab("Method of Data Collection") + 
  ylab("Number of Studies")+
  scale_y_continuous(limits = c(0,40))+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.title.x = element_text(vjust= -4, size =  rel(1.5)),
        axis.title.y = element_text(vjust = 5, size =  rel(1.5)),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(colour="black", vjust = 1, hjust = 1, size = 11, angle = 45),
        axis.text.y=element_text(colour="black", size = 11),
        axis.line = element_line(size=0.5, colour = "black"), 
        plot.margin = margin(1,1,1,1, "cm"))
meth
ggsave("analysis/figures/studies_methods.jpeg", width = 5.5, height = 7.5, units = "in", dpi = 350)



###
###    plot stratification study over time
###

# extract the years from the study ID - wont need this now because I've put it into the excel sheet

#substrRight <- function(x, n){
#  substr(x, nchar(x)-n+1, nchar(x))
#}

#locations$year = as.numeric(substrRight(locations$study_id.x, 4))

# count how many studies occured across different taxa and continents
method_breakdown = as.data.frame(locations) %>%
  filter(taxa != "Primates") %>%
  group_by(method) %>%
  tally()

years = locations_np %>% mutate(decade = floor(year/10)*10) 
years$decade = as.factor(years$decade)

years_grouped = years %>% 
  group_by(decade, taxa) %>% 
  tally()
years_grouped$decade = as.numeric(as.character(years_grouped$decade))

years_grouped = as.data.frame(years_grouped)
years_grouped = na.omit(years_grouped)

taxa_n = years_grouped %>% 
  group_by( taxa) %>% 
  tally(n, sort = T)

ggplot(years_grouped, aes(x = decade, y = n, fill = factor(taxa, levels= c( "Primates", "Amphibians","Small mammals", "Bats", "Birds" )))) + 
  geom_col() +
  ylab("Number of Papers") + xlab("Decade") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(colour="black", angle = 45, hjust = 1),
        axis.title.x = element_text(colour="black", vjust= -4, size =  rel(1.5)),
        axis.title.y = element_text(colour="black", vjust = 5, size =  rel(1.5)),
        strip.text.x = element_text(size = 11, face = "bold"),
        axis.text.y=element_text(colour="black", size = 11),
        legend.text=element_text(colour="black",size = 11, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"))

ggsave("analysis/figures/studies_decades.jpeg", width = 7, height = 7.5, units = "in", dpi = 350)

