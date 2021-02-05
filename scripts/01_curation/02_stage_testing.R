devtools::source_url("https://raw.githubusercontent.com/slamander/load_packages/main/prepare_libraries.R", sha1 = "609d531a319fcd855804de6bdcfe737cd6515332")

packages <- c("betareg",
              "tidyverse",
              "readr",
              "scales")

prepare_libraries(packages)

stages <- read_rds("data/stages.rds")

stages$stage_1$stage_1[stages$stage_1$stage_1 == 0] <- 1e-05
stages$stage_1$stage_1[stages$stage_1$stage_1 == 1] <- 0.99999
stages$stage_2$stage_2[stages$stage_2$stage_2 == 0] <- 1e-05
stages$stage_2$stage_2[stages$stage_2$stage_2 == 1] <- 0.99999


dat <- read_csv("data/stripped_data/final/data_joined.csv") %>%
  dplyr::filter(taxa != "All mammals") %>%
  dplyr::filter(taxa != "Primates") %>%
  dplyr::mutate(weight = spatial_rank + temporal_bredth_rank + temporal_resolution_rank,
                taxa = as.factor(taxa),
                taxa_order = factor(taxa, levels = c("Birds","Bats","Small mammals","Amphibians")),
                elevation = as.numeric(scale(elevation)),
                scaled_met = as.numeric(corrected_biodiversity_metric_value),
                link = as.factor(link), 
                method = as.factor(method), 
                bin_weight = bin_weight/mean(bin_weight),
                treatment = as.factor(treatment),
                continent = as.factor(continent),
                biodiversity_metric = as.factor(biodiversity_metric), 
                season = as.factor(season), 
                forest_type_iucn = as.factor(forest_type_iucn_new),  
                strata = as.numeric(mean_strata_height_p),
                canopy_height = as.numeric(scale(canopy_height)),
                latitude = as.numeric(latitude),
                longitude = as.numeric(longitude)) %>%
  dplyr::select(link, study_id.x, method, taxa, bin_weight, weight, continent, 
                biodiversity_metric, taxa_order, treatment, season, forest_type_iucn, 
                elevation, canopy_height, latitude, longitude, scaled_met, strata, correction_performed_on_data_in_table) %>%
  dplyr::select(-c(study_id.x)) %>%
  dplyr::mutate(forest_type_iucn = recode(forest_type_iucn, "dry forest" = "dry_forest"))

abund <- dat %>%
  dplyr::filter(biodiversity_metric == "abundance") %>%
  dplyr::filter(correction_performed_on_data_in_table %like% "intervals") %>%
  dplyr::select(link, strata, scaled_met, bin_weight) %>%
  dplyr::mutate(stage_3 = scaled_met) %>%
  dplyr::select(-c(scaled_met))

stages[["stage_3"]] <- abund

######### mods

### Stage 1

stage_1_links <- unique(stages[["stage_1"]]$link)

stage_1_mods <- data.frame(
  link = stage_1_links,
  lower = rep(NA, length(stage_1_links)),
  upper = rep(NA, length(stage_1_links))
)

stage_1_mods_data <- data.frame()

confint <- data.frame()
error <- data.frame()

for(i in 1:length(stage_1_links)){
  
  stage_1_mods_data <- stages[["stage_1"]] %>%
    dplyr::filter(link == stage_1_links[i])
  
  confint <- betareg(stage_1 ~ strata, data = stage_1_mods_data) %>%
    confint()
  
  stage_1_mods[i, 2:3] <- confint[2,1:2]
  
}

betareg(stage_1 ~ strata, data = stage_1_mods_data) %>% summary()

### Stage 2

stage_2_links <- unique(stages[["stage_2"]]$link)

stage_2_mods <- data.frame(
  link = stage_2_links,
  lower = rep(NA, length(stage_2_links)),
  upper = rep(NA, length(stage_2_links))
)

stage_2_mods_data <- data.frame()

confint <- data.frame()

for(i in 1:length(stage_2_links)){
  
  stage_2_mods_data <- stages[["stage_2"]] %>%
    dplyr::filter(link == stage_2_links[i])
  
  confint <- betareg(stage_2 ~ strata, data = stage_2_mods_data) %>%
    confint()
  
  stage_2_mods[i, 2:3] <- confint[2,1:2]
  
}

### Stage 3

stage_3_links <- unique(stages[["stage_3"]]$link)

stage_3_mods <- data.frame(
  link = stage_3_links,
  lower = rep(NA, length(stage_3_links)),
  upper = rep(NA, length(stage_3_links))
)

stage_3_mods_data <- data.frame()

confint <- data.frame()

for(i in 1:length(stage_3_links)){
  
  stage_3_mods_data <- stages[["stage_3"]] %>%
    dplyr::filter(link == stage_3_links[i])
  
  confint <- betareg(stage_3 ~ strata, data = stage_3_mods_data) %>%
    confint()
  
  stage_3_mods[i, 2:3] <- confint[2,1:2]
  
}

### Stage 4

stage_4_links <- stage_3_links

stage_4_mods <- data.frame(
  link = stage_4_links,
  lower = rep(NA, length(stage_4_links)),
  upper = rep(NA, length(stage_4_links))
)

stage_4_mods_data <- data.frame()

confint <- data.frame()

for(i in 1:length(stage_4_links)){
  
  stage_4_mods_data <- stages[["stage_3"]] %>%
    dplyr::filter(link == stage_3_links[i])
  
  confint <- betareg(stage_3 ~ strata, data = stage_4_mods_data, weights = bin_weight) %>%
    confint()
  
  stage_4_mods[i, 2:3] <- confint[2,1:2]
  
}

stages_coef <- data.frame(
  rbind(stage_1_mods,
        stage_2_mods,
        stage_3_mods,
        stage_4_mods),
  stage = rep(c("1",
                "2",
                "3",
                "4"), 
              each = nrow(stage_1_mods))) %>%
  mutate(link_plot = link %>% 
           str_replace("_"," ") %>% 
           str_replace(" _ ",": ") %>%
           str_replace(": 1","") %>%
           str_replace(": 2"," (2)") %>%
           str_replace(": 3"," (3)") %>%
           str_replace(": 4"," (4)") %>%
           str_replace(": 5"," (5)") %>%
           str_replace(": 6"," (6)") %>%
           str_replace(": 7"," (7)") %>%
           str_replace(": 8"," (8)") %>%
           str_replace(": 9"," (9)"))

ggplot(data = stages_coef) + 
  ylab("Slope Coefficient") + xlab("Study Unit") + coord_flip() +
  geom_hline(yintercept = 0, alpha = 0.5, size = 0.8) +
  geom_errorbar(aes(x = link_plot, ymin = lower, ymax = upper, color = stage), 
                position = position_dodge2(width = 0.5, reverse = T), width = 0.5, size = 0.5) + 
  scale_color_viridis_d("Data Stages") + theme_classic() + 
  theme(legend.position = "bottom")
ggsave("analysis/figures/model results/coef_stages.jpeg", width = 6, height = 8, units = "in", dpi = 300)


