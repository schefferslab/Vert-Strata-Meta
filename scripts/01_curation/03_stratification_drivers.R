## Ed Basham, J. Alex Baecher, David Klinges
## This script curates drivers data - i.e. splitting up the categories and joining into a new dataframe for plotting


## 1. Workspace prep ------------

## ....Load libraries ------------

library(tidyverse)
library(readxl)
library(scales)

## ....Load in data -----------
# the first row (column names) first time through....

driver_names <- c("structure", "food", "morphology", "interactions", "climate", "shelter", "seasonality", "light", "diurnality", "age", "sex")

drivers <- read_csv("data/stripped_data/curation/drivers.csv") %>%
  pivot_longer(-c(sites, study_ID),
    names_to = c("type", ".value"),
    names_sep = "_") %>%
  filter(type %in% driver_names) %>%
  mutate(type = recode(type, 
                       "structure" = "Habitat Structure", 
                       "food" = "Food / Foraging", 
                       "morphology" = "Morphology", 
                       "interactions" = "Species Interactions", 
                       "climate" = "Climate", 
                       "shelter" = "Nesting / Roosting", 
                       "seasonality" = "Seasonality", 
                       "light" = "Light", 
                       "diurnality" = "Diurnality",  
                       "age" = "Age", 
                       "sex" = "Sex")) %>%
  mutate(type = factor(type, levels = c(
    "Habitat Structure", "Food / Foraging", "Climate", "Morphology", "Species Interactions", "Seasonality", "Nesting / Roosting","Light", "Diurnality", "Age", "Sex")))

drivers %>%
  group_by(type) %>%
  summarize()

### merge with correct list of studies that are used in the analysis 

dat <- read_csv("data/stripped_data/analysis/data_joined.csv") %>%
  dplyr::mutate(taxa = as.factor(taxa)) %>%
  dplyr::select(
    study_id.x, 
    taxa) %>%
  rename(study_ID = "study_id.x") %>%
  distinct()

taxa_studies <- dat %>%
  group_by(taxa) %>%
  summarize(studies = n())

drivers_studies <- left_join(drivers, dat) %>%
  filter(!is.na(taxa)) %>%
  distinct()

drivers_summary <- drivers_studies %>%
  group_by(type, taxa) %>%
  summarize(theorized = sum(theorized %in% c("Y", "YY")),
            investigated = sum(investigated %in% c("Y", "YY"))) %>%
  left_join(taxa_studies) %>%
  mutate(perc_theorized = (theorized / studies)*100,
         perc_investigated = (investigated / studies)*100) 

drivers_summary_long <- drivers_summary %>%
  select(-c(theorized, investigated, studies)) %>%
  rename(theorized = "perc_theorized",
         investigated = "perc_investigated",
         driver = "type") %>%
  pivot_longer(cols = c(theorized, investigated),
               values_to = "percent",
               names_to = "study_type")

f_labels <- data.frame(taxa = c("Amphibians", "Bats", "Birds", "Small mammals"), label = c("n = 4", "n = 22", "n = 19", "n = 17"))

ggplot(drivers_summary_long) + 
  geom_col(aes(x = driver, y = percent, fill = taxa, alpha = study_type), position = position_dodge(width = -0.8), col = "grey40") +
  geom_text(data = f_labels, x = 10, y = 90, aes(label = label, fill = NULL, size = 1), show.legend = FALSE) +
  facet_wrap(~taxa, ncol = 1) +
  ylab("Percent of Papers") + xlab("Stratification Factors") +
  scale_fill_viridis_d(" ", direction = -1) +
  scale_alpha_discrete(" ", labels = c("Investigated", "Referenced"), range = rev(c(0.6, 1)), guide = guide_legend(reverse=T)) +
  facet_wrap(~ factor(taxa, levels = c("Bats", "Birds", "Small mammals", "Amphibians")), ncol = 1) + 
  guides(fill = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.title.x = element_text(vjust= -4, size =  rel(1.5)),
        axis.title.y = element_text(vjust = 5, size =  rel(1.5)),
        strip.text.x = element_text(size = 11, face = "bold"),
        axis.text.y=element_text(colour="black", size = 11),
        legend.text=element_text(colour="black", size = 11, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"))
ggsave("figures/stratification_drivers.jpeg", dpi = 600)