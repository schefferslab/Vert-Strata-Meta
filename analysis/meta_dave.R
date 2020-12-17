library(lme4)
library(AICcmodavg)
library(glmmTMB)
library(tidyverse)
library(readr)
library(scales)
library(sjPlot)
library(scales)
library(visreg)


dat <- read_csv("data/stripped_data/final/data_joined.csv") %>%
  dplyr::mutate(weight = spatial_rank + temporal_bredth_rank + temporal_resolution_rank,
         taxa = as.factor(taxa),
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
  dplyr::select(link, study_id.x, method, taxa, continent, biodiversity_metric,
         treatment, season, forest_type, elevation, canopy_height, latitude, longitude, 
         scaled_met, strata = mean_strata_height_p) 
glimpse(dat)

abund <- dat %>%
  subset(biodiversity_metric == "abundance")
rich <- dat %>% 
  subset(biodiversity_metric == "richness")
  
ggplot(abund, aes(x = strata, y = scaled_met, color = method))+
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~taxa)

ggplot(rich, aes(x = strata, y = scaled_met, color = method))+
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~taxa)

ggplot(abund, aes(x = strata, y = scaled_met, color = link))+
  geom_point() +
  #geom_line()+
  geom_smooth(method = "lm", alpha = 0.01) +
  facet_wrap(~taxa) +
  coord_cartesian(xlim=c(0,1), ylim= c(0,1)) +
  theme(legend.position = "none")+
  labs(title = "Abundance", x = "Strata Height (proportion of study specific max forest height)", y = "Abundance Metric (proportion of study specific max value)")

ggplot(rich, aes(x = strata, y = scaled_met, color = link))+
  geom_point()+
  #geom_line()+
  geom_smooth(method = "lm", alpha = 0.01)+
  facet_wrap(~taxa)+
  coord_cartesian(xlim=c(0,1), ylim= c(0,1)) +
  theme(legend.position = "none")+
  labs(title = "Richness", x = "Strata Height (proportion of study specific max forest height)", y = "Richness Metric (proportion of study specific max value)")

ggplot(abund, aes(x = strata, y = scaled_met, color = continent))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.01)+
  facet_wrap(~taxa)+
  coord_cartesian(xlim=c(0,1), ylim= c(0,1)) +
  labs(title = "Abundance", x = "Strata Height (proportion of study specific max forest height)", y = "Abundance Metric (proportion of study specific max value)")


ggplot(rich, aes(x = strata, y = scaled_met, color = continent))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.01)+
  facet_wrap(~taxa)+
  coord_cartesian(xlim=c(0,1), ylim= c(0,1)) +
  labs(title = "Richness", x = "Strata Height (proportion of study specific max forest height)", y = "Richness Metric (proportion of study specific max value)")


ggplot(rich, aes(x = strata, y = scaled_met))+
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(xlim=c(0,1), ylim= c(0,1)) +
  facet_wrap(~taxa)

ggplot(abund, aes(x = strata, y = scaled_met))+
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(xlim=c(0,1), ylim= c(0,1)) +
  facet_wrap(~taxa)


################################
#########    GLMS   ############    
################################

mods_rich <- list()
# strata only models 
mods_rich[[1]]  <- glmmTMB(scaled_met ~ 1 +                                                                   (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[2]]  <- glmmTMB(scaled_met ~ strata +                                                              (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[3]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) +                                                (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[4]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) +                                  (strata|link), family = beta_family(link = "logit"), data = rich)
# strata and taxa
mods_rich[[5]]  <- glmmTMB(scaled_met ~ strata + taxa +                                                       (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[6]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^2) + taxa +                           (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[7]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + taxa +                           (strata|link), family = beta_family(link = "logit"), data = rich)
# strata and continent 
mods_rich[[8]]  <- glmmTMB(scaled_met ~ strata + continent +                                                  (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[9]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + continent +                                    (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[10]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + continent +                      (strata|link), family = beta_family(link = "logit"), data = rich)
# strata and elevation
mods_rich[[11]] <- glmmTMB(scaled_met ~ strata + elevation +                                                  (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[12]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + elevation +                                    (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[13]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + elevation +                      (strata|link), family = beta_family(link = "logit"), data = rich)
# strata method
mods_rich[[14]] <- glmmTMB(scaled_met ~ strata + method +                                                     (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[15]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + method +                                       (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[16]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + method +                         (strata|link), family = beta_family(link = "logit"), data = rich)
# strata, elevation, and continent 
mods_rich[[17]] <- glmmTMB(scaled_met ~ strata + elevation + continent +                                      (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[18]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + elevation + continent +                        (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[19]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + elevation + continent +          (strata|link), family = beta_family(link = "logit"), data = rich)
# strata, method, and continent 
mods_rich[[20]] <- glmmTMB(scaled_met ~ strata + method + continent +                                         (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[21]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + method + continent +                           (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[22]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + method + continent +             (strata|link), family = beta_family(link = "logit"), data = rich)
# strata, method, and elevation 
mods_rich[[23]] <- glmmTMB(scaled_met ~ strata + method + continent + elevation +                             (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[24]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + method + continent + elevation +               (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[25]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + method + continent + elevation + (strata|link), family = beta_family(link = "logit"), data = rich)
# strata interactions with methods or continent or elevation 
mods_rich[[26]] <- glmmTMB(scaled_met ~ strata*method +                                                       (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[27]] <- glmmTMB(scaled_met ~ strata*continent +                                                    (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[28]] <- glmmTMB(scaled_met ~ strata*elevation +                                                    (strata|link), family = beta_family(link = "logit"), data = rich)
# strata interactions with methods or continent or elevation plus additive relationships continent and elevation and methods 
mods_rich[[29]] <- glmmTMB(scaled_met ~ strata*method + continent +                                           (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[30]] <- glmmTMB(scaled_met ~ strata*method + elevation +                                           (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[31]] <- glmmTMB(scaled_met ~ strata*continent + method +                                           (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[32]] <- glmmTMB(scaled_met ~ strata*continent + elevation +                                        (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[33]] <- glmmTMB(scaled_met ~ strata*elevation + method +                                           (strata|link), family = beta_family(link = "logit"), data = rich)
mods_rich[[34]] <- glmmTMB(scaled_met ~ strata*elevation + continent +                                        (strata|link), family = beta_family(link = "logit"), data = rich)

aictab(mods_rich)

plot_model(mods_rich[[5]], type = "re", transform = NULL)

model_estimates_rich <- get_model_data(mods_rich[[5]], type = "re", transform = NULL)

rich_predictions <- predict(mods_rich[[5]], se.fit = T, type = "response")

rich$predictions_fit <- rich_predictions$fit
rich$predictions_se.fit <- rich_predictions$se.fit
# rich$predictions_upper <- rich_predictions$fit + rich_predictions$se.fit
# rich$predictions_lower <- rich_predictions$fit - rich_predictions$se.fit

ggplot(rich, aes(y = predictions_fit, x = strata,
                 ymin = predictions_fit - predictions_se.fit*1.96, 
                 ymax = predictions_fit + predictions_se.fit*1.96,
                 color = link, fill = link)) + 
  geom_line(size = 1, alpha = 0.3) +
  geom_ribbon(color = NA, alpha = 0.3) +
  facet_wrap(~taxa, scales = "free") + theme_bw() + 
  theme(legend.position = "none") 
ggsave("figures/predictions_richness_verticality_se.jpeg", width = 8, height = 5, units = "in", dpi = 300)

ggplot(rich, aes(y = predictions_fit, x = strata,
                 ymin = predictions_lower, ymax = predictions_upper,
                 color = link, fill = link)) + 
  geom_line(size = 1, alpha = 0.5) +
  facet_wrap(~taxa, scales = "free") + theme_bw() + 
  theme(legend.position = "none")
ggsave("figures/predictions_richness_verticality.jpeg", width = 8, height = 5, units = "in", dpi = 300)

taxa_link_rich <- rich %>% 
  filter(taxa != "All mammals") %>%
  filter(taxa != "Primates") %>%
  group_by(taxa,link) %>%
  summarise() %>%
  left_join(model_estimates_rich, by = c("link" = "term"))

ggplot(taxa_link_rich, aes(x = reorder(link, estimate), y = estimate, col = reorder(taxa, taxa))) + 
  ylab("Effect Size") + xlab(" ") + coord_flip() + 
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high)) +
  facet_wrap(~facet) + scale_color_viridis_d("Taxa") + theme_bw()
ggsave("analysis/figures/predictions_richness_parameters_estimates_GH.jpeg", width = 8, height = 8, units = "in", dpi = 300)



################# abund 



mods_abund <- list()
# strata only models 
mods_abund[[1]]  <- glmmTMB(scaled_met ~ 1 +                                                                   (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[2]]  <- glmmTMB(scaled_met ~ strata +                                                              (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[3]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) +                                                (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[4]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) +                                  (strata|link), family = beta_family(link = "logit"), data = abund)
# strata and taxa
mods_abund[[5]]  <- glmmTMB(scaled_met ~ strata + taxa +                                                       (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[6]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^2) + taxa +                           (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[7]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + taxa +                           (strata|link), family = beta_family(link = "logit"), data = abund)
# strata and continent 
mods_abund[[8]]  <- glmmTMB(scaled_met ~ strata + continent +                                                  (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[9]]  <- glmmTMB(scaled_met ~ strata + I(strata^2) + continent +                                    (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[10]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + continent +                      (strata|link), family = beta_family(link = "logit"), data = abund)
# strata and elevation
mods_abund[[11]] <- glmmTMB(scaled_met ~ strata + elevation +                                                  (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[12]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + elevation +                                    (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[13]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + elevation +                      (strata|link), family = beta_family(link = "logit"), data = abund)
# strata method
mods_abund[[14]] <- glmmTMB(scaled_met ~ strata + method +                                                     (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[15]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + method +                                       (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[16]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + method +                         (strata|link), family = beta_family(link = "logit"), data = abund)
# strata, elevation, and continent 
mods_abund[[17]] <- glmmTMB(scaled_met ~ strata + elevation + continent +                                      (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[18]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + elevation + continent +                        (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[19]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + elevation + continent +          (strata|link), family = beta_family(link = "logit"), data = abund)
# strata, method, and continent 
mods_abund[[20]] <- glmmTMB(scaled_met ~ strata + method + continent +                                         (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[21]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + method + continent +                           (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[22]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + method + continent +             (strata|link), family = beta_family(link = "logit"), data = abund)
# strata, method, and elevation 
mods_abund[[23]] <- glmmTMB(scaled_met ~ strata + method + continent + elevation +                             (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[24]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + method + continent + elevation +               (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[25]] <- glmmTMB(scaled_met ~ strata + I(strata^2) + I(strata^3) + method + continent + elevation + (strata|link), family = beta_family(link = "logit"), data = abund)
# strata interactions with methods or continent or elevation 
mods_abund[[26]] <- glmmTMB(scaled_met ~ strata*method +                                                       (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[27]] <- glmmTMB(scaled_met ~ strata*continent +                                                    (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[28]] <- glmmTMB(scaled_met ~ strata*elevation +                                                    (strata|link), family = beta_family(link = "logit"), data = abund)
# strata interactions with methods or continent or elevation plus additive relationships continent and elevation and methods 
mods_abund[[29]] <- glmmTMB(scaled_met ~ strata*method + continent +                                           (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[30]] <- glmmTMB(scaled_met ~ strata*method + elevation +                                           (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[31]] <- glmmTMB(scaled_met ~ strata*continent + method +                                           (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[32]] <- glmmTMB(scaled_met ~ strata*continent + elevation +                                        (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[33]] <- glmmTMB(scaled_met ~ strata*elevation + method +                                           (strata|link), family = beta_family(link = "logit"), data = abund)
mods_abund[[34]] <- glmmTMB(scaled_met ~ strata*elevation + continent +                                        (strata|link), family = beta_family(link = "logit"), data = abund)

aictab(mods_abund)

plot_model(mods_abund[[16]], type = "re", transform = NULL)

model_estimates_abund <- get_model_data(mods_abund[[16]], type = "re", transform = NULL)

abund_predictions <- predict(mods_abund[[16]], se.fit = T, type = "response")

abund$predictions_fit <- abund_predictions$fit
abund$predictions_se.fit <- abund_predictions$se.fit
# abund$predictions_upper <- abund_predictions$fit + abund_predictions$se.fit
# abund$predictions_lower <- abund_predictions$fit - abund_predictions$se.fit

ggplot(abund, aes(y = predictions_fit, x = strata,
                 ymin = predictions_fit - predictions_se.fit*1.96, 
                 ymax = predictions_fit + predictions_se.fit*1.96,
                 color = link, fill = link)) + 
  geom_line(size = 1, alpha = 0.3) +
  geom_ribbon(color = NA, alpha = 0.3) +
  facet_wrap(~taxa, scales = "free") + theme_bw() + 
  theme(legend.position = "none") 
ggsave("figures/predictions_abundness_verticality_se.jpeg", width = 8, height = 5, units = "in", dpi = 300)


taxa_link_abund <- abund %>% 
  filter(taxa != "All mammals") %>%
  filter(taxa != "Primates") %>%
  group_by(taxa,link) %>%
  summarise() %>%
  left_join(model_estimates_abund, by = c("link" = "term"))

ggplot(taxa_link_abund, aes(x = reorder(link, estimate), y = estimate, col = reorder(taxa, taxa))) + 
  ylab("Effect Size") + xlab(" ") + coord_flip() + 
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low, 
                      ymax = conf.high)) +
  facet_wrap(~facet) + scale_color_viridis_d("Taxa") + theme_bw()
ggsave("analysis/figures/predictions_abundness_parameters_estimates_GH.jpeg", width = 8, height = 8, units = "in", dpi = 300)

