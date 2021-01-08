library(lme4)
library(AICcmodavg)
library(glmmTMB)
library(TMB)
library(tidyverse)
library(readr)
library(scales)
library(sjPlot)
library(emmeans)
library(ggpubr)
library(scales)
library(visreg)


dat <- read_csv("data/stripped_data/final/data_joined.csv") %>%
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
glimpse(dat)

abund <- dat %>%
  subset(biodiversity_metric == "abundance")
rich <- dat %>% 
  subset(biodiversity_metric == "richness")

################################
#######    Plotting   ########## 

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
  geom_smooth(alpha = 0.01)+
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


################################
######### Richness GLMs ########  

rich_cut <- rich %>% 
  filter(taxa != "All mammals") %>%
  filter(taxa != "Primates") 

mods_rich <- list()
# strata only models 
mods_rich[[1]]  <- glmmTMB(scaled_met ~ 1 +                                                (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[2]]  <- glmmTMB(scaled_met ~ strata +                                           (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[3]]  <- glmmTMB(scaled_met ~ poly(strata, 2) +                                  (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[4]]  <- glmmTMB(scaled_met ~ poly(strata, 3) +                                  (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata and taxa
mods_rich[[5]]  <- glmmTMB(scaled_met ~ strata + taxa +                                    (strata|link), family = beta_family(link = "logit"), data = rich_cut, se = T)
mods_rich[[6]]  <- glmmTMB(scaled_met ~ poly(strata, 2) + taxa +                           (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[7]]  <- glmmTMB(scaled_met ~ poly(strata, 3) + + taxa +                         (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata and continent 
mods_rich[[8]]  <- glmmTMB(scaled_met ~ strata + continent +                               (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[9]]  <- glmmTMB(scaled_met ~ poly(strata, 2) + continent +                      (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[10]] <- glmmTMB(scaled_met ~ poly(strata, 3) + continent +                      (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata and elevation
mods_rich[[11]] <- glmmTMB(scaled_met ~ strata + elevation +                               (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[12]] <- glmmTMB(scaled_met ~ poly(strata, 2) + elevation +                      (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[13]] <- glmmTMB(scaled_met ~ poly(strata, 3) + elevation +                      (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata method
mods_rich[[14]] <- glmmTMB(scaled_met ~ strata + method +                                  (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[15]] <- glmmTMB(scaled_met ~ poly(strata, 2) + method +                         (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[16]] <- glmmTMB(scaled_met ~ poly(strata, 3) + method +                         (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata, elevation, and continent 
mods_rich[[17]] <- glmmTMB(scaled_met ~ strata + elevation + continent +                   (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[18]] <- glmmTMB(scaled_met ~ poly(strata, 2) + elevation + continent +          (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[19]] <- glmmTMB(scaled_met ~ poly(strata, 3) + elevation + continent +          (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata, method, and continent 
mods_rich[[20]] <- glmmTMB(scaled_met ~ strata + method + continent +                      (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[21]] <- glmmTMB(scaled_met ~ poly(strata, 2) + method + continent +             (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[22]] <- glmmTMB(scaled_met ~ poly(strata, 3) + method + continent +             (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata, method, and elevation 
mods_rich[[23]] <- glmmTMB(scaled_met ~ strata + method + continent + elevation +          (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[24]] <- glmmTMB(scaled_met ~ poly(strata, 2) + method + continent + elevation + (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[25]] <- glmmTMB(scaled_met ~ poly(strata, 3) + method + continent + elevation + (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata interactions with methods or continent or elevation 
mods_rich[[26]] <- glmmTMB(scaled_met ~ strata*method +                                    (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[27]] <- glmmTMB(scaled_met ~ strata*continent +                                 (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[28]] <- glmmTMB(scaled_met ~ strata*elevation +                                 (strata|link), family = beta_family(link = "logit"), data = rich_cut)
# strata interactions with methods or continent or elevation plus additive relationships continent and elevation and methods 
mods_rich[[29]] <- glmmTMB(scaled_met ~ strata*method + continent +                        (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[30]] <- glmmTMB(scaled_met ~ strata*method + elevation +                        (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[31]] <- glmmTMB(scaled_met ~ strata*continent + method +                        (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[32]] <- glmmTMB(scaled_met ~ strata*continent + elevation +                     (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[33]] <- glmmTMB(scaled_met ~ strata*elevation + method +                        (strata|link), family = beta_family(link = "logit"), data = rich_cut)
mods_rich[[34]] <- glmmTMB(scaled_met ~ strata*elevation + continent +                     (strata|link), family = beta_family(link = "logit"), data = rich_cut)

aictab(mods_rich)

################################

plot_model(mods_rich[[5]], type = "re", transform = NULL)

rich_predictions <- predict(mods_rich[[5]], se.fit = T, type = "response")

rich_cut$predictions_fit <- rich_predictions$fit
rich_cut$predictions_se.fit <- rich_predictions$se.fit
rich_cut$predictions_95CI_lower <- rich_cut$predictions_fit - rich_cut$predictions_se.fit*1.96
rich_cut$predictions_95CI_upper <- rich_cut$predictions_fit + rich_cut$predictions_se.fit*1.96
rich_cut$predictions_95CI_lower[rich_cut$predictions_95CI_lower < 0] <- 0
rich_cut$predictions_95CI_upper[rich_cut$predictions_95CI_upper > 1] <- 1

{ggplot(rich_cut, aes(y = predictions_fit, x = strata,
                 ymin = predictions_95CI_lower, 
                 ymax = predictions_95CI_upper,
                 fill = taxa_order, group = link)) + 
  ylab("Predicted species richness") + xlab("Mean strata height measure") +
  geom_line(size = 1, alpha = 0.3) +
  geom_ribbon(color = NA, alpha = 0.2) +
  scale_fill_viridis_d("Taxa") +
  facet_grid(rows = vars(taxa), cols = vars(continent)) + theme_bw() + 
  theme(legend.position = "bottom")
ggsave("analysis/figures/richness_strata_full_continent.jpeg", width = 8, height = 6, units = "in", dpi = 300)}


{ggplot(rich_cut, aes(y = predictions_fit, x = strata,
                     ymin = predictions_95CI_lower, 
                     ymax = predictions_95CI_upper,
                     fill = taxa_order)) + 
  ylab("Predicted species richness") + xlab("Mean strata height measure") +
  geom_line(size = 1, alpha = 0.3) +
  geom_ribbon(color = NA, alpha = 0.3) +
  facet_wrap(~link + taxa) + theme_bw() + 
  scale_fill_viridis_d("Taxa") + theme_bw() +
  theme(legend.position = c(0.3,0.06), legend.box = "horizontal") 
ggsave("analysis/figures/richness_strata_full.jpeg", width = 18, height = 12, units = "in", dpi = 300)}



taxa_link_rich <- rich_cut %>% 
  group_by(taxa_order,link,continent) %>%
  summarise() %>%
  full_join(model_estimates_rich, by = c("link" = "term")) 

taxa_link_rich$facet[taxa_link_rich$facet == "link (Intercept)"] <- "Richness at lowest strata"
taxa_link_rich$facet[taxa_link_rich$facet == "strata"] <- "Change in richness with increasing strata"

taxa_link_rich_order <- taxa_link_rich %>%
  group_by(facet, factor(taxa_order, levels = c("Birds","Bats","Small mammals","Amphibians"))) %>%
  arrange(estimate, .by_group = T)

taxa_link_rich_order$order <- rep(NA, nrow(taxa_link_rich))
taxa_link_rich_order$order[taxa_link_rich_order$facet == "Richness at lowest strata"] <- 
  1:length(taxa_link_rich_order$order[taxa_link_rich_order$facet == "Richness at lowest strata"])
taxa_link_rich_order$order[taxa_link_rich_order$facet == "Change in richness with increasing strata"] <- 
  1:length(taxa_link_rich_order$order[taxa_link_rich_order$facet == "Change in richness with increasing strata"])
glimpse(taxa_link_rich_order)

taxa_link_rich_avg <- taxa_link_rich_order %>%
  group_by(taxa_order, facet) %>%
  summarize(estimate = mean(estimate),
          conf.high = mean(conf.high),
          conf.low = mean(conf.low))
taxa_link_rich_avg$order <- rep(NA, nrow(taxa_link_rich_avg))
taxa_link_rich_avg$order[taxa_link_rich_avg$facet == "Richness at lowest strata"] <- 
  1:length(taxa_link_rich_avg$order[taxa_link_rich_avg$facet == "Richness at lowest strata"])
taxa_link_rich_avg$order[taxa_link_rich_avg$facet == "Change in richness with increasing strata"] <- 
  1:length(taxa_link_rich_avg$order[taxa_link_rich_avg$facet == "Change in richness with increasing strata"])
glimpse(taxa_link_rich_avg)

## Two panel figure
col_2_est <- ggplot(taxa_link_rich_order, aes(x = rev(order), y = estimate, col = taxa_order)) + 
  ylab(" ") + xlab(" ") + coord_flip() + 
  geom_hline(yintercept = 0, col = "grey80", size = 1, linetype = 1) + geom_point(size = 1.5) + 
  geom_linerange(aes(ymin = estimate - ((estimate - conf.low)/1.96)*1.28, 
                     ymax = estimate + ((conf.high - estimate)/1.96)*1.28), size = 1.1) +
  geom_linerange(aes(ymin = conf.low, 
                     ymax = conf.high)) +
  scale_x_continuous(breaks = rev(taxa_link_rich_order$order[1:50]), labels = taxa_link_rich_order$link[1:50]) +
  facet_grid(cols = vars(factor(facet, levels = c("Richness at lowest strata",
                                                  "Change in richness with increasing strata")))) + 
  scale_color_viridis_d("Taxa") + theme_bw() + theme(legend.position = "none"); col_2_est


col_2_est_avg <- ggplot(taxa_link_rich_avg, aes(x = rev(taxa_order), y = estimate, col = taxa_order)) + 
  ylab("Estimate") + xlab(" ") + coord_flip() + 
  geom_hline(yintercept = 0, col = "grey80", size = 1, linetype = 1) + geom_point(size = 1.5) + 
  geom_linerange(aes(ymin = estimate - ((estimate - conf.low)/1.96)*1.28, 
                     ymax = estimate + ((conf.high - estimate)/1.96)*1.28), size = 1.1) +
  geom_linerange(aes(ymin = conf.low, 
                     ymax = conf.high)) +
  scale_x_discrete(labels = rev(c("Birds","Bats","             Small mammals","Amphibians"))) +
  facet_grid(cols = vars(factor(facet, levels = c("Richness at lowest strata",
                                                  "Change in richness with increasing strata")))) + 
  scale_color_viridis_d("Taxa") + theme_bw() + 
  theme(legend.position = "bottom"); col_2_est_avg

ggarrange(col_2_est, col_2_est_avg, ncol = 1, nrow = 2, heights = c(4,1), widths = c(4,1))
ggsave("analysis/figures/predictions_richness_parameters_estimates_taxa_2_panel.jpeg", width = 8, height = 10, units = "in", dpi = 300)


################################
######### Richness GLMs ########    

abund_cut =abund %>% 
  filter(taxa != "All mammals") %>%
  filter(taxa != "Primates") 
  
mods_abund <- list()
# strata only models 
mods_abund[[1]]  <- glmmTMB(scaled_met ~ 1 +                                                (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[2]]  <- glmmTMB(scaled_met ~ strata +                                           (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[3]]  <- glmmTMB(scaled_met ~ poly(strata, 2) +                                  (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[4]]  <- glmmTMB(scaled_met ~ poly(strata, 3) +                                  (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata and taxa
mods_abund[[5]]  <- glmmTMB(scaled_met ~ strata + taxa +                                    (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[6]]  <- glmmTMB(scaled_met ~ poly(strata, 2) + taxa +                           (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[7]]  <- glmmTMB(scaled_met ~ poly(strata, 3) + + taxa +                         (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata and continent 
mods_abund[[8]]  <- glmmTMB(scaled_met ~ strata + continent +                               (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[9]]  <- glmmTMB(scaled_met ~ poly(strata, 2) + continent +                      (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[10]] <- glmmTMB(scaled_met ~ poly(strata, 3) + continent +                      (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata and elevation
mods_abund[[11]] <- glmmTMB(scaled_met ~ strata + elevation +                               (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[12]] <- glmmTMB(scaled_met ~ poly(strata, 2) + elevation +                      (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[13]] <- glmmTMB(scaled_met ~ poly(strata, 3) + elevation +                      (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata method
mods_abund[[14]] <- glmmTMB(scaled_met ~ strata + method +                                  (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[15]] <- glmmTMB(scaled_met ~ poly(strata, 2) + method +                         (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[16]] <- glmmTMB(scaled_met ~ poly(strata, 3) + method +                         (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata, elevation, and continent 
mods_abund[[17]] <- glmmTMB(scaled_met ~ strata + elevation + continent +                   (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[18]] <- glmmTMB(scaled_met ~ poly(strata, 2) + elevation + continent +          (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[19]] <- glmmTMB(scaled_met ~ poly(strata, 3) + elevation + continent +          (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata, method, and continent 
mods_abund[[20]] <- glmmTMB(scaled_met ~ strata + method + continent +                      (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[21]] <- glmmTMB(scaled_met ~ poly(strata, 2) + method + continent +             (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[22]] <- glmmTMB(scaled_met ~ poly(strata, 3) + method + continent +             (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata, method, and elevation 
mods_abund[[23]] <- glmmTMB(scaled_met ~ strata + method + continent + elevation +          (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[24]] <- glmmTMB(scaled_met ~ poly(strata, 2) + method + continent + elevation + (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[25]] <- glmmTMB(scaled_met ~ poly(strata, 3) + method + continent + elevation + (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata interactions with methods or continent or elevation 
mods_abund[[26]] <- glmmTMB(scaled_met ~ strata*method +                                    (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[27]] <- glmmTMB(scaled_met ~ strata*continent +                                 (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[28]] <- glmmTMB(scaled_met ~ strata*elevation +                                 (strata|link), family = beta_family(link = "logit"), data = abund_cut)
# strata interactions with methods or continent or elevation plus additive relationships continent and elevation and methods 
mods_abund[[29]] <- glmmTMB(scaled_met ~ strata*method + continent +                        (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[30]] <- glmmTMB(scaled_met ~ strata*method + elevation +                        (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[31]] <- glmmTMB(scaled_met ~ strata*continent + method +                        (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[32]] <- glmmTMB(scaled_met ~ strata*continent + elevation +                     (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[33]] <- glmmTMB(scaled_met ~ strata*elevation + method +                        (strata|link), family = beta_family(link = "logit"), data = abund_cut)
mods_abund[[34]] <- glmmTMB(scaled_met ~ strata*elevation + continent +                     (strata|link), family = beta_family(link = "logit"), data = abund_cut)

aictab(mods_abund)

################################

plot_model(mods_abund[[16]], type = "re", transform = NULL)

model_estimates_abund <- get_model_data(mods_abund[[16]], type = "re", se = T, transform = NULL)

abund_predictions <- predict(mods_abund[[16]], se.fit = T, type = "response")

abund_cut$predictions_fit <- abund_predictions$fit
abund_cut$predictions_se.fit <- abund_predictions$se.fit
# abund$predictions_upper <- abund_predictions$fit + abund_predictions$se.fit
# abund$predictions_lower <- abund_predictions$fit - abund_predictions$se.fit


ggplot(abund_cut, aes(y = predictions_fit, x = strata,
                  ymin = predictions_fit - predictions_se.fit*1.96, 
                  ymax = predictions_fit + predictions_se.fit*1.96,
                  color = link, fill = link)) + 
  geom_line(size = 1, alpha = 0.3) +
  geom_ribbon(color = NA, alpha = 0.15) +
  facet_wrap(~taxa) + theme_bw() + 
  theme(legend.position = "none") 

ggsave("figures/predictions_abund_verticality_se.jpeg", width = 8, height = 5, units = "in", dpi = 300)

ggplot(abund_cut, aes(y = predictions_fit, x = strata,
                  ymin = predictions_fit - predictions_se.fit*1.96, 
                  ymax = predictions_fit + predictions_se.fit*1.96)) + 
  geom_smooth(size = 1, alpha = 0.3, method = "gam") +
  facet_wrap(~taxa) + theme_bw() + 
  theme(legend.position = "none") 

taxa_link_abund <- abund_cut %>% 
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



#
#
#    trying to get taxa specific lines
#


mods_overall <- list()

mods_overall[[1]]  <- glmmTMB(scaled_met ~ strata + taxa + biodiversity_metric, family = beta_family(link = "logit"), data = dat)



plot_model(mods_overall[[1]], type = "re", transform = NULL)
