## Path analysis
## standalone script

## I'm thinking of being liberal about transforming variables for the path analysis
## Then checking the direction of estimates against more generalized models.

library(tidyverse)
library(sjPlot)
library(patchwork)
library(MuMIn)
library(DHARMa)
library(lme4)
library(piecewiseSEM)

sem_data <- read_csv("data/path_analysis_data.csv")

## Ultimate terminal node is prevalence of infected ticks, which we believe is 
## determined primarily by tick density but may also be affected by host density.
## We have three ways to calculate prevalence of ticks infected with human pathogens:
## 1. Only human pathogens, excluding R. amblyommatis
## 2. Human pathogens, including R. amblyommatis
## 3. Human and animal pathogens, including R. amblyommatis
### While this would be ideal it doesn't play nicely overall with the current
### implementation of the path modeling, so it is now separated to:
### 'Rscripts/model_infected_tic_prevalence.R'

## We also have multiple groupings for relative host abundance
## 1. All host dung clusters
## 2. Deer and other except cottontail
## 3. Only deer

sem_data %>% 
  select(deer:total_clusters1m) %>% 
  colSums( )

sem_data <- sem_data %>%
  mutate(biomass_log = log(avg_dry_standing_gm2),
         d_since_fire_log = log(d_since_fire)
  )


## Tick abundance models ####
sem_data$tpt <- ceiling(sem_data$ticks_per_trap)

tick_mod_allHosts <- glmer.nb(tpt ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + total_clusters1m + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
  data = sem_data, 
  na.action = "na.fail",
  control=glmerControl(
    optimizer = "Nelder_Mead",
    optCtrl=list(maxfun=1e6)))
# summary(tick_mod_allHosts)
simulateResiduals(tick_mod_allHosts) %>% plot()
# summary(effectsize::standardize(tick_mod_allHosts))


tick_mod_allHosts_gamma <- glmer(ticks_per_trap01 ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + total_clusters1m + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
                         data = sem_data,
                         family = Gamma(link = "log"),
                         na.action = "na.fail",
                         control=glmerControl(
                           optimizer = "Nelder_Mead",
                           optCtrl=list(maxfun=1e6)))
# summary(tick_mod_allHosts_gamma)
# effectsize::standardize(tick_mod_allHosts_gamma) %>% summary()
# simulateResiduals(tick_mod_allHosts) %>% plot()
# simulateResiduals(tick_mod_allHosts_gamma) %>% plot()

tpt_pois <- glmer(tpt ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + total_clusters1m + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
      data = sem_data,
      family = poisson(link = "log"),
      na.action = "na.fail",
      control=glmerControl(
        optimizer = "Nelder_Mead",
        optCtrl=list(maxfun=1e6)))
# simulateResiduals(tpt_pois) %>% plot()
# summary(tpt_pois)
# effectsize::standardize(tpt_pois) %>% summary()
# plot_model(tpt_pois, show.values = T)
# plot_model(tpt_pois, type = "pred", show.data = T, grid = T)

tick_mod_deer <- glmer.nb(ticks_per_trap ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + deer + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
                            data = sem_data, 
                            na.action = "na.fail",
                            control=glmerControl(
                              optimizer = "Nelder_Mead",
                              optCtrl=list(maxfun=1e6)))
# summary(tick_mod_deer)
# effectsize::standardize(tick_mod_deer) %>% summary()
tick_mod_deer_gamma <- glmer(ticks_per_trap01 ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + deer + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
                            data = sem_data, 
                            family = Gamma(link = "log"),
                            na.action = "na.fail",
                            control=glmerControl(
                              optimizer = "Nelder_Mead",
                              optCtrl=list(maxfun=1e6)))
# summary(tick_mod_hosts_deer_gamma)
# simulateResiduals(tick_mod_deer_gamma) %>% plot()
# simulateResiduals(tick_mod_deer) %>% plot()


tick_mod_noRabbit <- glmer.nb(ticks_per_trap ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + deer_other + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
                            data = sem_data, 
                            na.action = "na.fail",
                            control=glmerControl(
                              optimizer = "Nelder_Mead",
                              optCtrl=list(maxfun=1e6)))
simulateResiduals(tick_mod_noRabbit) %>% plot()
effectsize::standardize(tick_mod_noRabbit) %>% 
  simulateResiduals() %>% 
  plot()
# summary(tick_mod_noRabbit)
# effectsize::standardize(tick_mod_noRabbit) %>% summary()

tick_mod_noRabbit_gamma <- glmer(ticks_per_trap01 ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + deer_other + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
                            data = sem_data, 
                            family = Gamma(link = "log"),
                            na.action = "na.fail",
                            control=glmerControl(
                              optimizer = "Nelder_Mead",
                              optCtrl=list(maxfun=1e6)))
# summary(tick_mod_noRabbit_gamma)
effectsize::standardize(tick_mod_noRabbit_gamma) %>% summary()
effectsize::standardize(tick_mod_noRabbit_gamma) %>% 
  simulateResiduals() %>% 
  plot()

tick_mod_noHosts <- glmer.nb(ticks_per_trap ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
                            data = sem_data, 
                            na.action = "na.fail",
                            control=glmerControl(
                              optimizer = "Nelder_Mead",
                              optCtrl=list(maxfun=1e6)))
# summary(tick_mod_noHosts)
effectsize::standardize(tick_mod_noHosts) %>% summary()

tick_mod_noHosts_gamma <- glmer(ticks_per_trap01 ~ d_since_fire_log + logit_litter + avg_canopy_cover + avg_litter_depth_all + biomass_log + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
                             data = sem_data, 
                             family = Gamma(link = "log"),
                             na.action = "na.fail",
                             control=glmerControl(
                               optimizer = "Nelder_Mead",
                               optCtrl=list(maxfun=1e6)))
# summary(tick_mod_noHosts_gamma)
effectsize::standardize(tick_mod_noHosts_gamma) %>% summary()

AIC(tick_mod_allHosts, tick_mod_deer, tick_mod_noRabbit, tick_mod_noHosts)


## Host models: all, deer only, no rabbit ####
host_mod_all <- glmer(total_clusters1m ~ avg_canopy_cover + biomass_log + (1 | inst_name/plot_id),
                       data = sem_data, family = poisson(link = "log"),
                       control = glmerControl(
                         optimizer="Nelder_Mead", optCtrl=list(maxfun=1e6)))
host_mod_deer <- glmer(deer ~ avg_canopy_cover + biomass_log + (1|inst_name/plot_id),
                      data = sem_data, family = poisson(link = "log"),
                      control = glmerControl(
                        optimizer="Nelder_Mead", optCtrl=list(maxfun=1e6)))
host_mod_noRabbit <- glmer(deer_other ~ avg_canopy_cover + biomass_log + (1|inst_name/plot_id),
                   data = sem_data, family = poisson(link = "log"),
                   control = glmerControl(
                     optimizer="Nelder_Mead", optCtrl=list(maxfun=1e6)))

## Litter/understory models ####
litter_cover_mod <- lmer(logit_litter ~ d_since_fire_log + fri15yr + avg_canopy_cover + (1|inst_name/plot_id),
                         data = sem_data)
litter_depth_mod <- lmer(avg_litter_depth_all ~ d_since_fire_log + fri15yr + avg_canopy_cover + (1|inst_name/plot_id), 
                         data = sem_data)
standing_biomass_mod <- lmer(biomass_log ~ d_since_fire_log + avg_canopy_cover + (1|inst_name/plot_id),
                data = sem_data,
                # family = Gamma(link = "log")
                )
plot(standing_biomass_mod)
simulateResiduals(standing_biomass_mod) %>% plot()
summary(standing_biomass_mod)


## Tree (overstory) canopy cover ####
# pine_mod <- lmer(logit_pct_pine ~ fri15yr_log + avg_canopy_cover +(1|inst_name/plot_id), 
#                  data = sem_data)
canopy_mod <- lmer(avg_canopy_cover ~ d_since_fire_log + fri15yr + (1|inst_name/plot_id), 
                   data = sem_data)
simulateResiduals(canopy_mod) %>% plot()
summary(canopy_mod)


## Fire regime models ####
time_since_fire_mod_pred <- glmer.nb(d_since_fire ~ fri15yr + (1|inst_name/plot_id),
         data = sem_data)
simulateResiduals(time_since_fire_mod) %>% plot()
plot(time_since_fire_mod)
plot_model(time_since_fire_mod, show.values = T)
plot_model(time_since_fire_mod, type = "pred", terms = "fri15yr [all]", show.data = T)

time_since_fire_mod <- lmer(d_since_fire_log ~ fri15yr + (1|inst_name/plot_id), data = sem_data)
simulateResiduals(time_since_fire_mod) %>% plot()
plot(time_since_fire_mod)
summary(time_since_fire_mod)
plot_model(time_since_fire_mod, show.values = T)

fri_mod <- lmer(fri15yr ~ cv_30yr_fire_days + (1 | inst_name/plot_id), 
                data = sem_data)
simulateResiduals(fri_mod) %>% plot()
plot(fri_mod)

sem_data$fri15yr_int <- ceiling(sem_data$fri15yr)
fri_mod_pred <- glmer.nb(fri15yr_int ~ cv_30yr_fire_days + (1 | inst_name/plot_id), 
                      data = sem_data,
                      # family = poisson(link = "log")
                      control = glmerControl(
                        optimizer="Nelder_Mead", optCtrl=list(maxfun=1e6))
                      )
simulateResiduals(fri_mod_pred) %>% plot()
plot(fri_mod_pred)

plot_model(fri_mod, type = "pred")
plot_model(fri_mod_pred, type = "pred")


## Weather ~ Climate model
avg_1yr_vp_mod <- lmer(avg_1yr_vp..Pa. ~ cv_30yr_fire_days + (1|inst_name), data = sem_data)
# simulateResiduals(avg_1yr_vp_mod) %>% plot()
# summary(avg_1yr_vp_mod)

## Path Models ####
sem_df <- as.data.frame(sem_data)
## Strictly human, all hosts
human_only_psem <- psem(
  # m,
  # human_no_Ramb_mod_allHosts,
  # tick_mod_allHosts_gamma,
  tpt_pois,
  host_mod_all,
  litter_cover_mod, litter_depth_mod, canopy_mod,
  m2,
  # time_since_fire_mod, 
  fri_mod,
  avg_1yr_vp_mod,
  data = sem_df
)
h_only_psem <- summary(human_only_psem)

## Strictly human, no rabbit hosts
human_only_noRabbit_psem <- psem(
  # human_no_Ramb_mod_noRabbit,
  tick_mod_noRabbit,
  host_mod_noRabbit,
  litter_cover_mod, litter_depth_mod, canopy_mod,
  time_since_fire_mod, fri_mod,
  avg_1yr_vp_mod,
  data = sem_df
)
h_only_noRabbit_psem <- summary(human_only_noRabbit_psem)

## Strictly human, deer only
human_only_deer_psem <- psem(
  # human_no_Ramb_mod_deer,
  tick_mod_deer,
  host_mod_all,
  litter_cover_mod, litter_depth_mod, canopy_mod,
  time_since_fire_mod, fri_mod,
  avg_1yr_vp_mod,
  data = sem_df
)
h_only_deer_psem <- summary(human_only_deer_psem)

## Strictly human, no hosts
human_only_noHost_psem <- psem(
  # human_no_Ramb_mod_noHosts,
  tick_mod_noHosts,
  litter_cover_mod, litter_depth_mod, canopy_mod,
  time_since_fire_mod, fri_mod,
  avg_1yr_vp_mod,
  data = sem_df
)
h_only_noHosts_psem <- summary(human_only_noHost_psem)

h_only_psem$AIC
h_only_noHosts_psem$AIC




