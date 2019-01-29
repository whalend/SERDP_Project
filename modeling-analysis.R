

library(plyr); library(dplyr); library(ggplot2); library(readr)
# library(stringi); library(stringr)
library(tidyr)
library(viridisLite); library(viridis)

library(lme4)
# devtools::install_github("jslefche/piecewiseSEM@devel", build_vignette = T)
library(piecewiseSEM)
# library(afex)
# library(sjPlot)

plotlevel_data <- read_csv("data/all_plotlevel_data.csv")
str(plotlevel_data)

model_data <- plotlevel_data %>%
      select(-starts_with("se_"), -starts_with("sd_"),
             -n_obs, -total_clusters2m) %>%
      filter(!is.na(imcy_inv))
str(model_data)

summary(model_data)
model_data <- filter(model_data, !is.na(years_since_fire))

## Grouping factors: installation, plot_id, visit_year
## plot_id + visit_year = unique id

## Broad factors of interest: fire, invasion

# GGally::ggpairs(select(model_data, imcy_inv, years_since_fire, total_ticks, contains("litter"), avg_canopy_cover, avg_pct_bare:richness, pct_pinus_dbh, plot_avg_veg_cover, -last_fire_year), aes(color = imcy_inv))

# GGally::ggpairs(select(model_data, imcy_inv, years_since_fire, total_ticks, avg_dry_litter_gm2, avg_pct_bare, plot_avg_veg_cover, avg_dry_standing_gm2, pct_pinus_dbh, avg_canopy_cover, ycoord_lat, total_clusters1m), aes(color = imcy_inv))

# GGally::ggpairs(select(model_data, imcy_inv, years_since_fire, total_ticks, avg_dry_litter_gm2, avg_pct_bare, plot_avg_veg_cover, richness, pct_pinus_dbh, avg_canopy_cover, ycoord_lat, total_clusters1m), aes(color = imcy_inv))

model_data <- model_data %>%
      select(ticks = total_ticks,
             yrs_fire = years_since_fire,
             litter = avg_dry_litter_gm2,
             can_cover = avg_canopy_cover,
             richness,
             lat = ycoord_lat,
             hosts = total_clusters1m,
             installation,
             plot_id)

names(model_data)


summary(model_data)

# model_data <- as.data.frame(model_data, row.names = NULL)
# str(model_data)


mm1 <- glmer(ticks ~ yrs_fire + litter + can_cover + richness + lat + hosts + (1|installation) + (1|plot_id), family = "poisson", data = model_data)
# plot(mm1)
# summary(mm1)

mm_litter <- lmer(litter ~ yrs_fire + (1|installation), data = model_data)
# plot(mm_litter)
# summary(mm_litter)

serdp_psem <- psem(
      nlme::lme(litter ~ yrs_fire + can_cover, random = ~1|installation, data = model_data),
      # lmer(litter ~ yrs_fire + can_cover + (1|installation), data = model_data),
      lmer(can_cover ~ yrs_fire + (1|installation), data = model_data),
      # glmer.nb(total_clusters1m ~ avg_canopy_cover + (1|installation), data = model_data),
      glmer(ticks ~ litter + (1|installation), family = "poisson", data = model_data))

summary(serdp_psem, conserve = T)

coefs(serdp_psem)



data("shipley")
library(nlme)
shipley.psem <- psem(

      lme(DD ~ lat, random = ~ 1 | site / tree, na.action = na.omit,
          data = shipley),

      lmer(Date ~ DD + lat + (1 | site / tree), na.action = na.omit,
           data = shipley),

      lme(Growth ~ Date, random = ~ 1 | site / tree, na.action = na.omit,
          data = shipley),

      glmer(Live ~ Growth + (1 | site) + (1 | tree),
            family = binomial(link = "logit"), data = shipley)

)
summary(shipley.psem)
