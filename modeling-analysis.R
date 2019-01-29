

library(plyr); library(dplyr); library(ggplot2); library(readr)
# library(stringi); library(stringr)
library(tidyr)
library(viridisLite); library(viridis)

library(lme4)
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

GGally::ggpairs(select(model_data, imcy_inv, years_since_fire, total_ticks, avg_dry_litter_gm2, avg_pct_bare, plot_avg_veg_cover, richness, pct_pinus_dbh, avg_canopy_cover, ycoord_lat, total_clusters1m), aes(color = imcy_inv))

model_data <- select(model_data, total_ticks, years_since_fire, avg_dry_litter_gm2, avg_canopy_cover, richness, ycoord_lat, total_clusters1m, installation, plot_id)


mm1 <- glmer(total_ticks ~ years_since_fire + avg_dry_litter_gm2 + avg_canopy_cover + richness + ycoord_lat + total_clusters1m + (1|installation) + (1|plot_id), family = poisson, data = model_data)
plot(mm1)
summary(mm1)

mm_litter <- lmer(avg_dry_litter_gm2 ~ years_since_fire + (1|installation), data = model_data)
plot(mm_litter)
summary(mm_litter)

serdp_psem <- psem(
      list(
            lmer(avg_dry_litter_gm2 ~ years_since_fire + (1|installation), data = model_data),
            lmer(avg_canopy_cover ~ years_since_fire + (1|installation), data = model_data),
            glmer.nb(total_clusters1m ~ avg_canopy_cover + ycoord_lat + (1|installation), data = model_data),
            glmer.nb(total_ticks ~ avg_dry_litter_gm2 + total_clusters1m + (1|installation), data = model_data)
            )
)

