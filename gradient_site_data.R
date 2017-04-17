# Gradient Site Data

library(readr); library(readxl)
library(dplyr); library(tidyr)
library(ggplot2)

# read in data
biomass <- read_excel("data/gradient-sites/Biomass_July2015_FINAL.xlsx")
# Collected biomass of all plants (live and standing dead) plus litter within the confines of one 0.25 x 0.25 m quadrat per plot. Vines whose leaves projected areally over the quadrats were collected even if they were not rooted in the quadrat. For all other species, we made an effort to only collected aboveground biomass from plants rooted in the plot.
str(biomass)
biomass$avetillerlength <- as.numeric(biomass$avetillerlength)
summary(biomass70)
# cogon: green, live
# cogon_standingdead: rooted in plot, but dead/senesced >60% of tiller)
biomass <- biomass %>% mutate(dead_live_ratio = cogon_standingdead/cogon)

cogon_sites <- filter(biomass, trt == "REF")
summary(cogon_sites)
# rearrange data
cogon_sites <- gather(cogon_sites, type, biomass, cogon:unknown)
# ?gather
summary(cogon_sites)

cogon_data_plot <- ggplot(cogon_sites)
cogon_data_plot +
      geom_boxplot(aes (x = type, y = biomass*16))
cogon_data_plot +
      geom_point(aes(x = biomass*16, y = type)) +
      xlab("biomass (g/m^2)") +
      ylab("type") +
      theme_bw()

# ratio of standing dead to live cogon grass
cogon_data_plot +
      geom_point(aes(x = dead_live_ratio, y = site)) +
      xlab("Standing Dead:Live Cogongrass") +
      ylab("site") +
      theme_bw()





