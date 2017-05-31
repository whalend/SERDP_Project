#plot data

library(readxl)
library(plyr); library(dplyr); library(ggplot2)

quadrat_data <- read_excel("data/serdp-plot-data.xlsx", sheet = 5)
species_data <- read_excel("data/serdp-plot-data.xlsx", sheet = 6)

summary(quadrat_data)
hist(quadrat_data$pct_green)
hist(quadrat_data$pct_litter)
quadrat_sub <- (filter(quadrat_data, !quadrat_id %in% c("w2","w4","n2","n4","s2","s4","e2","e4")))
summary(quadrat_sub)
hist(quadrat_sub$pct_green)

t.test(quadrat_data$pct_green,quadrat_sub$pct_green)
t.test(quadrat_data$pct_litter,quadrat_sub$pct_litter)


summary(species_data)
length(unique(species_data$veg_id))
species_sub <- (filter(species_data, !quadrat_id %in% c("w2","w4","n2","n4","s2","s4","e2","e4")))
length(unique(species_sub$veg_id))

hist(species_data$pct_cover)
hist(species_sub$pct_cover)
t.test(species_data$pct_cover,species_sub$pct_cover)

species_data2 <- read_excel("data/serdp-plot-data-sigmaPlotadds.xlsx", sheet = 6)
species_plot <- ggplot(species_data2 %>%
                             filter(pct_cover > 5) %>%
                             group_by(plot_id, veg_id) %>%
                             summarise(avg_pct_cover = mean(pct_cover),
                                       occurrence = length(veg_id)),
      aes(veg_id,avg_pct_cover, label = occurrence))

species_plot +
      geom_bar(stat = "identity") +
      facet_grid(plot_id ~.) +
      theme_bw() +
      ggtitle("20 quadrats per plot")

species_sub <- (filter(species_data2, !quadrat_id %in% c("w2","w4","n2","n4","s2","s4","e2","e4")))
species_sub_plot <- ggplot(species_sub %>%
                             filter(pct_cover > 5) %>%
                             group_by(plot_id, veg_id) %>%
                             summarise(avg_pct_cover = mean(pct_cover),
                                       occurrence = length(veg_id)),
      aes(veg_id,avg_pct_cover, label = occurrence))

species_sub_plot +
      geom_bar(stat = "identity") +
      facet_grid(plot_id ~.) +
      theme_bw() +
      ggtitle("12 quadrats per plot")

      # dbh_census_years %>%
      # group_by(sample_year, speciesid, stem_status) %>%
      # summarise(occurrence = length(unique(plotid))) %>%
      # arrange(desc(occurrence))

