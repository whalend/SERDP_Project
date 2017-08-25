#plot data

library(readxl)
library(plyr); library(dplyr); library(ggplot2)

plot_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "plotinfo")
plot_data$years_since_fire <- 2017 - plot_data$last_fire_year


tick_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "ticks")
dung_data <- tick_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "dung")


#' ## Quadrat Data
#' Total % cover
#'
#'    -litter
#'    -bare ground
#+ quadrat data ####
quadrat_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "quadrat1m")
summary(quadrat_data)
hist(quadrat_data$pct_green)
hist(quadrat_data$pct_litter)

quadrat_sub <- filter(quadrat_data, plot_id != "bland03" & plot_id != "bland02")
quadrat_sub <- (filter(quadrat_data, !quadrat_id %in% c("w2","w4","n2","n4","s2","s4","e2","e4")))
summary(quadrat_sub)
hist(quadrat_sub$pct_green)

t.test(quadrat_data$pct_green,quadrat_sub$pct_green)
t.test(quadrat_data$pct_litter,quadrat_sub$pct_litter)


#' ## Species Data - 1-meter Quadrats
#'
#+ species data ####
species_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "species1m")
species_data <- filter(species_data, date>"2017-06-01", plot_id != "bland03" & plot_id != "bland02")
summary(species_data)
length(unique(species_data$veg_id))
species_data <- left_join(
      species_data,
      select(plot_data, installation, plot_id, last_fire_year:years_since_fire),
      by = c("installation","plot_id")
)

species_sub <- (filter(species_data, !quadrat_id %in% c("w2","w4","n2","n4","s2","s4","e2","e4")))
length(unique(species_sub$veg_id))

## Testing for differences in dense vs. less dense data collection
# hist(species_data$pct_cover)
# hist(species_sub$pct_cover)
# t.test(species_data$pct_cover,species_sub$pct_cover)

species_over_5pct <- species_sub %>%
      filter(pct_cover>5) %>%
      select(installation,plot_id,veg_id,pct_cover,`Scientific Name`,years_since_fire, cogongrass, land_use)
species_over_5pct <- filter(species_over_5pct, cogongrass != "NA")

ggplot(species_over_5pct,
       aes(years_since_fire,pct_cover, color = `Scientific Name`)) +
      geom_point(position="jitter") +
      theme_bw() +
      facet_grid(installation~cogongrass) +
      ylab("% cover") +
      xlab("Years since fire") +
      ggtitle("Range of species cover at 1-meter quadrats")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/species-pct-cover.png",
       height = 7)

# datac$se <- datac$sd / sqrt(datac$N)
avg_species_cover <- species_over_5pct %>%
             group_by(installation, cogongrass, years_since_fire) %>%
             summarise(avg_pct_cover = mean(pct_cover),
                       occurrence = length(veg_id),
                       max_pct_cover = max(pct_cover),
                       se = sd(pct_cover)/sqrt(length(pct_cover)))
ggplot(avg_species_cover,
      aes(years_since_fire, avg_pct_cover, fill = cogongrass)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = avg_pct_cover + se, ymax = avg_pct_cover - se),
                    width = .2) +
      facet_grid(installation~.) +
      theme_bw() +
      ylab("Average % cover") +
      xlab("Years since fire") +
      ggtitle("Averaged % cover of vegetation for each plot",
              subtitle = "averaged across 1-meter quadrats")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/avg-pct-cover-bars.png",
       height = 7)

# species_sub <- (filter(species_data2, !quadrat_id %in% c("w2","w4","n2","n4","s2","s4","e2","e4")))
# species_sub_plot <- ggplot(species_sub %>%
#                              filter(pct_cover > 5) %>%
#                              group_by(plot_id, veg_id) %>%
#                              summarise(avg_pct_cover = mean(pct_cover),
#                                        occurrence = length(veg_id)),
#       aes(veg_id,avg_pct_cover, label = occurrence))

# species_sub_plot +
#       geom_bar(stat = "identity") +
#       facet_grid(plot_id ~.) +
#       theme_bw() +
#       ggtitle("12 quadrats per plot")

      # dbh_census_years %>%
      # group_by(sample_year, speciesid, stem_status) %>%
      # summarise(occurrence = length(unique(plotid))) %>%
      # arrange(desc(occurrence))

head(species_data)

ggplot(species_data %>% filter(pct_cover >=0, date > "2017-06-01", plot_id != "bland02", plot_id != "bland03") %>% group_by(installation, plot_id) %>%
             summarise(richness = length(unique(veg_id))),
       aes(plot_id, richness)) +
      geom_point() +
      facet_grid(.~installation) +
      theme_bw()

ggplot(species_data %>% filter(pct_cover >=0, date > "2017-06-01", plot_id != "bland02", plot_id != "bland03") %>% group_by(installation, plot_id),
       aes(plot_id, pct_cover, color = veg_id)) +
      geom_point(position = "jitter") +
      facet_grid(.~installation) +
      theme_bw()



#' ## Tree Stem Data

#' ## Tree Stem Data
#+ tree data ####
tree_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "trees")
tree_data

tree_data_sub <- filter(tree_data, date>"2017-06-01", plot_id != "bland03", plot_id != "bland02")

tree_data_sub <- left_join(
      tree_data_sub,
      select(plot_data, installation,plot_id,last_fire_year:years_since_fire),
      by = c("installation","plot_id"))

ggplot(tree_data_sub %>%
             group_by(installation, species, last_fire_year, cogongrass) %>%
             summarise(total_dbh = sum(dbh)),
       aes(last_fire_year, total_dbh)
      ) +
      geom_bar(aes(fill = species), stat = "identity", position = "dodge") +
      facet_grid(.~installation) +
      theme_bw()

ggplot(tree_data_sub %>%
             group_by(installation, species, years_since_fire, cogongrass),
       aes(years_since_fire, dbh, color = species)) +
      geom_point(position = "jitter") +
      facet_grid(installation~cogongrass) +
      theme_bw() +
      xlab("Years since fire") +
      ylab("Diameter at breast height")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/tree-dbh-jitter.png", height=7)

ggplot(tree_data_sub %>%
             group_by(installation, species, years_since_fire, cogongrass),
       aes(years_since_fire, height, color = species)) +
      geom_point(position = "jitter") +
      facet_grid(installation~cogongrass) +
      theme_bw() +
      xlab("Years since fire") +
      ylab("Tree height")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/tree-height-jitter.png", height=7)

#' ## Biomass data sampled at 25cm quadrats
#'
#+ 25cm biomass data ####
biomass_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "quadrat25cm")

biomass_data <- filter(biomass_data, date>"2017-04-25", plot_id!="bland02", plot_id!="bland03")
summary(biomass_data)
biomass_data$fuel_mass_wet <- round(as.numeric(biomass_data$fuel_mass_wet),2)
biomass_data <- left_join(
      select(biomass_data, installation,plot_id,fuel_mass_wet,litter_mass_wet),
      select(plot_data, installation,plot_id,last_fire_year:years_since_fire),
      by = c("installation","plot_id")
)
# biomass_data$last_fire_year <- as.integer(biomass_data$last_fire_year)
# biomass_data$years_since_fire <- 2017 - biomass_data$last_fire_year

ggplot(biomass_data,
       aes(years_since_fire, fuel_mass_wet*16)) +
      geom_point(aes(color = installation), position = "jitter") +
      # facet_grid(.~installation) +
      # geom_smooth(method = "loess")
      theme_bw() +
      ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
      xlab("Years since last fire") +
      ggtitle("Fresh standing biomass across burn units at each installation",
              subtitle = "(measurements at 25cm quadrats)")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/standing-biomass-hitter.png", height=7)

ggplot(biomass_data %>% group_by(installation, last_fire_year),
        aes(years_since_fire, litter_mass_wet*16)) +
      geom_point(aes(color = installation), position = "jitter") +
      # facet_grid(.~installation) +
      theme_bw() +
      ylab(expression(paste("Litter biomass (g/", m^{2}, ")"))) +
      xlab("Years since last fire") +
      ggtitle("Fresh litter biomass across burn units at each installation",
              subtitle = "(measurements at 25cm quadrats)")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/litter-biomass-jitter.png", height=7)

#' ## Cogongrass Data Sampled from Invasions
#'
#+ cogongrass data ####
cogon_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "CogonData")
cogon_biomass <- cogon_data %>%
      # select(site,plot_id,Quad,fresh_biomass) %>%
      filter(site!="NA") %>%
      mutate(biomass=fresh_biomass*16)
cogon_biomass <- left_join(
      cogon_biomass,
      select(plot_data, installation,plot_id,years_since_fire),
      by=c("site"="installation","plot_id"))
ggplot(cogon_biomass, aes(years_since_fire, biomass)) +
      geom_point() +
      theme_bw() +
      xlab("Years since fire") +
      ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
      ggtitle("Cogongrass invasion subsample at Avon Park AFR")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/cogon-biomass.png", height = 7)


#' ## Tick Data
#+ tick data ####
tick_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "ticks")
tick_data$plot_id <- tolower(tick_data$plot_id)

tick_data <- left_join(
      tick_data,
      select(plot_data, installation,plot_id,last_fire_year:cogongrass,years_since_fire),
      by = c("installation", "plot_id")
)

ggplot(tick_data %>%
             group_by(installation,years_since_fire,Species) %>%
             summarise(tick_number = sum(count)),
       aes(years_since_fire, tick_number, color = Species)) +
      geom_point(position = "jitter") +
      theme_bw()