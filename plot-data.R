library(plyr); library(dplyr); library(ggplot2); library(readr)
library(stringi)

#+ plot data ####
# plot_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/plot_info.csv")

plot_locations <- rgdal::readOGR("data/plot-locations.shp")
plot_locations@data$xcoord_lon <- plot_locations@coords[,1]
plot_locations@data$ycoord_lat <- plot_locations@coords[,2]

plot_data <- plot_locations@data
plot_data$last_fire_year <- as.integer(as.character(plot_data$fire_year))
plot_data <- plot_data %>%
      # filter(is.na(last_fire_year)==FALSE) %>%
      rename(plot_id = name, description = descriptio, installation = instal) %>%
      mutate(burn_unit_id = stri_sub(plot_id,1,-2)) %>%
      select(plot_id, description, imcy_inv:burn_unit_id)

plot_data$burn_unit_id[plot_data$burn_unit_id=="Flame"] <- "BlandingC"
plot_data$last_fire_year[is.na(plot_data$last_fire_year)] <-
plot_data$imcy_inv[is.na(plot_data$imcy_inv)] <- "uninvaded"
plot_data$plot_id <- tolower(plot_data$plot_id)

plot_data$years_since_fire <- 2017 - plot_data$last_fire_year
## Add labels column for installation names
plot_data$installation_full_name[plot_data$installation=="blanding"] <- "Camp Blanding"
plot_data$installation_full_name[plot_data$installation=="eglin"] <- "Eglin AFB"
plot_data$installation_full_name[plot_data$installation=="tyndall"] <- "Tyndall AFB"
plot_data$installation_full_name[plot_data$installation=="avonpark"] <- "Avon Park AFR"
plot_data$installation_full_name[plot_data$installation=="shelby"] <- "Camp Shelby"
plot_data$installation_full_name[plot_data$installation=="moody"] <- "Moody AFB"

##


ggplot(plot_data, aes(installation)) +
      geom_bar() +
      ylab("Number of plots") +
      ggtitle("Number of plots established at each installation") +
      theme_bw()

ggplot(plot_data, aes(as.factor(years_since_fire))) +
      geom_bar(aes(fill = imcy_inv), position = "dodge") +
      ylab("Number of plots") +
      xlab("Years since last fire") +
      ggtitle("Plots sampled across burn units") +
      theme_bw()
# Number of burn units sampled
length(unique(plot_data$fire_year))

ggplot(plot_data, aes(as.factor(years_since_fire))) +
      geom_bar() +
      facet_grid(installation ~ .) +
      ylab("Number of plots") +
      xlab("Years since last fire") +
      ggtitle("Number of plots across burn units by installation") +
      theme_bw()

ggplot(plot_data, aes(imcy_inv)) +
      geom_bar() +
      ylab("Number of plots") +
      xlab("") +
      ggtitle("Invaded plots: 8, Uninvaded plots: 21") +
      theme_bw()


#' ## Quadrat Data
#' Percent cover
#'
#'    - litter
#'    - bare ground
#'    - green
#'
#' Heights
#'
#'    - woody vegetation
#'    - grassy/herbaceous vegetation
#'    - litter (depth)
#'
#+ quadrat data ####
quadrat_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/quadrat1m.csv")
quadrat_data$date <- as.Date(quadrat_data$date, format = "%m/%d/%Y")
quadrat_data <- filter(quadrat_data, date > "2017-06-01")# discard early data collection at Camp Blanding

summary(quadrat_data)

quadrat_data$woody_veg_ht1[is.na(quadrat_data$woody_veg_ht1)] <- 0
# quadrat_data$woody_veg_ht1 <- as.numeric(quadrat_data$woody_veg_ht1)
quadrat_data$veg_ht1[is.na(quadrat_data$veg_ht1)] <- 0
# quadrat_data$veg_ht1 <- as.numeric(quadrat_data$veg_ht1)
quadrat_data$pct_green[quadrat_data$pct_green=="<1"] <- 0.5
quadrat_data$pct_green <- as.numeric(quadrat_data$pct_green)

hist(quadrat_data$pct_green)
hist(quadrat_data$pct_litter)

quadrat_summaries <- quadrat_data %>%
      group_by(installation, plot_id) %>%
      summarise(
            avg_woody_ht1 = mean(woody_veg_ht1),
            avg_woody_ht_all = mean(
                  c(woody_veg_ht1,woody_veg_ht2,woody_veg_ht3), na.rm = T),
            avg_herb_ht1 = mean(veg_ht1),
            avg_herb_ht_all = mean(c(veg_ht1,veg_ht2,veg_ht3), na.rm = T),
            avg_litter_depth1 = mean(litter_ht1),
            avg_litter_depth_all =
                  mean(c(litter_ht1,litter_ht2,litter_ht3), na.rm = T),
            avg_pct_green = mean(pct_green),
            avg_pct_litter = mean(pct_litter),
            avg_pct_wood = mean(pct_wood),
            avg_pct_bare = mean(pct_bare))

quadrat10m_summaries <- quadrat_data %>%
      filter(quadrat_id == "n10"|quadrat_id == "e10"|quadrat_id=="s10"|quadrat_id=="w10") %>%
      group_by(installation, plot_id) %>%
      summarise(
            avg_woody_ht1 = mean(woody_veg_ht1),
            avg_woody_ht_all = mean(
                  c(woody_veg_ht1,woody_veg_ht2,woody_veg_ht3), na.rm = T),
            avg_herb_ht1 = mean(veg_ht1),
            avg_herb_ht_all = mean(c(veg_ht1,veg_ht2,veg_ht3), na.rm = T),
            avg_litter_depth1 = mean(litter_ht1),
            avg_litter_depth_all =
                  mean(c(litter_ht1,litter_ht2,litter_ht3), na.rm = T),
            avg_pct_green = mean(pct_green),
            avg_pct_litter = mean(pct_litter),
            avg_pct_wood = mean(pct_wood),
            avg_pct_bare = mean(pct_bare))

summary(quadrat_summaries)
summary(quadrat10m_summaries)

#' ## Compare All Quadrat Data to Subset of Data
#' Test for differences in means of data summarized across all quadrats (12) to data from the quadrats at 10-meters (4).
#'
#+ compare all quadrats to subset of 4 at 10m ####
par(mfrow=c(1,2))
hist(log(quadrat_summaries$avg_woody_ht1))
hist(log(quadrat10m_summaries$avg_woody_ht1))
t.test(log(quadrat_summaries$avg_woody_ht1),log(quadrat10m_summaries$avg_woody_ht1), var.equal = F)

hist(log(quadrat_summaries$avg_woody_ht_all))
var(log(quadrat_summaries$avg_woody_ht_all))
hist(log(quadrat10m_summaries$avg_woody_ht_all))
var(log(quadrat10m_summaries$avg_woody_ht_all))
t.test(log(quadrat_summaries$avg_woody_ht_all),log(quadrat10m_summaries$avg_woody_ht_all), var.equal = F)

hist(log(quadrat_summaries$avg_herb_ht1))
var(log(quadrat_summaries$avg_herb_ht1))
hist(log(quadrat10m_summaries$avg_herb_ht1))
var(log(quadrat10m_summaries$avg_herb_ht1))
t.test(log(quadrat_summaries$avg_herb_ht1),
       log(quadrat10m_summaries$avg_herb_ht1), var.equal = F)

hist(log(quadrat_summaries$avg_herb_ht_all))
var(log(quadrat_summaries$avg_herb_ht_all))
hist(log(quadrat10m_summaries$avg_herb_ht_all))
var(log(quadrat10m_summaries$avg_herb_ht_all))
t.test(log(quadrat_summaries$avg_herb_ht_all),
       log(quadrat10m_summaries$avg_herb_ht_all), var.equal = F)

hist(log(quadrat_summaries$avg_litter_depth1))
var(log(quadrat_summaries$avg_litter_depth1))
hist(log(quadrat10m_summaries$avg_litter_depth1))
var(log(quadrat10m_summaries$avg_litter_depth1))
t.test(log(quadrat_summaries$avg_litter_depth1),
       log(quadrat10m_summaries$avg_litter_depth1), var.equal = F)

hist(log(quadrat_summaries$avg_litter_depth_all))
var(log(quadrat_summaries$avg_litter_depth_all))
hist(log(quadrat10m_summaries$avg_litter_depth_all))
var(log(quadrat10m_summaries$avg_litter_depth_all))
t.test(log(quadrat_summaries$avg_litter_depth_all),
       log(quadrat10m_summaries$avg_litter_depth_all), var.equal = T)

hist((quadrat_summaries$avg_pct_green))
var((quadrat_summaries$avg_pct_green))
hist((quadrat10m_summaries$avg_pct_green))
var((quadrat10m_summaries$avg_pct_green))
t.test((quadrat_summaries$avg_pct_green),
       (quadrat10m_summaries$avg_pct_green), var.equal = F)

hist(log(quadrat_summaries$avg_pct_litter))
var(log(quadrat_summaries$avg_pct_litter))
hist(log(quadrat10m_summaries$avg_pct_litter))
var(log(quadrat10m_summaries$avg_pct_litter))
t.test(log(quadrat_summaries$avg_pct_litter),
       log(quadrat10m_summaries$avg_pct_litter), var.equal = F)

hist(log1p(quadrat_summaries$avg_pct_bare))
var((quadrat_summaries$avg_pct_bare))
hist(log1p(quadrat10m_summaries$avg_pct_bare))
var((quadrat10m_summaries$avg_pct_bare))
t.test(log1p(quadrat_summaries$avg_pct_bare),
       log1p(quadrat10m_summaries$avg_pct_bare), var.equal = F)

#' There are no statistical differences in the means of these variables, which are aggregated to the plot level, based on these t-tests.

#' ## Canopy Cover Data
#'
#+ load canopy cover data ####
canopy_cover <- read_csv("~/Dropbox (UF)/SERDP-Project/data/densiometer_canopy_cover.csv")

canopy_cover_summary <- canopy_cover %>%
      group_by(installation, plot_id) %>%
      summarise(avg_canopy_cover = mean(pct_canopy_cover),
                sd_canopy_cover = sd(pct_canopy_cover),
                n_obs = 12)

#+ join data summarised at plot level ####
plot_data1 <- left_join(ungroup(plot_data), ungroup(canopy_cover_summary))
plot_data1 <- left_join(ungroup(plot_data), ungroup(quadrat_summaries))

#' ## Species Data - 1-meter Quadrats
#'
#+ species data ####
species_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/species1m.csv")
species_data$date <- as.Date(species_data$date, format = "%m/%d/%Y")
species_data <- filter(species_data, date>"2017-06-01")
species_data$veg_id <- tolower(species_data$veg_id)
length(unique(species_data$veg_id))
species_data <- left_join(
      species_data,
      select(plot_data, installation, plot_id, fire_year:imcy_inv),
      by = c("installation","plot_id")
)
summary(species_data)

species_sub <- (filter(species_data, quadrat_id %in% c("e10","w10","n10","s10")))
length(unique(species_sub$veg_id))
summary(species_sub)

## Testing for differences in dense vs. less dense data collection
hist(log(species_data$pct_cover))
var(log(species_data$pct_cover))
hist(log(species_sub$pct_cover))
var(log(species_sub$pct_cover))
t.test(log(species_data$pct_cover), log(species_sub$pct_cover),
       var.equal = T, paired = F)


species_over_5pct <- filter(species_data, pct_cover>5)
# species_sub_over_5pct <- filter(species_sub_over_5pct, cogongrass != "NA")

ggplot(species_over_5pct,
       aes(years_since_fire, pct_cover, color = veg_id)) +
      geom_point(position="jitter") +
      theme_bw() +
      facet_grid(installation~imcy_inv) +
      ylab("% cover") +
      xlab("Years since fire") +
      ggtitle("Range of species cover (>5%) at 1-meter quadrats")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/species-pct-cover.png",
       height = 7)

# datac$se <- datac$sd / sqrt(datac$N)
avg_species_cover <- species_data %>%
             group_by(installation, imcy_inv, years_since_fire) %>%
             summarise(avg_pct_cover = mean(pct_cover),
                       # n_obs = length(veg_id),
                       max_pct_cover = max(pct_cover),
                       se = sd(pct_cover)/sqrt(length(pct_cover)))
ggplot(avg_species_cover,
      aes(as.factor(years_since_fire), avg_pct_cover, fill = imcy_inv)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = avg_pct_cover + se, ymax = avg_pct_cover - se),
                    width = .2, position=position_dodge(width = .9)) +
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

ggplot(species_data %>%
             group_by(installation, plot_id) %>%
             summarise(richness = length(unique(veg_id))),
       aes(plot_id, richness)) +
      geom_point() +
      facet_grid(.~installation) +
      theme_bw()

ggplot(species_data %>%
             filter(pct_cover > 1) %>%
             group_by(installation, plot_id),
       aes(plot_id, pct_cover, color = veg_id)) +
      geom_point(position = "jitter") +
      facet_grid(.~installation) +
      theme_bw()



#' ## Tree Stem Data

#' ## Tree Stem Data
#+ tree data ####
tree_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/tree-data.csv")
tree_data

tree_data_sub <- filter(tree_data, date>"2017-06-01", plotid != "bland03", plotid != "bland02")

tree_data_sub <- left_join(
      tree_data_sub,
      select(plot_data, installation:full_names),
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
biomass_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/quadrat25cm.csv")

biomass_data <- filter(biomass_data, date>"2017-04-25", plot_id!="bland02", plot_id!="bland03")
summary(biomass_data)
biomass_data$fuel_mass_wet <- round(as.numeric(biomass_data$fuel_mass_wet),2)
biomass_data <- left_join(
      select(biomass_data, installation,plot_id,fuel_mass_wet,litter_mass_wet),
      select(plot_data, installation:full_names),
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
cogon_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/cogon-data.csv")
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
      ggtitle("Cogongrass invasion subsample")
ggsave("~/Dropbox (UF)/SERDP-Project/figures/cogon-biomass.png", height = 7)


#' ## Tick Abundance Estimates
#+ tick data ####
tick_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/ticks.csv")

tick_data$plot_id <- tolower(tick_data$plot_id)
tick_data$plot_id[tick_data$plot_id=="c"] <- "c1"

tick_data <- left_join(
      tick_data,
      select(plot_data, installation,plot_id,fire_year:full_names),
      by = c("installation", "plot_id")
)
summary(tick_data)
filter(tick_data, is.na(years_since_fire))
# "extra" ticks at Moody AFB were from an area last burned in 2005
tick_data$years_since_fire[tick_data$installation=="moody"] <- 2017-2005


ggplot(tick_data %>%
             group_by(installation,years_since_fire,Species) %>%
             summarise(tick_number = sum(count)),
       aes(as.factor(years_since_fire), tick_number)) +
      # geom_point(position = "jitter") +
      geom_bar(fill = "#124873", stat = "identity", position = "dodge") +
      # facet_grid(installation~., scales = "free_y") +
      xlab("Years since fire") +
      ylab("Tick abundance") +
      theme_bw() +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 22))

ggplot(tick_data %>%
             group_by(full_names, years_since_fire, Species) %>%
             summarise(tick_number = sum(count)),
       aes(as.factor(years_since_fire), tick_number, fill = Species)) +
      # geom_point(position = "jitter") +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(full_names~., scales = "free_y") +
      xlab("Years since fire") +
      ylab("Tick abundance") +
      theme_bw() +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 22))

#' ## Host Abundance Estimates
#+ dung data ####
dung_data <- read_csv("~/Dropbox (UF)/SERDP-Project/data/dung.csv")
