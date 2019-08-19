####### 2019 plot report data analysis #####

#### load packages ####
library(plyr); library(dplyr); library(ggplot2); library(readr)
library(stringi)

#+read in data ####
tick_data <- read_csv(file = "data/raw_data/2019_serdp_data/2019-only-tick-data.csv")


tick_data <- tick_data %>% 
  filter(installation !="avonpark") %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

tick_data <- tick_data %>% 
  mutate(invaded = if_else(invaded=="y", "invaded", "native"))


tail(tick_data)
unique(tick_data$installation)
unique(tick_data$plot_id)
unique(tick_data$life_stage)
unique(tick_data$location)

ticks_adult <- filter(tick_data, life_stage == "adult")
ticks_nymphs <- filter(tick_data, life_stage == "nymph")

adults <- ticks_adult %>% 
  group_by(life_stage) %>% 
  summarise(total_adult = sum(count))
nymph <- ticks_nymphs %>% 
  group_by(life_stage) %>% 
  summarise(total_nymph = sum(count))
status <- tick_data %>% 
  group_by(invaded, life_stage) %>% 
  summarise(total=sum(count))

###### setting figure themes #####

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_blank())
invasion_color <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_color_2 <- scale_color_manual(values = c("deepskyblue", "red", "green", "purple"))
invasion_fill_2 <- scale_color_manual(values = c("deepskyblue", "red", "green", "purple"))

##### working on tick figures by installation/status#####

adult_figure <- ggplot(ticks_adult, aes(date, count, color = invaded)) +
  geom_smooth(aes(color = invaded), method = "lm", alpha = .2) +
  geom_jitter(data = ticks_adult, aes(y = count, x = date, color = invaded)) +
  def_theme + 
  invasion_color +
  #invasion_fill + 
  xlab("Date") +
  ylab("Number of adults") +
  #scale_y_continuous(limits = c(0,5)) +
  NULL

nymph_figure <- ggplot(ticks_nymphs, aes(date, count, color = invaded)) +
  geom_smooth(aes(color = invaded), method = "lm", alpha = .2) +
  geom_jitter(data = ticks_nymphs, aes(y = count, x = date, color = invaded)) +
  def_theme + 
  #scale_x_date(date_breaks = "1 month", date_labels = "%m-%d-%Y") +
  #scale_x_continuous(limits = c(04-01-2019, 06-01-2019), breaks = c(04-01-2019, 05-01-2019, 06-01-2019)) +
  invasion_color +
  #invasion_fill + 
  xlab("Date") +
  ylab("Number of nymphs") + 
  #scale_y_continuous(limits = c(0,10)) +
  NULL

tick_data_no_brown <- tick_data %>% 
  filter(installation != "brown") %>% 
ggplot(tick_data, aes(invaded, count, color = invaded)) +
  geom_boxplot() +
  theme_bw()

ggplot(tick_data_no_brown, aes(invaded, count, color = invaded)) +
  geom_boxplot() +
  theme_bw()

####### tick abundance BY TRAP  
ggplot(tick_data, aes(invaded, count, color = invaded, position = "jitter")) +
                        geom_boxplot(outlier.size = NA) + 
                        geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
                        theme_classic() +
                        theme(text = element_text(size=20)) +
                        labs (y = 'Ticks per trap') +
                        xlab("") +
                        annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
                        invasion_color +
                        invasion_fill +
                        theme(legend.position="none")+
  scale_y_continuous(limits = c(0,10))

status_count <- tick_data %>% 
  group_by(invaded) %>% 
  summarise(number_invaded = n())

ticks_per_plot <- tick_data %>% 
  group_by(installation, date, plot_id, invaded) %>% 
  summarise(ticks_per_plot = sum(count)) 

total_ticks_all <- tick_data %>% 
  group_by(installation, invaded) %>% 
  summarise(ticks_per_plot = sum(count)) 

total_ticks_all$invaded[total_ticks_all$invaded=="invaded"] <- "zummaryY"
total_ticks_all$invaded[total_ticks_all$invaded=="native"] <- "zummaryN"


ggplot(ticks_per_plot, aes(installation, ticks_per_plot, color = invaded)) +
  geom_boxplot() +
  geom_boxplot(data = total_ticks_all, aes(invaded, ticks_per_plot, color = invaded)) +
  invasion_color_2 +
  invasion_fill_2 +
  ylab("Ticks per plot") +
  xlab("Site") +
  theme_bw()

ggplot(total_ticks_all, aes(invaded, ticks_per_plot), color = invaded) +
  geom_boxplot() +
  theme_bw() +
  invasion_color +
  invasion_fill +
  NULL

###### final tick figure for export by drew #####

tick_figure_status <- ggplot(data = ticks_per_plot, mapping = aes(x = invaded, y = ticks_per_plot, color= invaded, position = "jitter")) +
  #scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  #geom_boxplot(alpha = 0.5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Ticks per plot') +
  xlab("") +
  annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size=.25) 

ggsave(plot = tick_figure_status, "data/raw_data/2019_serdp_data/esa figures/tick_abundance.png", width = 7, height = 7)

#ggsave(plot = avg_time_days_temp_rh_barchart, "figures/tick-survival-assay/avg_time_days_temp_rh_barchart.png"

Maov<- aov(ticks_per_plot ~ invaded, data = ticks_per_plot)
summary(Maov)


ggplot(total_ticks_all, aes(installation, ticks_per_plot), color = invaded) +
  geom_boxplot() +
  invasion_color +
  NULL

###### post ESA looking at 17-18 tick data comparisons to 2019 

old_ticks <- read_csv("data/processed_data/ticks.csv")
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

old_w_status <- left_join(old_ticks, plot_visit_data, by = "plot_id")
old_w_status <- old_w_status[,c(1:19)]
old_w_status <- old_w_status[,-c(9:18)]
old_w_status <- old_w_status %>% 
  mutate(imcy_inv = if_else(imcy_inv=="uninvaded", "native", "invaded"))
colnames(old_w_status)[colnames(old_w_status)=="imcy_inv"] <- "invaded"
colnames(old_w_status)[colnames(old_w_status)=="visit_date.x"] <- "date"
colnames(old_w_status)[colnames(old_w_status)=="installation.x"] <- "installation"

tick_data <- tick_data[,-c(10:11)]


serdp_ticks <- rbind(old_w_status, tick_data)
serdp_ticks <- serdp_ticks %>% 
  mutate(visit_year = lubridate::year(date))
dod_ticks <- serdp_ticks %>% 
  filter(date < "2018-12-31") %>% 
  filter(invaded != "NA")
florida_ticks <- serdp_ticks %>% 
  filter(date > "2018-12-31")

ticks_on_dod <- ggplot(data = dod_ticks, mapping = aes(x = invaded, y = count, color= invaded, position = "jitter")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Ticks per trap') +
  xlab("") +
  annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  #coord_cartesian(ylim =c(0,75)) +
  theme(legend.position="none") +
  ggtitle("Ticks on DoD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,20), breaks = c(0, 5, 10, 15, 20))

ticks_2019 <- ggplot(data = florida_ticks, mapping = aes(x = invaded, y = count, color= invaded, position = "jitter")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("") +
  ylab("") +
  annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  theme(axis.title.y = element_blank()) +
  ggtitle("2019 Florida Ticks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #coord_cartesian(ylim =c(0,75)) +
  scale_y_continuous(limits = c(0,20), breaks = c(0, 5, 10, 15, 20))

cowplot::plot_grid(ticks_on_dod, ticks_2019, ncol = 2)

###### begin assessing 2019 plot data for veg hts, bioass, canopy cover, etc ####

quadrat_data <- read_csv("data/processed_data/quadrat1m.csv")

biomass_data <- read_csv("data/processed_data/quadrat25cm.csv")

canopy_data <- read_csv("data/processed_data/canopy-cover.csv")

quadrat_data_2019 <- quadrat_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"))

biomass_data_2019 <- biomass_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"))

canopy_data_2019 <- canopy_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"))

quadrat_data_2019[is.na(quadrat_data_2019)] <- 0

quadrat_stats <- quadrat_data_2019 %>% 
  group_by(installation, date, plot_id, transect_id, status) %>%
  filter(!is.na(woody_veg_ht1), !is.na(woody_veg_ht2), !is.na(woody_veg_ht3), !is.na(herb_veg_ht1), !is.na(herb_veg_ht2), !is.na(herb_veg_ht3), !is.na(litter_ht1), !is.na(litter_ht2), !is.na(litter_ht3)) %>%
  summarise(avg_woody_veg_ht = mean(woody_veg_ht1, woody_veg_ht2, woody_veg_ht3), 
            avg_herb_veg_ht = mean(herb_veg_ht1, herb_veg_ht2, herb_veg_ht3),
            avg_litter_ht = mean(litter_ht1, litter_ht2, litter_ht3))


quadrat_stats$status[quadrat_stats$plot_id=="n1"] <- "native"
quadrat_stats$status[quadrat_stats$plot_id=="n2"] <- "native"
quadrat_stats$avg_herb_veg_ht[quadrat_stats$avg_herb_veg_ht==454] <- 35


quadrat_stats_2 <- quadrat_stats %>% 
  group_by(installation, date, plot_id, status) %>% 
  summarise(plot_woody_veg_ht = mean(avg_woody_veg_ht),
            plot_herb_veg_ht = mean(avg_herb_veg_ht),
            plot_litter_ht = mean(avg_litter_ht))
#quadrat1m stats complete ready for join

biomass_stats <- biomass_data_2019 %>% 
  group_by(installation, date, plot_id, status) %>% 
  summarise(plot_standing_dry = mean(standing_fuel_mass_dry),
            plot_litter_dry = mean(litter_mass_dry))
biomass_stats$status[biomass_stats$plot_id=="n1"] <- "native"
biomass_stats$status[biomass_stats$plot_id=="n2"] <- "native"
#quadrat25cm biomass stats ready to join

canopy_stats <- canopy_data_2019 %>% 
  group_by(installation, date, plot_id, status) %>% 
  summarise(mean_canopy_cover_dots = mean(fill_dots, na.rm = T),
            plot_canopy_cover_pct = mean_canopy_cover_dots*1.04)
canopy_stats$status[canopy_stats$plot_id=="n1"] <- "native"
canopy_stats$status[canopy_stats$plot_id=="n2"] <- "native"
#canopy stats done ready to merge all#

total_1 <- left_join(biomass_stats, quadrat_stats_2)
quadrat_data_all_2019 <- left_join(canopy_stats, total_1)
write_csv(quadrat_data_all_2019, "data/processed_data/2019_quadrat_biomass_canopy_analysis.csv")

# filter(!is.na(woody_veg_ht1), !is.na(woody_veg_ht2), !is.na(woody_veg_ht3), !is.na(herb_veg_ht1), !is.na(herb_veg_ht2), !is.na(herb_veg_ht3), !is.na(litter_ht1), !is.na(litter_ht2), !is.na(litter_ht3)) %>%

###### SITE CONDITIONS figure making #####

#veg height-----
veg_ht <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_herb_veg_ht, color= status, position = "jitter")) +
  #scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  #geom_boxplot(alpha = 0.5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Vegetation height (cm)') +
  xlab("") +
  #annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  NULL
  #geom_hline(yintercept=0, linetype="dashed", color = "gray", size=.25) 
#ylim(-.5,19)

#biomass standing-----
biomass_standing <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_standing_dry, color= status, position = "jitter")) +
  #scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  #geom_boxplot(alpha = 0.5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Standing biomass (g)') +
  xlab("") +
  #annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  NULL
  #geom_hline(yintercept=0, linetype="dashed", color = "gray", size=.25) 
#ylim(-.5,19)

#biomass litter-----
biomass_litter <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_litter_dry, color= status, position = "jitter")) +
  #scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  #geom_boxplot(alpha = 0.5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Litter biomass (g)') +
  xlab("") +
  #annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  NULL
  #geom_hline(yintercept=0, linetype="dashed", color = "gray", size=.25) 
#ylim(-.5,19)

#canopy cover-----
canopy_cover <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_canopy_cover_pct, color= status, position = "jitter")) +
  #scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  #geom_boxplot(alpha = 0.5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Canopy cover (%)') +
  xlab("") +
  #annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none")
  #geom_hline(yintercept=0, linetype="dashed", color = "gray", size=.25) 
#ylim(-.5,19)

ggsave(plot = veg_ht, "data/raw_data/2019_serdp_data/esa figures/2019_veg_ht.png", height = 7, width = 7)

ggsave(plot = biomass_standing, "data/raw_data/2019_serdp_data/esa figures/2019_biomass_standing.png", height = 7, width = 7)

ggsave(plot = biomass_litter, "data/raw_data/2019_serdp_data/esa figures/2019_biomass_litter.png", height = 7, width = 7)

ggsave(plot = canopy_cover, "data/raw_data/2019_serdp_data/esa figures/2019_canopy_cover.png", height = 7, width = 7)


site_conditions <- cowplot::plot_grid(veg_ht, canopy_cover, biomass_standing, biomass_litter, ncol = 2)

ggsave(plot = site_conditions, "data/raw_data/2019_serdp_data/esa figures/2019_site_conditions.png", height = 8, width = 8)


#^ with all installation and summary box
ggplot(quadrat_stats_2, aes(installation, plot_herb_veg_ht, color = status)) +
  geom_boxplot() +
  geom_boxplot(data = quadrat_stats_all, aes(status, plot_herb_veg_ht, color = status)) +
  invasion_color_2 +
  theme_bw() +
  NULL

ggplot(quadrat_stats_2, aes(status, plot_herb_veg_ht, color = status)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 24) +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

quadrat_stats_all <- quadrat_stats %>% 
  group_by(installation, status) %>% 
  summarise(plot_herb_veg_ht = mean(avg_herb_veg_ht)) 

quadrat_stats_all$status[quadrat_stats_all$status=="invaded"] <- "zummaryI"
quadrat_stats_all$status[quadrat_stats_all$status=="native"] <- "zummaryN"


##### reading in secondary logger info #####

loggers <- read_csv("data/processed_data/2019-final-round-loggers-serdp.csv")

summary(loggers)
loggers <- loggers[c(2:5)]
  
loggers <- loggers %>% 
  mutate(date = substr(time, 1,8),
         time_2 = substr(time, 9,20),
         date_time = lubridate::mdy_hms(paste(date,time_2)))

loggers <- loggers[-c(1:1)]
loggers <- loggers[-c(5:5)]

loggers <- loggers %>% 
  filter(date_time < "2019-08-01 20:00:00") %>% 
  filter(date_time > "2019-06-12 20:00:00") %>% 
  filter(tempC > 10) %>% 
  filter(RH > 20) %>% 
  filter(logger_id != 19) %>%
  filter(logger_id != 17) %>%
  mutate(installation = "test",
         plot_id = "test",
         status = "n")

#adding all of installation and status and plot id text ##

loggers$installation[loggers$logger_id=="01"] <- "hancock"
loggers$installation[loggers$logger_id=="02"] <- "riversedge"
loggers$installation[loggers$logger_id=="03"] <- "riversedge"
loggers$installation[loggers$logger_id=="04"] <- "brown"
loggers$installation[loggers$logger_id=="05"] <- "silversprings"
loggers$installation[loggers$logger_id=="06"] <- "munson"
loggers$installation[loggers$logger_id=="07"] <- "silversprings"
loggers$installation[loggers$logger_id=="08"] <- "hancock"
loggers$installation[loggers$logger_id=="10"] <- "hancock"
loggers$installation[loggers$logger_id=="11"] <- "brown"
loggers$installation[loggers$logger_id=="12"] <- "munson"
loggers$installation[loggers$logger_id=="15"] <- "hancock"
loggers$installation[loggers$logger_id=="16"] <- "peace"
loggers$installation[loggers$logger_id=="17"] <- "wes"
loggers$installation[loggers$logger_id=="19"] <- "wes"
loggers$installation[loggers$logger_id=="20"] <- "peace"
loggers$installation[loggers$logger_id=="21"] <- "peace"
loggers$installation[loggers$logger_id=="23"] <- "wes"
loggers$installation[loggers$logger_id=="24"] <- "wes"

loggers$plot_id[loggers$logger_id=="01"] <- "n2"
loggers$plot_id[loggers$logger_id=="02"] <- "n1"
loggers$plot_id[loggers$logger_id=="03"] <- "i2"
loggers$plot_id[loggers$logger_id=="04"] <- "n2"
loggers$plot_id[loggers$logger_id=="05"] <- "n1"
loggers$plot_id[loggers$logger_id=="06"] <- "i1"
loggers$plot_id[loggers$logger_id=="07"] <- "i1"
loggers$plot_id[loggers$logger_id=="08"] <- "i3"
loggers$plot_id[loggers$logger_id=="10"] <- "n1"
loggers$plot_id[loggers$logger_id=="11"] <- "i2"
loggers$plot_id[loggers$logger_id=="12"] <- "n1"
loggers$plot_id[loggers$logger_id=="15"] <- "i4"
loggers$plot_id[loggers$logger_id=="16"] <- "i2"
loggers$plot_id[loggers$logger_id=="17"] <- "n1"
loggers$plot_id[loggers$logger_id=="19"] <- "i3"
loggers$plot_id[loggers$logger_id=="20"] <- "i1"
loggers$plot_id[loggers$logger_id=="21"] <- "n1"
loggers$plot_id[loggers$logger_id=="23"] <- "i3"
loggers$plot_id[loggers$logger_id=="24"] <- "n1"

loggers$status[loggers$logger_id=="01"] <- "native"
loggers$status[loggers$logger_id=="02"] <- "native"
loggers$status[loggers$logger_id=="03"] <- "invaded"
loggers$status[loggers$logger_id=="04"] <- "native"
loggers$status[loggers$logger_id=="05"] <- "native"
loggers$status[loggers$logger_id=="06"] <- "invaded"
loggers$status[loggers$logger_id=="07"] <- "invaded"
loggers$status[loggers$logger_id=="08"] <- "invaded"
loggers$status[loggers$logger_id=="10"] <- "native"
loggers$status[loggers$logger_id=="11"] <- "invaded"
loggers$status[loggers$logger_id=="12"] <- "native"
loggers$status[loggers$logger_id=="15"] <- "invaded"
loggers$status[loggers$logger_id=="16"] <- "invaded"
loggers$status[loggers$logger_id=="17"] <- "native"
loggers$status[loggers$logger_id=="19"] <- "invaded"
loggers$status[loggers$logger_id=="20"] <- "invaded"
loggers$status[loggers$logger_id=="21"] <- "native"
loggers$status[loggers$logger_id=="23"] <- "invaded"
loggers$status[loggers$logger_id=="24"] <- "native"

#fixing error because ggplot wouldnt plot date as character
loggers <- loggers %>% 
  mutate(date = as.Date(substr(date_time, 1,10)))

#View(loggers)

loggers_by_status <- loggers %>% 
  group_by(status, date) %>% 
  summarise(mean_tempC = mean(tempC),
            mean_rh = mean(RH))

write_csv(loggers_by_status, "data/raw_data/2019_serdp_data/scripts/loggers_by_status.csv")

#figure for average rh all time
ggplot(loggers_by_status, aes(date, mean_rh, color = status)) +
  geom_smooth(method = "lm", formula = y ~ (poly(x,3)), alpha = .2) +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

#figure for average temp all time
ggplot(loggers_by_status, aes(date, mean_tempC, color = status)) +
  geom_smooth() +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

loggers_by_logger <- loggers %>% 
  group_by(logger_id, status, date) %>% 
  summarise(min_daily_rh = min(RH)) %>% 
  ungroup(.) %>% 
  group_by(status, date) %>% 
  summarise(avg_daily_min_rh = mean(min_daily_rh)) %>% 
  select(status, date, avg_daily_min_rh)

write_csv(loggers_by_logger, "data/raw_data/2019_serdp_data/scripts/loggers_by_logger.csv")

#figure for daily min rh 
ggplot(loggers_by_logger, aes(date, avg_daily_min_rh, color = status)) +
  geom_smooth(method = "lm", formula = y ~ (poly(x,3)), alpha = .2) +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

#starting analysis on time below/above 

thres_1 <- logger_thresholds_below_75 <- loggers %>%
  filter(between(RH, 20, 74.99)) %>%
  group_by(date, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5) %>%
  select(date, status, logger_id, minutes_below_75)

thres_2 <- logger_thresholds_bw_75_85 <- loggers %>%
  filter(between(RH, 75, 85)) %>%
  group_by(date, status, logger_id) %>%
  summarise(obs_bw_75_85 = n(),
            minutes_bw_75_85 = obs_bw_75_85*5) %>%
  select(date, status, logger_id, minutes_bw_75_85)

thres_3 <- logger_thresholds_above_85 <- loggers %>%
  filter(RH >85.01) %>%
  group_by(date, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5) %>%
  select(date, status, logger_id, minutes_above_85)

threshold_75 <- left_join(thres_2, thres_1)
humidity_75 <- left_join(thres_3, threshold_75)

humidity_75 <- humidity_75 %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12")))

##^save that as csv later for drew
write_csv(humidity_75, "data/raw_data/2019_serdp_data/scripts/esa_serdp_thresholds.csv")

#above 85
ggplot(humidity_75, aes(days, minutes_above_85, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humidity_75, aes(y = minutes_above_85, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  #invasion_fill +
  ylab("Minutes above 85% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes above 85 % RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  NULL

#below 75
ggplot(humidity_75, aes(days, minutes_below_75, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humidity_75, aes(y = minutes_below_75, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  #invasion_fill +
  ylab("minutes_below_75") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "minutes_below_75")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  NULL

#bw 75-85

ggplot(humidity_75, aes(days, minutes_bw_75_85, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humidity_75, aes(y = minutes_bw_75_85, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  #invasion_fill +
  ylab("minutes_bw_75_85") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "minutes_bw_75_85")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  NULL

loggers_by_status_2 <- loggers %>% 
  group_by(status) %>% 
  summarise(mean_tempC = mean(tempC),
            mean_rh = mean(RH))








