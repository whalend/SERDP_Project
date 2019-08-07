####### 2019 plot report data analysis #####

#### load packages ####
library(plyr); library(dplyr); library(ggplot2); library(readr)
library(stringi)

#+read in data ####
tick_data <- read_csv(file = "data/raw_data/2019_serdp_data/2019-only-tick-data.csv")


tick_data <- tick_data %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

tick_data <- tick_data %>% 
  filter(installation != "avonpark")


tail(tick_data)
unique(tick_data$installation)
unique(tick_data$plot_id)
unique(tick_data$life_stage)
unique(tick_data$location)

#####
# tick_data_nymphs <- tick_data %>% 
#   mutate(life_stage = case_when(count == "0" ~ "nymph",
#                                 TRUE ~ life_stage))
# 
# write_csv(tick_data_nymphs, "data/raw_data/2019_serdp_data/tick_data_nymphs.csv")
# 
# tick_data_adults <- tick_data %>% 
#   mutate(life_stage = case_when(count == "0" ~ "adult",
#                                 TRUE ~ life_stage))
# tick_data_adults <- tick_data_adults %>% 
#   filter(count == "0")
# 
# write_csv(tick_data_adults, "data/raw_data/2019_serdp_data/tick_data_adults.csv")
# 
# View(tick_data_adults)
#####


ticks_adult <- filter(tick_data, life_stage == "adult")
ticks_nymphs <- filter(tick_data, life_stage == "nymph")

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
invasion_fill <- scale_color_manual(values = c("deepskyblue", "red"))
invasion_color_2 <- scale_color_manual(values = c("deepskyblue", "red", "green", "purple"))
invasion_fill_2 <- scale_color_manual(values = c("deepskyblue", "red", "green", "purple"))

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

ggplot(tick_data, aes(installation, count, color = invaded)) +
  geom_boxplot() +
  theme_bw()

ticks_per_plot <- tick_data %>% 
  group_by(installation, date, plot_id, invaded) %>% 
  summarise(ticks_per_plot = sum(count)) 

total_ticks_all <- tick_data %>% 
  group_by(installation, invaded) %>% 
  summarise(ticks_per_plot = sum(count)) 

total_ticks_all$invaded[total_ticks_all$invaded=="y"] <- "zummaryY"
total_ticks_all$invaded[total_ticks_all$invaded=="n"] <- "zummaryN"


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

ggplot(total_ticks_all, aes(installation, ticks_per_plot), color = invaded) +
  geom_boxplot() +
  invasion_color +
  NULL

#reading in all quadrat 1m data and re running all of QAQC, ADDED 2019 DATA POINTS. 
#' quadrat_data <- read_csv("data/raw_data/2019_serdp_data/quadrat1m-data-entry.csv")
#' summary(quadrat_data)
#' str(quadrat_data)
#' 
#' #' Data Issues
#' #' * ~~4 `NA's` in 'date'~~
#' #' * ~~116 `NA's` in 'woody_veg_ht1~~
#' #' * ~~'herb_veg_ht' & 'pct_green' columns are reading in as character when they should be numeric~~
#' #' * 3 `NA's` in 'pct_litter'
#' #' * 63 `NA's` in 'pct_bare'
#' #' * ~~need unique plot id~~
#' #' * ~~need to filter out data from pilot plot visits, i.e. before 20170601~~
#' #'
#' #' Since we took up to three measurements of vegetation height and litter depth we deemed that if there wasn't anything meeting that category then the first measurement would be 0 while the remaining two would be recorded as `NA`. There shouldn't be any `NA's` in the first measurement column for these variables. Also, there should rarely (never?) be `NA's` for measures of percent cover.
#' 
#' #+ add unique plot id and investigate date NA's ####
#' quadrat_data <- quadrat_data %>%
#'   mutate(plot_id = paste(installation, plot_id, sep = " "))
#' unique(quadrat_data$plot_id)# "extra" plots from design tests
#' unique(plot_visit_data$plot_id)
#' quadrat_data$plot_id[quadrat_data$plot_id=="blanding cogan"] <- "blanding theater_cogon"
#' 
#' filter(quadrat_data, is.na(date))$plot_id
#' filter(plot_visit_data, plot_id=="blanding c1")
#' quadrat_data$date[is.na(quadrat_data$date)] <- 20180516
#' 
#' quadrat_data <- quadrat_data %>%
#'   filter(date > 20170601) # discard initial sampling test data
#' 
#' unique(plot_visit_data$plot_id) %in% unique(quadrat_data$plot_id)
#' 
#' summary(quadrat_data)
#' 
#' #+ investigate herb_veg_ht as character
#' sort(unique(quadrat_data$herb_veg_ht1))# "82I"
#' sort(unique(quadrat_data$herb_veg_ht2))# "74I"
#' sort(unique(quadrat_data$herb_veg_ht3))# "70I" & "nA"
#' 
#' 
#' filter(quadrat_data, herb_veg_ht1=="82I")$plot_id
#' quadrat_data$herb_veg_ht1[quadrat_data$herb_veg_ht1=="82I"] <- 82
#' filter(quadrat_data, herb_veg_ht2=="74I")$plot_id
#' quadrat_data$herb_veg_ht2[quadrat_data$herb_veg_ht2=="74I"] <- 74
#' filter(quadrat_data, herb_veg_ht3=="70I")$plot_id
#' quadrat_data$herb_veg_ht3[quadrat_data$herb_veg_ht3=="70I"] <- 70
#' filter(quadrat_data, herb_veg_ht3=="nA")$plot_id
#' ## jackson h1 okay to be NA
#' 
#' #+ convert herb_veg_ht to numeric ###
#' quadrat_data <- quadrat_data %>%
#'   mutate(herb_veg_ht1 = as.numeric(herb_veg_ht1),
#'          herb_veg_ht2 = as.numeric(herb_veg_ht2),
#'          herb_veg_ht3 = as.numeric(herb_veg_ht3))
#' 
#' summary(quadrat_data)
#' 
#' #+ check NA's in ht1 columns and convert to 0 as appropriate ####
#' summary(filter(quadrat_data, is.na(woody_veg_ht1)))
#' ## woody veg ht2 and 3 are all NA's for this subset, so woody veg ht1 = 0
#' # View(filter(quadrat_data, is.na(woody_veg_ht1), date < 20180101))
#' quadrat_data$woody_veg_ht1[is.na(quadrat_data$woody_veg_ht1)] <- 0
#' 
#' summary(filter(quadrat_data, is.na(herb_veg_ht1)))
#' ## same as for woody veg hts, so herb veg ht1 = 0
#' quadrat_data$herb_veg_ht1[is.na(quadrat_data$herb_veg_ht1)] <- 0
#' 
#' ## check 'pct_green' as character
#' unique(quadrat_data$pct_green)
#' quadrat_data$pct_green[quadrat_data$pct_green=="<1"] <- 0.5
#' quadrat_data$pct_green <- as.numeric(quadrat_data$pct_green)
#' 
#' ## check pct litter, pct bare, pct green NA's ####
#' filter(quadrat_data, is.na(pct_litter)) %>%
#'   select(plot_id, quadrat_id, pct_litter, pct_green, pct_bare, date)
#' ## All from the same plots/quadrats, probably the ones we whiffed on recording data. I (WD) went back to original datasheets to come up with some estimates based on values for the 25cm quadrat and the other 1m quadrats in the plot.
#' 
#' ### replacement values for Shelby I1 quadrat S10
#' quadrat_data$pct_litter[quadrat_data$plot_id=="shelby i1" & quadrat_data$quadrat_id=="s10"] <- 100
#' quadrat_data$pct_green[quadrat_data$plot_id=="shelby i1" & quadrat_data$quadrat_id=="s10"] <- 55
#' quadrat_data$pct_bare[quadrat_data$plot_id=="shelby i1" & quadrat_data$quadrat_id=="s10"] <- 0
#' 
#' ### replacement values for Shelby A1 quadrat e10
#' quadrat_data$pct_litter[quadrat_data$plot_id=="shelby a1" & quadrat_data$quadrat_id=="e10"] <- 20
#' quadrat_data$pct_green[quadrat_data$plot_id=="shelby a1" & quadrat_data$quadrat_id=="e10"] <- 66
#' quadrat_data$pct_bare[quadrat_data$plot_id=="shelby a1" & quadrat_data$quadrat_id=="e10"] <- 75
#' 
#' ### replacement values for Shelby B3 quadrat s10
#' quadrat_data$pct_litter[quadrat_data$plot_id=="shelby b3" & quadrat_data$quadrat_id=="s10"] <- 35
#' quadrat_data$pct_green[quadrat_data$plot_id=="shelby b3" & quadrat_data$quadrat_id=="s10"] <- 80
#' quadrat_data$pct_bare[quadrat_data$plot_id=="shelby b3" & quadrat_data$quadrat_id=="s10"] <- 65
#' 
#' summary(quadrat_data)
#' 
#' #+ check pct wood litter NAs ####
#' filter(quadrat_data, is.na(pct_wood_litter))
#' quadrat_data$pct_wood_litter[is.na(quadrat_data$pct_wood_litter)] <- 0
#' 
#' filter(quadrat_data, plot_id=="blanding c1") %>%
#'   select(installation, plot_id, date)
#' 
#' quadrat_data$date[quadrat_data$plot_id=="blanding c1" & quadrat_data$date==20170607] <- 20170609
#' 
#' quadrat_data$date <- as.Date(as.character(quadrat_data$date), format = "%Y%m%d")
#' quadrat_data <- quadrat_data %>%
#'   mutate(visit_year = lubridate::year(quadrat_data$date),
#'          plot_id = case_when(
#'            plot_id=="gordon z1" ~ "gordon a1",
#'            plot_id=="gordon y1" ~ "gordon b1",
#'            plot_id=="gordon x1" ~ "gordon c1",
#'            plot_id=="gordon w1" ~ "gordon d1",
#'            plot_id=="gordon v1" ~ "gordon e1",
#'            plot_id=="gordon t1" ~ "gordon f1",
#'            plot_id=="gordon s1" ~ "gordon g1",
#'            plot_id=="gordon r1" ~ "gordon h1",
#'            TRUE ~ plot_id
#'          ))
#' summary(quadrat_data)
#' 
#' unique(quadrat_data$plot_id)
#' 
#' write_csv(quadrat_data, "data/processed_data/quadrat1m.csv")

quadrat_data <- read_csv("data/processed_data/quadrat1m.csv")

quadrat_data_2019 <- quadrat_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"))


  # steven stop here sleepy. trying to string out last two characters for plot id to eventually get invaded/native status column.
quadrat_data_2019[is.na(quadrat_data_2019)] <- 0

quadrat_stats <- quadrat_data_2019 %>% 
  group_by(installation, date, plot_id, transect_id, status) %>% 
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

# filter(!is.na(woody_veg_ht1), !is.na(woody_veg_ht2), !is.na(woody_veg_ht3), !is.na(herb_veg_ht1), !is.na(herb_veg_ht2), !is.na(herb_veg_ht3), !is.na(litter_ht1), !is.na(litter_ht2), !is.na(litter_ht3)) %>%


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








