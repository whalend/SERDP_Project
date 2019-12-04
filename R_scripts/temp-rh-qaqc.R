#' # Script for doing QA/QC on tick assay temp/RH logger data

library(plyr)
# require(lubridate)
library(tidyverse)
# library(dplyr)
# library(readr)
library(ggplot2)
library(stringi)

#+ load logger data ####
temp_rh_data <- read_csv("data/processed_data/tick_survival_combined_temperature_rh.csv")

summary(temp_rh_data)
names(temp_rh_data)

# unique(temp_rh_data$logger_id)
# unique(as.integer(temp_rh_data$logger_id))

temp_rh_data <- temp_rh_data %>%
      mutate(id = as.integer(logger_id),
            status = ifelse(id <= 12, "invaded", "native"),
            date_time = lubridate::ymd_hms(paste(date,time)),
            days = julian(date, origin = as.Date("2018-06-21")))

## Added invaded/native status and days since launch
test_logger_1 <- temp_rh_data %>% 
  filter(id==5)

# Look at seperate dataframes for native and invaded ####
temp_rh_data %>%
      filter(status =="invaded") %>%
      summary(.)

temp_rh_data %>%
      filter(status =="native") %>%
      summary(.)

temp_rh_data %>%
      filter(status =="invaded", RH < 20) %>%
      summary(.)

temp_rh_data %>%
      filter(status =="native", RH < 20) %>%
      summary(.)

## invaded 92.16 avg RH all included, 94.65 avg RH excluding < 15 RH
## native 92.89 avg RH all included, 93.38 avg RH excluding < 15 RH

messed_up_loggers <- temp_rh_data %>%
      filter(RH < 15)

summary(messed_up_loggers)
unique(messed_up_loggers$logger_id)

## RH < 10 at loggers 1, 2, and 19. Total of 13353 data points
## Moves to loggers 1, 2, 9, and 19 with RH < 15. Total of 14042 data points

# Logger 1 processing ####

logger_1 <- temp_rh_data %>%
      filter(logger_id == "01")

plot(logger_1$date_time, logger_1$RH)

messed_up_logger_1 <- messed_up_loggers %>%
      filter(logger_id == "01")

summary(logger_1)
summary(messed_up_logger_1)

logger_1 <- logger_1 %>%
      filter(RH > 15)

summary(logger_1)

rm(logger_1)
rm(messed_up_logger_1)

## logger 1 jumps from 85.17 to 88.21 avg RH when removing < 15 RH

# Logger 2 processing ####

logger_2 <- temp_rh_data %>%
      filter(logger_id == "02")

plot(logger_2$date_time, logger_2$RH)

# logger_3 <- temp_rh_data %>%
#       filter(logger_id == "03")
# plot(logger_3$date_time, logger_3$RH)

messed_up_logger_2 <- messed_up_loggers %>%
      filter(logger_id == "02")

summary(logger_2)
summary(messed_up_logger_2)

logger_2 <- logger_2 %>%
      filter(RH > 15)

summary(logger_2)

rm(logger_2)
rm(messed_up_logger_2)

## logger 2 is most messed up, avg RH jumps from 64.82 to 90.15 when removing < 15

# Logger 9 processing ####

logger_9 <- temp_rh_data %>%
      filter(logger_id == "09")

messed_up_logger_9 <- messed_up_loggers %>%
      filter(logger_id == "09")

summary(logger_9)
summary(messed_up_logger_9)

rm(logger_9)
rm(messed_up_logger_9)

## Logger 19 processing ####

logger_19 <- temp_rh_data %>%
      filter(logger_id == "19")

messed_up_logger_19 <- messed_up_loggers %>%
      filter(logger_id == "19")

summary(logger_19)
summary(messed_up_logger_19)

logger_19 <- logger_19 %>%
      filter(RH > 15)

summary(logger_19)

rm(logger_19)
rm(messed_up_logger_19)

## logger 19 avg RH jump from 72.72 to 77.57 removing < 15 RH ##

rm(messed_up_loggers)

# filter(temp_rh_data, date == "2018-10-25") %>%
#       select(date, days)

# ggplot(temp_rh_data, aes(date_time, RH)) +
# geom_point(aes(color = as.numeric(logger_id), shape = status), alpha = .2)

## Summary values for each logger ####

temp_rh_data %>%
      group_by(logger_id, status) %>%
      summarise(avg_rh = mean(RH),
                max_rh = max(RH),
                min_rh = min(RH)) %>%
      # View(.)
      # filter(min_rh>1) %>%
      summary(.)
## suggests that values below ~12% RH should be strongly questioned

# lubridate::ymd_hms(paste(temp_rh_data$date, temp_rh_data$time))

## Average temp and RH per treatment PER DAY ####

temp_rh_data_grouped <- temp_rh_data %>%
      filter(RH >= 15, date != "2018-06-21") %>%
      group_by(date, status, logger_id, days) %>%
      summarise(avg_daily_tempC = mean(tempC),
                max_tempC = max(tempC),
                min_tempC = min(tempC),
                avg_daily_RH = mean(RH),
                avg_max_rh = max(RH),
                avg_min_rh = min(RH)
      ) %>%
      ungroup(.) %>%
      group_by(date, status, days) %>%
      summarise(avg_daily_tempC = mean(avg_daily_tempC),
                avg_dailymin_tempC = mean(min_tempC),
                avg_dailymax_tempC = mean(max_tempC),
                avg_daily_RH = mean(avg_daily_RH),
                avg_dailymax_rh = mean(avg_max_rh),
                avg_dailymin_rh = mean(avg_min_rh)
      )

# write_csv(temp_rh_data_grouped, "data/processed_data/tsa_temp_rh_trt_grp.csv")

## calculation for coefficient of variation
### CV = (sd/mean)*100

## Calculation of vapor pressure deficit ####
## https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit

### saturation vapor pressure in millibars (mb), corrected for temp in ºC
get.es <- function(temp){
      es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp)))
      return(es)
}
## Where 𝐿 is the latent heat of vaporization, 2.5×106 J kg−1, 𝑅𝑣 is the gas constant for water vapor (461 J K−1kg−1).

## Then calculate vapor pressure deficit, which is the difference between the saturation vapor pressure, es,  and the actual vapor pressure
get.vpd <- function(rh, temp){
      ## calculate saturation vapor pressure
      es <- get.es(temp)
      ## calculate vapor pressure deficit
      vpd <- ((100 - rh) / 100) * es
      return(vpd)
}

## ## Calculate mean saturation vapor pressure (esm)
# get.esm <- function(tmin, tmax){
#       esmn <-  .6108 * exp((17.27 * tmin) / (tmin + 237.3))
#       esmx <-  .6108 * exp((17.27 * tmax) / (tmax + 237.3))
#       esm <- (esmn+esmx)/2
#       return(esm)
# }
#
# ## Calculate actual vapor pressure (ea)
#
# get.ea <- function(rh, tmin, tmax){
#       esm <- get.esm(tmin, tmax)
#       ea <- (rh/100) * esm
#       return(ea)
# }
#
# ## Calculate vapor pressure deficit (vpd = esm - ea; getting esm and ea functions)
#
# get.vpd <- function(rh, tmin, tmax){
#       esm <- get.esm(tmin, tmax)
#       ea <- get.ea(rh, tmin, tmax)
#       vpd <- esm - ea
#       return(vpd)
# }
#
# get.ea <- function(rhmin, rhmax, tmin, tmax){
#       esmn <- get.esmn(tmin) # other fun same as James suggested
#       esmx <- get.esmx(tmax)
#       ea <- (esmn * rhmax/100 + esmx * rhmin/100) / 2
#       return(ea)
#

temp_rh_plot_grp <-  temp_rh_data %>%
      filter(RH > 15, date != "2018-06-21") %>%
      group_by(date, status, logger_id, days) %>%
      summarise(avg_daily_tempC = mean(tempC),
                daily_max_tempC = max(tempC),
                daily_min_tempC = min(tempC),
                avg_daily_RH = mean(RH),
                daily_max_rh = max(RH),
                daily_min_rh = min(RH),
                daily_vpd = get.vpd(avg_daily_RH, avg_daily_tempC)
                )
# summary(temp_rh_plot_grp)
## Plot VPD and temperature values; try to understand the metric
# qplot(date, daily_vpd, data = filter(temp_rh_plot_grp, logger_id=="05" | logger_id=="17"), color = logger_id) +
#       geom_point(aes(y=avg_daily_tempC), shape = 1)


## slicing and summarizing humidity above below 80-82 ####

rh_below_80 <- temp_rh_data %>%
      filter(between(RH, 15, 80), date != "2018-06-21") %>%
      group_by(date, status, logger_id, days) %>%
      summarise(obs_below_80 = n(),
            rh_below_80_mins = obs_below_80*5) %>%
      select(-obs_below_80)
summary(rh_below_80)

rh_below_75 <- temp_rh_data %>%
      filter(between(RH, 15, 75), date != "2018-06-21") %>%
      group_by(date, status, logger_id, days) %>%
      summarise(obs_below_75 = n(),
                rh_below_75_mins = obs_below_75*5) %>%
      select(-obs_below_75)
  # ungroup(.) %>%
  # group_by(date, status, days) %>%
  # summarise(minutes_below_80 = minutes_below_80/1,
  #           mean_below_80 = mean(minutes_below_80),
  #           se_below_80 = sd(minutes_below_80)) %>%
  # select(date, status, days, minutes_below_80, mean_below_80, se_below_80)

rh_80_82 <- temp_rh_data %>%
      filter(between(RH, 80, 82), date != "2018-06-21") %>%
      group_by(date, status, logger_id, days) %>%
      summarise(obs_80_82 = n(),
            rh_80_82_mins = obs_80_82*5) %>%
  select(-obs_80_82)

rh_above_82 <- temp_rh_data %>%
  filter(RH > 82, date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_above_82 = n(),
            rh_above_82_mins = obs_above_82*5) %>%
      select(-obs_above_82)

humidity_minutes_by_logger <- list(
      rh_above_82, rh_80_82, rh_below_75, rh_below_80) %>%
      reduce(left_join) #wow, from purrr package, i've never used
# summary(humidity_minutes_by_logger)
humidity_minutes_by_logger[is.na(humidity_minutes_by_logger)] <- 0
write_csv(humidity_minutes_by_logger, "data/processed_data/rh_by_logger.csv")


temp_rh_plot_grp <- left_join(
      temp_rh_plot_grp,
      humidity_minutes_by_logger
)

summary(temp_rh_plot_grp)
# filter(temp_rh_plot_grp, is.na(rh_above_82_mins))$logger_id
## we get 357 NA for RH values. are these from logger failures?

temp_rh_plot_grp <- temp_rh_plot_grp %>%
      mutate(
            Treatment = ifelse(status=="invaded","Cogongrass","Native"),
            LocationID = case_when(
                  status=="invaded"~paste("C",as.integer(logger_id), sep=""),
                  status=="native"~paste("N",as.integer(logger_id), sep="")
            )
      )

# write_csv(temp_rh_plot_grp, "data/processed_data/tsa_temp_rh_plot_grp.csv")




test34 <- humidity_minutes_by_logger %>%
  group_by(status) %>%
  summarise(mean_above = mean(minutes_above_82),
            mean_bw = mean(minutes_bw_80_82),
            mean_below = mean(minutes_below_80))

humidity_means_se_treatment <- humidity_minutes_by_logger %>%
  group_by(date, status, days) %>%
  summarise(mean_above_82 = mean(minutes_above_82),
            se_above_82 = sd(minutes_above_82),
            mean_bw_80_82 = mean(minutes_bw_80_82),
            se_bw_80_82 = sd(minutes_bw_80_82),
            mean_below_80 = mean(minutes_below_80),
            se_below_80 = sd(minutes_below_80))

write_csv(humidity_minutes_by_logger, "data/processed_data/humidity_minutes_by_logger.csv")

write_csv(humidity_means_se_treatment, "data/processed_data/humidity_means_se_treatment.csv")

##### new testing for different thresholds ####
##### theme #####
def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_blank())
invasion_color <- scale_color_manual(values = c("red","deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red","deepskyblue"))

## test for 78

temp_rh_humidity_testing_below_78 <- temp_rh_data %>%
  filter(between(RH, 20, 78), date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_below_78 = n(),
            minutes_below_78 = obs_below_78*5) %>%
  select(date, status, days, logger_id, minutes_below_78)

temp_rh_humidity_testing_between_78_85 <- temp_rh_data %>%
  filter(between(RH, 78, 85), date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_bw_78_85 = n(),
            minutes_bw_78_85 = obs_bw_78_85*5) %>%
  select(date, status, logger_id, days, minutes_bw_78_85)

temp_rh_humidity_testing_above_85 <- temp_rh_data %>%
  filter(RH >85, date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5) %>%
  select(date, status, logger_id, days, minutes_above_85)

testing_78 <- left_join(temp_rh_humidity_testing_above_85, temp_rh_humidity_testing_between_78_85)
humidity_78 <- left_join(testing_78, temp_rh_humidity_testing_below_78)

humidity_78[is.na(humidity_78)] <- 0

write_csv(humidity_78, "data/processed_data/humidity_78.csv")

## test for 75


temp_rh_humidity_testing_below_75 <- temp_rh_data %>%
  filter(between(RH, 20, 74.99), date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5) %>%
  select(date, status, days, logger_id, minutes_below_75)

temp_rh_humidity_testing_between_75_85 <- temp_rh_data %>%
  filter(between(RH, 75, 85), date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_bw_75_85 = n(),
            minutes_bw_75_85 = obs_bw_75_85*5) %>%
  select(date, status, logger_id, days, minutes_bw_75_85)

temp_rh_humidity_testing_above_85 <- temp_rh_data %>%
  filter(RH >85.01, date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5) %>%
  select(date, status, logger_id, days, minutes_above_85)

testing_75 <- left_join(temp_rh_humidity_testing_above_85, temp_rh_humidity_testing_between_75_85)
humidity_75 <- left_join(testing_75, temp_rh_humidity_testing_below_75)

humidity_75[is.na(humidity_75)] <- 0
tail(humidity_75)
humidity_75 <- filter(humidity_75, minutes_above_85 <1441)
#View(weird)
write_csv(humidity_75, "data/processed_data/humidity_75.csv")
#humidity_75 <- filter(humidity_75, minutes_above_85 >1441)
#View(weird)

##### random ####
# p2 <- ggplot(temp_rh_data_grouped, aes(date, avg_dailymax_rh)) +
#       geom_smooth(aes(y = avg_daily_RH, fill = status, color = status), se = T, method = "lm", alpha = .2) +
#       geom_point(aes(color = status), size = 2) +
#       geom_point(data = temp_rh_data_grouped, aes(date, avg_dailymin_rh, color = status), shape = 25, size = 2) +
#       geom_hline(yintercept = 80, linetype = "dashed") +
#       invasion_color +
#       invasion_fill +
#       def_theme +
#       theme_classic() +
#       xlab("Date") +
#       ylab("avg daily RH") +
#       def_theme +
#       NULL
#
# temp_rh_data_grouped %>%
#       ungroup(.) %>%
#       filter(avg_dailymin_rh<80) %>%
#       group_by(status) %>%
#       summarise(days_minRH_blw80 = n())
#
# p1 <- ggplot(temp_rh_data_grouped, aes(date, avg_daily_tempC)) +
#       geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
#       geom_point(aes(color = status), size = 2) +
#       geom_point(data = temp_rh_data_grouped, aes(y=avg_dailymax_tempC, color = status), shape = 25, size = 2) +
#       geom_point(data = temp_rh_data_grouped, aes(y=avg_dailymin_tempC, color = status), shape = 24, size = 2) +
#       # geom_hline(yintercept = 80, linetype = "dashed") +
#       invasion_color +
#       invasion_fill +
#       def_theme +
#       theme_classic() +
#       # xlab("date") +
#       # ylab("Daily ") +
#       def_theme +
#       NULL
#
# cowplot::plot_grid(p1,p2, ncol = 1)
#
# ggplot(temp_rh_data_grouped, aes(avg_dailymax_tempC, avg_dailymin_rh)) +
#       geom_point(aes(color = status)) +
#       invasion_color +
#       def_theme

## Steven doing random stuffs ####

# ggplot(temp_rh_data_grouped, aes(days, avg_daily_RH)) +
#   geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
#   geom_point(aes(color = status)) +
#   invasion_color +
#   invasion_fill +
#   def_theme+
#   NULL

# #ggplot(temp_rh_data_grouped, aes(days, avg_daily_RH)) +
#   geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
#   geom_point(aes(color = status), size = 2) +
#   geom_point(data = temp_rh_data_grouped, aes(y = avg_max_rh, color = status), shape = 25, size = 2) +
#   geom_point(data = temp_rh_data_grouped, aes(y = avg_min_rh, color = status), shape = 24, stroke = 1.05) +
#   invasion_color +
#   invasion_fill +
#   def_theme +
#   NULL



####### thresholds min, above below 80 only for drew re-do #####

temp_rh_data

temp_rh_data_min <- temp_rh_data %>%
  mutate(days = julian(date, origin = as.Date("2018-06-21"))) %>% 
  group_by(date, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

temp_rh_data_below <- temp_rh_data %>% 
  mutate(days = julian(date, origin = as.Date("2018-06-21"))) %>% 
  filter(RH < 79.99) %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, status, logger_id, days, hours_below_80)

temp_rh_data_above <- temp_rh_data %>%
  mutate(days = julian(date, origin = as.Date("2018-06-21"))) %>% 
  filter(RH >80.01) %>%
  group_by(date, status, logger_id, days) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, status, logger_id, days, hours_above_80)

new_thresholds_1 <- left_join(temp_rh_data_min, temp_rh_data_above)
new_thresholds <- left_join(new_thresholds_1, temp_rh_data_below)
View(new_thresholds)
#weird <- merge(temp_rh_data_below, temp_rh_data_above)

new_thresholds[is.na(new_thresholds)] <- 0

write_csv(new_thresholds, "data/processed_data/tsa_above_below_80_all_time.csv")
