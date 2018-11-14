#' # Script for doing QA/QC on tick assay temp/RH logger data

library(plyr)
library(tidyverse)
# library(dplyr)
# library(readr)
# library(ggplot2)
library(stringi)

#+ load logger data ####
temp_rh_data <- read_csv("data/processed_data/tick_survival_combined_temperature_rh.csv")

summary(temp_rh_data)

names(temp_rh_data)

temp_rh_data <- temp_rh_data %>%
  mutate(status = ifelse(logger_id <= 12, "invaded", "native"))

#### Added invaded/native status ####

temp_rh_data_invaded <- temp_rh_data %>%
  filter(status =="invaded")

temp_rh_data_native <- temp_rh_data %>%
  filter(status =="native")

#### Created seperate dataframes for native and invaded ####

summary(temp_rh_data_invaded)
summary(temp_rh_data_native)

temp_rh_data_invaded_filtered <- temp_rh_data_invaded %>%
  filter(RH > 15)

summary(temp_rh_data_invaded)
summary(temp_rh_data_invaded_filtered)

#### invaded 92.16 avg RH all included, 94.65 avg RH excluding < 15 RH ####

temp_rh_data_native_filtered <- temp_rh_data_native %>%
  filter(RH > 15)

summary(temp_rh_data_native)
summary(temp_rh_data_native_filtered)

#### native 92.89 avg RH all included, 93.38 avg RH excluding < 15 RH ####

messed_up_loggers <- temp_rh_data %>%
  filter(RH < 15)

summary(messed_up_loggers)
unique(messed_up_loggers$logger_id)

#### RH < 10 at loggers 1, 2, and 19. Total of 13353 data points ####

#### Moves to loggeres 1, 2, 9, and 19 with RH < 15. Total of 14042 data points ####

#### Logger 1 processing

logger_1 <- temp_rh_data %>%
  filter(logger_id == "01")

messed_up_logger_1 <- messed_up_loggers %>%
  filter(logger_id == "01")

summary(logger_1)
summary(messed_up_logger_1)

logger_1 <- logger_1 %>%
  filter(RH > 15)

summary(logger_1)

#### logger 1 jumps from 85.17 to 88.21 avg RH when removing < 15 RH

#### Logger 2 processing

logger_2 <- temp_rh_data %>%
  filter(logger_id == "02")

messed_up_logger_2 <- messed_up_loggers %>%
  filter(logger_id == "02")

summary(logger_2)
summary(messed_up_logger_2)

logger_2 <- logger_2 %>%
  filter(RH > 15)

summary(logger_2)

#### logger 2 is most messed up, avg RH jumps from 64.82 to 90.15 when removing < 15 ####

#### Logger 9 processing ####

logger_9 <- temp_rh_data %>%
  filter(logger_id == "09")

messed_up_logger_9 <- messed_up_loggers %>%
  filter(logger_id == "09")

summary(logger_9)
summary(messed_up_logger_9)

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

## logger 19 avg RH jump from 72.72 to 77.57 removing < 15 RH ##


temp_rh_data <- temp_rh_data %>%
      mutate(date_time = lubridate::ymd_hms(paste(date,time)))

# ggplot(temp_rh_data, aes(date_time, RH)) +
      # geom_point(aes(color = as.numeric(logger_id), shape = status), alpha = .2)

## Summary values for each logger ####

temp_rh_data %>%
      group_by(logger_id, status) %>%
      summarise(avg_rh = mean(RH),
                max_rh = max(RH),
                min_rh = min(RH)) %>%
      # View(.)
      filter(min_rh>1) %>%
      summary(.)
## suggests that values below ~12% RH should be strongly questioned



# lubridate::ymd_hms(paste(temp_rh_data$date, temp_rh_data$time))

#### Attempting to average temp and RH per treatment PER DAY ####

## should be 435,392 data points and 127 dates ##

temp_rh_data_grouped <- temp_rh_data %>%
      filter(RH >= 20, date != "2018-06-21") %>%
      group_by(date, status, logger_id) %>%
      summarise(avg_daily_tempC = mean(tempC),
            max_tempC = max(tempC),
            min_tempC = min(tempC),
            avg_daily_RH = mean(RH),
            avg_max_rh = max(RH),
            avg_min_rh = min(RH)
            ) %>%
      ungroup(.) %>%
      group_by(date, status) %>%
      summarise(avg_daily_tempC = mean(avg_daily_tempC),
            avg_min_tempC = mean(min_tempC),
            avg_max_tempC = mean(max_tempC),
            avg_daily_RH = mean(avg_daily_RH),
            avg_max_rh = mean(avg_max_rh),
            avg_min_rh = mean(avg_min_rh)
            )

# View(temp_rh_data_grouped)

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank())
invasion_color <- scale_color_viridis_d()
invasion_fill <- scale_fill_viridis_d()


ggplot(temp_rh_data_grouped, aes(date, avg_max_rh)) +
      geom_smooth(aes(y = avg_daily_RH, fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status), size = 2) +
      geom_point(data = temp_rh_data_grouped, aes(date, avg_min_rh, color = status), shape = 25, size = 2) +
      geom_hline(yintercept = 80, linetype = "dashed") +
      invasion_color +
      invasion_fill +
      def_theme +
      theme_classic() +
      xlab("date") +
      ylab("daily RH") +
      def_theme +
      NULL

temp_rh_data_grouped %>%
      ungroup(.) %>%
      filter(avg_min_rh<80) %>%
      group_by(status) %>%
      summarise(days_minRH_blw80 = n())

