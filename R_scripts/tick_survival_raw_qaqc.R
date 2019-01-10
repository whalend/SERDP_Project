#### Script for doing QA/QC on tick survival raw tick data numbers ####

library(plyr)
library(tidyverse)
# library(dplyr)
# library(readr)
# library(ggplot2)
library(stringi)
library(ggplot2)

#+ load tick survival data ####

tick_survival <- read_csv("data/raw_data/tick_survival_assay/TickSurvivaldata_CleanedVersion.csv")

summary(tick_survival)

unique(tick_survival$Date)

names(tick_survival)

unique(tick_survival$Invaded)

tick_survival$Date <- as.Date.character(as.integer(tick_survival$Date), format = "%Y%m%d")

tick_survival <- tick_survival %>%
  mutate(days = julian(Date, origin = as.Date("2018-06-21")))

colnames(tick_survival) <- c("LocationID", "Tag", "Invaded", "Date", "JulianDay", "nymphs_alive", "nymphs_dead", "nymph_survival", "adult_F_alive", "adult_F_dead", "adult_F_survival", "adult_M_alive", "adult_M_dead", "adult_M_survival", "Avg CC", "NOTES", "days")

range(tick_survival$days)

tick_survival_long <- tick_survival %>% 
  select(LocationID:JulianDay, days, nymph_survival, adult_F_survival, adult_M_survival) %>% 
  gather(., key = life_stage, value = survival, -LocationID:-days) %>% 
  mutate(Invaded = ifelse(Invaded == "Yes", "invaded", "native"))
summary(tick_survival_long)

  

#### Merging all adults to one ####

tick_survival <- tick_survival %>%
  mutate(total_adults_alive = adult_F_alive + adult_M_alive,
         total_adults_dead = adult_F_dead + adult_M_dead,
         total_adult_survival = total_adults_alive/(total_adults_alive + total_adults_dead))

tick_survival <- tick_survival %>%
  mutate(Invaded = ifelse(Invaded == "Yes", "invaded", "native"))

## adding visit number, ADD NEW VISITS EACH TIME

days_adding_visit_number <- data.frame(days = unique(tick_survival$days), visit_number= as.double(seq(1,17,1))) 

tick_survival <- left_join(tick_survival, days_adding_visit_number)

#View(tick_survival)

tick_survival_grouped <- tick_survival %>%
  group_by(visit_number, Invaded, days) %>%
  summarise(avg_nymph_survival = mean(nymph_survival),
            sd_nymph_survival = sd(nymph_survival),
            avg_adult_survival = mean(total_adult_survival),
            sd_adult_survival = sd(total_adult_survival))

#View(tick_survival_grouped)


#+ set theme ####

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_blank())
invasion_color <- scale_color_manual(values = c("red","blue"))
invasion_fill <- scale_color_manual(values = c("red","blue"))

ggplot(tick_survival_grouped, aes(days, avg_nymph_survival, color = Invaded)) +
  geom_smooth(aes(fill = Invaded), show.legend = F, alpha = .2) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_nymph_survival - sd_nymph_survival, ymax = avg_nymph_survival + sd_nymph_survival, color = Invaded), width = .1) +
  invasion_color +
  invasion_fill +
  theme_bw() +
  def_theme +
  NULL

ggplot(tick_survival_grouped, aes(days, avg_adult_survival, color = Invaded)) +
  geom_smooth(aes(fill = Invaded, color = Invaded), se = T, alpha = .2) +
  geom_point() +
  invasion_color +
  invasion_fill +
  def_theme +
  NULL

unique(tick_survival$days)
#### Drews graphs ####

nymph_survival_all_time <- ggplot(data = tick_survival) +
  stat_summary(aes(days, nymph_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, nymph_survival, fill = Invaded, color = Invaded),
              se = T, alpha = .2, geom = "path") +
  invasion_color +
  invasion_fill +
  def_theme +
  #theme_classic() +
  ggtitle(label = "Nymphs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(" ") +
  ylab("Survival") +
  guides(fill=FALSE, color=FALSE) +
  scale_y_continuous(limits = c(0, 1 )) +
  scale_x_continuous(limits = c(0, 202)) +
  geom_vline(xintercept = (11), linetype = "dashed") +
  geom_vline(xintercept = (15), linetype = "dashed") +
  geom_vline(xintercept = (25), linetype = "dashed") +
  geom_vline(xintercept = (32), linetype = "dashed") +
  geom_vline(xintercept = (39), linetype = "dashed") +
  geom_vline(xintercept = (55), linetype = "dashed") +
  geom_vline(xintercept = (67), linetype = "dashed") +
  geom_vline(xintercept = (77), linetype = "dashed") +
  geom_vline(xintercept = (89), linetype = "dashed") +
  geom_vline(xintercept = (98), linetype = "dashed") +
  geom_vline(xintercept = (110), linetype = "dashed") +
  geom_vline(xintercept = (126), linetype = "dashed") +
  geom_vline(xintercept = (146), linetype = "dashed") +
  geom_vline(xintercept = (159), linetype = "dashed") +
  geom_vline(xintercept = (175), linetype = "dashed") +
  geom_vline(xintercept = (202), linetype = "dashed") +
  #theme(axis.title.x = element_blank()) +
  NULL

f_adult_survival_all_time <-ggplot(data = tick_survival) +
  stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_F_survival, color = Invaded, fill = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Female Adults") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE, color=FALSE) +
  #theme(legend.position = c(0, 0), legend.justification = c(0, 0)) +
  #theme(axis.title.y = element_blank()) +
  scale_y_continuous(limits = c(0, 1 )) +
  scale_x_continuous(limits = c(0, 202)) +
  ylab("Survival") +
  xlab(" ") +
  geom_vline(xintercept = (11), linetype = "dashed") +
  geom_vline(xintercept = (15), linetype = "dashed") +
  geom_vline(xintercept = (25), linetype = "dashed") +
  geom_vline(xintercept = (32), linetype = "dashed") +
  geom_vline(xintercept = (39), linetype = "dashed") +
  geom_vline(xintercept = (55), linetype = "dashed") +
  geom_vline(xintercept = (67), linetype = "dashed") +
  geom_vline(xintercept = (77), linetype = "dashed") +
  geom_vline(xintercept = (89), linetype = "dashed") +
  geom_vline(xintercept = (98), linetype = "dashed") +
  geom_vline(xintercept = (110), linetype = "dashed") +
  geom_vline(xintercept = (126), linetype = "dashed") +
  geom_vline(xintercept = (146), linetype = "dashed") +
  geom_vline(xintercept = (159), linetype = "dashed") +
  geom_vline(xintercept = (175), linetype = "dashed") +
  geom_vline(xintercept = (202), linetype = "dashed") +
  NULL

m_adult_survival_all_time <-ggplot(data = tick_survival) +
  stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_M_survival, color = Invaded, fill = Invaded),
              se = T, alpha = .2) +
  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Male Adults") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE, color=FALSE) +
  scale_y_continuous(limits = c(0, 1 )) +
  scale_x_continuous(limits = c(0, 202)) +
  xlab(" ") +
  ylab("Survival") +
  #theme(axis.title.x = element_blank()) +
  #theme(axis.title.y = element_blank()) +
  geom_vline(xintercept = (11), linetype = "dashed") +
  geom_vline(xintercept = (15), linetype = "dashed") +
  geom_vline(xintercept = (25), linetype = "dashed") +
  geom_vline(xintercept = (32), linetype = "dashed") +
  geom_vline(xintercept = (39), linetype = "dashed") +
  geom_vline(xintercept = (55), linetype = "dashed") +
  geom_vline(xintercept = (67), linetype = "dashed") +
  geom_vline(xintercept = (77), linetype = "dashed") +
  geom_vline(xintercept = (89), linetype = "dashed") +
  geom_vline(xintercept = (98), linetype = "dashed") +
  geom_vline(xintercept = (110), linetype = "dashed") +
  geom_vline(xintercept = (126), linetype = "dashed") +
  geom_vline(xintercept = (146), linetype = "dashed") +
  geom_vline(xintercept = (159), linetype = "dashed") +
  geom_vline(xintercept = (175), linetype = "dashed") +
  geom_vline(xintercept = (202), linetype = "dashed") +
  NULL

tick_survival_all_time <- cowplot::plot_grid(nymph_survival_all_time,
                   f_adult_survival_all_time, m_adult_survival_all_time, ncol = 3)

#### making stacked all life stages ######

all_stages_stacked <-ggplot(data = tick_survival_long) +
  stat_summary(aes(days, survival, color = Invaded, shape = life_stage),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, survival, color = Invaded, linetype = life_stage),
               se = T, alpha = .2, fill = NA) +
  # stat_summary(aes(days, adult_F_survival,  color = Invaded),
  #              fun.data = mean_se, geom = "pointrange", shape = 22) +
  # stat_smooth(aes(days, adult_F_survival, color = Invaded),
  #             se = T, alpha = .2) +
  # stat_summary(aes(days, adult_M_survival, color = Invaded),
  #              fun.data = mean_se, geom = "pointrange", shape = 23) +
  # stat_smooth(aes(days, adult_M_survival, color = Invaded),
  #             se = T, alpha = .2) +
  invasion_color +
  invasion_fill +
  def_theme +
  theme_bw() +
  ggtitle(label = "Tick Survival") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #guides(fill=FALSE, color=FALSE) +
  scale_y_continuous(limits = c(0, 1 )) +
  scale_x_continuous(limits = c(0, 202)) +
  xlab("Days") +
  ylab("Survival") +
  NULL


#ggsave(plot = all_stages_stacked, "figures/tick-survival-assay/all_stages_stacked.png", height = 7, width = 7)

#ggsave(plot = tick_survival_all_time, "figures/tick-survival-assay/tick_survival_all_time.png")

## Begin break up by 40 day increments ####

tick_survival_40 <- tick_survival %>%
  filter(days <=66)

tick_survival_80 <- tick_survival %>%
  filter(between(days, 67, 132))

tick_survival_120 <- tick_survival %>%
  filter(between(days, 133, 202))


# nymphs by days interval NOT USED PROBABLY ####

nymph_survival_40 <- ggplot(data = tick_survival_40) +
  stat_summary(aes(days, nymph_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, nymph_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Nymphs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 66) +
  ylim(0, 1) +
  xlab("Days") +
  ylab("Survival") +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold")) +
  #theme(axis.title.x = element_blank()) +
  NULL

nymph_survival_80 <- ggplot(data = tick_survival_80) +
  stat_summary(aes(days, nymph_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, nymph_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Nymphs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(67, 132) +
  ylim(0, 1) +
  xlab("Days") +
  #ylab("Survival") +
  theme(legend.position = "none") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=12, face = "bold")) +
  NULL

nymph_survival_120 <- ggplot(data = tick_survival_120) +
  stat_summary(aes(days, nymph_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, nymph_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Nymphs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(133, 202) +
  ylim(0, 1) +
  xlab("Days") +
  #ylab("Survival") +
  theme(legend.position = "none") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=12, face = "bold")) +
  NULL

nymphs_intervals <- cowplot::plot_grid(nymph_survival_40,
                   nymph_survival_80, nymph_survival_120, ncol =3)

# adult F by days NOT USED ####

adult_F_survival_40 <- ggplot(data = tick_survival_40) +
  stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_F_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Adult Female") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 40) +
  ylim(0, 1) +
  xlab("Days") +
  ylab("Survival") +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold")) +
  #theme(axis.title.x = element_blank()) +
  NULL

adult_F_survival_80 <- ggplot(data = tick_survival_80) +
  stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_F_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Adult Female") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(40, 80) +
  ylim(0, 1) +
  xlab("Days") +
  #ylab("Survival") +
  theme(legend.position = "none") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=12, face = "bold")) +
  NULL

adult_F_survival_120 <- ggplot(data = tick_survival_120) +
  stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_F_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Adult Female") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(80, 126) +
  ylim(0, 1) +
  xlab("Days") +
  #ylab("Survival") +
  theme(legend.position = "none") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=12, face = "bold")) +
  NULL

adult_F_intervals <- cowplot::plot_grid(adult_F_survival_40,
                   adult_F_survival_80, adult_F_survival_120, ncol =3)


# adult M by days NOT USED####

adult_M_survival_40 <- ggplot(data = tick_survival_40) +
  stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_M_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Adult Male") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 40) +
  ylim(0, 1) +
  xlab("Days") +
  ylab("Survival") +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold")) +
  #theme(axis.title.x = element_blank()) +
  NULL

adult_M_survival_80 <- ggplot(data = tick_survival_80) +
  stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_M_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Adult Male") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(40, 80) +
  ylim(0, 1) +
  xlab("Days") +
  #ylab("Survival") +
  theme(legend.position = "none") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=12, face = "bold")) +
  NULL

adult_M_survival_120 <- ggplot(data = tick_survival_120) +
  stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
               fun.data = mean_se, geom = "pointrange") +
  stat_smooth(aes(days, adult_M_survival, color = Invaded),
              se = T, alpha = .2) +  invasion_color +
  invasion_fill +
  def_theme +
  ggtitle(label = "Adult Male") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(80, 126) +
  ylim(0, 1) +
  xlab("Days") +
  #ylab("Survival") +
  theme(legend.position = "none") +
  #theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=12, face = "bold")) +
  NULL

adult_M_intervals <- cowplot::plot_grid(adult_M_survival_40,
                   adult_M_survival_80, adult_M_survival_120, ncol =3)


#+ IMPORT ALL OF TEMP RH DATA SCRIPT ####

#' # Script for doing QA/QC on tick assay temp/RH logger data

#library(plyr)
#library(tidyverse)
# library(dplyr)
# library(readr)
# library(ggplot2)
#library(stringi)

#+ load logger data ####
temp_rh_data <- read_csv("data/processed_data/tick_survival_combined_temperature_rh.csv")

summary(temp_rh_data)

names(temp_rh_data)

temp_rh_data <- temp_rh_data %>%
  mutate(status = ifelse(logger_id <= 12, "invaded", "native"))

temp_rh_data$logger_id <- as.integer(temp_rh_data$logger_id)

join(temp_rh_data, days_adding_visit_number)



#### Added invaded/native status and days since launch ####

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

temp_rh_data <- temp_rh_data %>%
  mutate(days = julian(date, origin = as.Date("2018-06-21")))

filter(temp_rh_data, date == "2018-10-25") %>%
  select(date, days)

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

## Attempting to average temp and RH per treatment PER DAY ####

## should be 435,392 data points and 127 dates ##

temp_rh_data_grouped <- temp_rh_data %>%
  filter(RH >= 20, date != "2018-06-21") %>%
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
            avg_min_tempC = mean(min_tempC),
            avg_max_tempC = mean(max_tempC),
            avg_daily_RH = mean(avg_daily_RH),
            avg_max_rh = mean(avg_max_rh),
            avg_min_rh = mean(avg_min_rh)
  )

## Added days since launch ####
summary(temp_rh_data_grouped)

#View(temp_rh_data_grouped)

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank())
invasion_color <- scale_color_manual(values = c("red","blue"))
invasion_fill <- scale_color_manual(values = c("red","blue"))

p2 <- ggplot(temp_rh_data_grouped, aes(date, avg_max_rh)) +
  geom_smooth(aes(y = avg_daily_RH, fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status), size = 2) +
  geom_point(data = temp_rh_data_grouped, aes(date, avg_min_rh, color = status), shape = 25, size = 2) +
  geom_hline(yintercept = 80, linetype = "dashed") +
  invasion_color +
  invasion_fill +
  def_theme +
  theme_classic() +
  # xlab("date") +
  ylab("avg daily RH") +
  def_theme +
  NULL

temp_rh_data_grouped %>%
  ungroup(.) %>%
  filter(avg_min_rh<80) %>%
  group_by(status) %>%
  summarise(days_minRH_blw80 = n())

p1 <- ggplot(temp_rh_data_grouped, aes(date, avg_daily_tempC)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status), size = 2) +
  geom_point(data = temp_rh_data_grouped, aes(y=avg_max_tempC, color = status), shape = 25, size = 2) +
  geom_point(data = temp_rh_data_grouped, aes(y=avg_min_tempC, color = status), shape = 24, size = 2) +
  # geom_hline(yintercept = 80, linetype = "dashed") +
  invasion_color +
  invasion_fill +
  def_theme +
  theme_classic() +
  # xlab("date") +
  # ylab("Daily ") +
  def_theme +
  NULL

cowplot::plot_grid(p1,p2, ncol = 1)

ggplot(temp_rh_data_grouped, aes(avg_max_tempC, avg_min_rh)) +
  geom_point(aes(color = status))

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

#### figure for max temps all time all days ####

scaleFUN <- function(x) sprintf("%.1f", x)

max_temp_all_time <- ggplot(temp_rh_data_grouped, aes(days, avg_max_tempC, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_hline(yintercept = 35, linetype = "dashed") +
  geom_vline(xintercept = (11), linetype = "dashed") +
  geom_vline(xintercept = (15), linetype = "dashed") +
  geom_vline(xintercept = (25), linetype = "dashed") +
  geom_vline(xintercept = (32), linetype = "dashed") +
  geom_vline(xintercept = (39), linetype = "dashed") +
  geom_vline(xintercept = (55), linetype = "dashed") +
  geom_vline(xintercept = (67), linetype = "dashed") +
  geom_vline(xintercept = (77), linetype = "dashed") +
  geom_vline(xintercept = (89), linetype = "dashed") +
  geom_vline(xintercept = (98), linetype = "dashed") +
  geom_vline(xintercept = (110), linetype = "dashed") +
  geom_vline(xintercept = (126), linetype = "dashed") +
  geom_vline(xintercept = (146), linetype = "dashed") +
  geom_vline(xintercept = (159), linetype = "dashed") +
  geom_vline(xintercept = (175), linetype = "dashed") +
  geom_vline(xintercept = (202), linetype = "dashed") +
  geom_point(data = temp_rh_data_grouped, aes(y = avg_max_tempC, color = status), shape = 25) +
  invasion_color +
  invasion_fill +
  ylab("Avg daily max temp C") +
  xlab(" ") +
  scale_y_continuous(labels = scaleFUN) +
  scale_x_continuous(limits = c(0, 202)) +
  guides(fill=FALSE, color=FALSE) +
  def_theme +
  NULL

#### figure for min rhs all time all days ####

min_rh_all_time <- ggplot(temp_rh_data_grouped, aes(days, avg_min_rh, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_vline(xintercept = (11), linetype = "dashed") +
  geom_vline(xintercept = (15), linetype = "dashed") +
  geom_vline(xintercept = (25), linetype = "dashed") +
  geom_vline(xintercept = (32), linetype = "dashed") +
  geom_vline(xintercept = (39), linetype = "dashed") +
  geom_vline(xintercept = (55), linetype = "dashed") +
  geom_vline(xintercept = (67), linetype = "dashed") +
  geom_vline(xintercept = (77), linetype = "dashed") +
  geom_vline(xintercept = (89), linetype = "dashed") +
  geom_vline(xintercept = (98), linetype = "dashed") +
  geom_vline(xintercept = (110), linetype = "dashed") +
  geom_vline(xintercept = (126), linetype = "dashed") +
  geom_vline(xintercept = (146), linetype = "dashed") +
  geom_vline(xintercept = (159), linetype = "dashed") +
  geom_vline(xintercept = (175), linetype = "dashed") +
  geom_vline(xintercept = (202), linetype = "dashed") +
  geom_point(data = temp_rh_data_grouped, aes(y = avg_min_rh, color = status), shape = 25) +
  invasion_color +
  invasion_fill +
  ylab("Avg daily min RH") +
  xlab("Days") +
  scale_x_continuous(limits = c(0, 202)) +
  guides(fill=FALSE, color=FALSE) +
  def_theme +
  NULL

nymphs_temp_rh_all_time <- cowplot::plot_grid(nymph_survival_all_time,
                   max_temp_all_time, min_rh_all_time, ncol = 1)
m_adult_temp_rh_all_time <- cowplot::plot_grid(m_adult_survival_all_time,
                   max_temp_all_time, min_rh_all_time, ncol = 1)
f_adult_temp_rh_all_time <- cowplot::plot_grid(f_adult_survival_all_time,
                   max_temp_all_time, min_rh_all_time, ncol = 1)

ggsave(plot = nymphs_temp_rh_all_time, height = 10, width = 7, "figures/tick-survival-assay/nymphs_temp_rh_all_time.png")
ggsave(plot = m_adult_temp_rh_all_time, height = 10, width = 7,"figures/tick-survival-assay/m_adult_temp_rh_all_time.png")
ggsave(plot = f_adult_temp_rh_all_time, height = 10, width = 7, "figures/tick-survival-assay/f_adult_temp_rh_all_time.png")

##################### sep by days

temp_rh_survival1 <- temp_rh_data_grouped %>%
  filter(between(days, 1, 66))
temp_rh_survival2 <- temp_rh_data_grouped %>%
  filter(between(days, 67, 132))
temp_rh_survival3 <- temp_rh_data_grouped %>%
  filter(between(days, 133, 202))

## Figures for day 1-126 intervals for RH ####

surv1_rh <- ggplot(temp_rh_survival1, aes(days, avg_daily_RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_vline(xintercept = (11), linetype = "dashed") +
  geom_vline(xintercept = (15), linetype = "dashed") +
  geom_vline(xintercept = (25), linetype = "dashed") +
  geom_vline(xintercept = (32), linetype = "dashed") +
  geom_vline(xintercept = (39), linetype = "dashed") +
  geom_vline(xintercept = (55), linetype = "dashed") +
  geom_point(data = temp_rh_survival1, aes(y = avg_min_rh, color = status), shape = 25) +
  #geom_point(data = temp_rh_survival1, aes(y = avg_max_rh, color = status), shape = 24) +
  invasion_color +
  invasion_fill +
  ylab("Average daily RH") +
  xlab("Days") +
  theme(legend.position = "none") +
  def_theme +
  ylim(50,100) +
  xlim(0, 66) +
  NULL

surv2_rh <- ggplot(temp_rh_survival2, aes(days, avg_daily_RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_vline(xintercept = (67), linetype = "dashed") +
  geom_vline(xintercept = (77), linetype = "dashed") +
  geom_vline(xintercept = (89), linetype = "dashed") +
  geom_vline(xintercept = (98), linetype = "dashed") +
  geom_vline(xintercept = (110), linetype = "dashed") +
  geom_vline(xintercept = (126), linetype = "dashed") +
  geom_point(data = temp_rh_survival2, aes(y = avg_min_rh, color = status), shape = 25) +
  #geom_point(data = temp_rh_survival2, aes(y = avg_max_rh, color = status), shape = 24) +
  invasion_color +
  invasion_fill +
  xlab("Days") +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  def_theme +
  ylim(50,100) +
  xlim(67, 132) +
  NULL

surv3_rh <- ggplot(temp_rh_survival3, aes(days, avg_daily_RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_vline(xintercept = (146), linetype = "dashed") +
  geom_vline(xintercept = (159), linetype = "dashed") +
  geom_vline(xintercept = (175), linetype = "dashed") +
  geom_vline(xintercept = (202), linetype = "dashed") +
  geom_point(data = temp_rh_survival3, aes(y = avg_min_rh, color = status), shape = 25) +
  #geom_point(data = temp_rh_survival3, aes(y = avg_max_rh, color = status), shape = 24) +
  invasion_color +
  invasion_fill +
  xlab("Days") +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  def_theme +
  ylim(50,100) +
  xlim(133, 202) +
  NULL

## Figures for day 1-126 intervals for TEMP ####

surv1_temp <- ggplot(temp_rh_survival1, aes(days, avg_daily_tempC, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_point(data = temp_rh_survival1, aes(y = avg_min_tempC, color = status), shape = 25) +
  geom_point(data = temp_rh_survival1, aes(y = avg_max_tempC, color = status), shape = 24) +
  invasion_color +
  invasion_fill +
  geom_hline(yintercept = 35, linetype = "dashed") +
  geom_vline(xintercept = (11), linetype = "dashed") +
  geom_vline(xintercept = (15), linetype = "dashed") +
  geom_vline(xintercept = (25), linetype = "dashed") +
  geom_vline(xintercept = (32), linetype = "dashed") +
  geom_vline(xintercept = (39), linetype = "dashed") +
  geom_vline(xintercept = (55), linetype = "dashed") +
  xlab("Days") +
  ylab("Average daily temp C") +
  theme(legend.position = "none") +
  def_theme +
  ylim(5,45) +
  xlim(0, 66) +
  NULL

surv2_temp <- ggplot(temp_rh_survival2, aes(days, avg_daily_tempC, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_point(data = temp_rh_survival2, aes(y = avg_min_tempC, color = status), shape = 25) +
  geom_point(data = temp_rh_survival2, aes(y = avg_max_tempC, color = status), shape = 24) +
  invasion_color +
  invasion_fill +
  geom_hline(yintercept = 35, linetype = "dashed") +
  geom_vline(xintercept = (67), linetype = "dashed") +
  geom_vline(xintercept = (77), linetype = "dashed") +
  geom_vline(xintercept = (89), linetype = "dashed") +
  geom_vline(xintercept = (98), linetype = "dashed") +
  geom_vline(xintercept = (110), linetype = "dashed") +
  geom_vline(xintercept = (126), linetype = "dashed") +
  xlab("Days") +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  def_theme +
  ylim(5,45) +
  xlim(67, 132) +
  NULL

surv3_temp <- ggplot(temp_rh_survival3, aes(days, avg_daily_tempC, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_point(data = temp_rh_survival3, aes(y = avg_min_tempC, color = status), shape = 25) +
  geom_point(data = temp_rh_survival3, aes(y = avg_max_tempC, color = status), shape = 24) +
  invasion_color +
  invasion_fill +
  geom_hline(yintercept = 35, linetype = "dashed") +
  geom_vline(xintercept = (146), linetype = "dashed") +
  geom_vline(xintercept = (159), linetype = "dashed") +
  geom_vline(xintercept = (175), linetype = "dashed") +
  geom_vline(xintercept = (202), linetype = "dashed") +
  xlab("Days") +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  def_theme +
  ylim(5,45) +
  xlim(133, 202) +
  NULL

#surv1_temp_rh <- ggplot(temp_rh_survival1, aes(days, avg_min_rh, color = status)) +
#geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
#geom_point(aes(color = status)) +
#scale_y_continuous("avg_min_rh", sec.axis = sec_axis(temp_rh_survival1, name = "avg_max_tempC")) +
#invasion_color +
#invasion_fill +
#def_theme +
#NULL

avg_min_rhs_days_intervals <- cowplot::plot_grid(surv1_rh, surv2_rh, surv3_rh, ncol = 3)
#ggsave(plot = avg_min_rhs_days_intervals, "figures/tick-survival-assay/avg_min_rhs_days_intervals.png", height = 10, width = 7)

avg_max_temps_days_intervals <- cowplot::plot_grid(surv1_temp, surv2_temp, surv3_temp, ncol = 3)
#ggsave(plot = avg_max_temps_days_intervals, "figures/tick-survival-assay/avg_max_temps_days_intervals.png", height = 10, width =7)

## plots for first check up date 14 days ####

temp_rh_surv_first_check <- temp_rh_data_grouped %>%
  filter(days <= 14)

surv_first_check_rh <- ggplot(temp_rh_surv_first_check, aes(days, avg_min_rh, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = 0) +
  geom_point(aes(color = status)) +
  invasion_color +
  invasion_fill +
  def_theme +
  NULL

surv_first_check_temp <- ggplot(temp_rh_surv_first_check, aes(days, avg_max_tempC, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = 0) +
  geom_point(aes(color = status)) +
  invasion_color +
  invasion_fill +
  def_theme +
  NULL

max_temp_min_rh_first_check <- cowplot::plot_grid(surv_first_check_rh, surv_first_check_temp, ncol = 2)
#ggsave(plot = max_temp_min_rh_first_check, "figures/tick-survival-assay/max_temp_min_rh_first_check.png")

## grouping by individual date time points attempt QUESTION ####
# only one temp/rh measurement taken per timepoint, no min/max unless you take min/max between 12 loggers of same treatment?

# #temp_rh_data_timepoints <- temp_rh_data %>%
#   group_by(time, status, logger_id, days) %>%
#   summarise(avg_daily_tempC = mean(tempC),
#             avg_daily_RH = mean(RH)
#             ) %>%
#   ungroup(.) %>%
#   group_by(time, status, days, logger_id) %>%
#   summarise(avg_daily_tempC = mean(avg_daily_tempC),
#             avg_daily_RH = mean(avg_daily_RH)
#   )

## Begin processing for time above/below data ####

rh_below_80 <- temp_rh_data %>%
  filter(between(RH, 20, 80)) %>%
  #filter(RH <= 80) %>%
  group_by(status, logger_id) %>%
  summarise(obs = n(),
            hrs = obs/12,
            days = hrs/24)

#View(rh_below_80)

rh_below_80_avg <- temp_rh_data %>%
  filter(between(RH, 20, 80)) %>%
  #filter(RH <= 80) %>%
  group_by(status, logger_id) %>%
  summarise(obs = n(),
            hrs = obs/12,
            days = hrs/24) %>%
  ungroup(.) %>%
  group_by(status) %>%
  summarise(avg_days = mean(days),
            sd = sd(days))

temp_above_35 <- temp_rh_data %>%
  filter(tempC >= 35) %>%
  group_by(status, logger_id) %>%
  summarise(obs = n(),
            hrs = obs/12,
            days = hrs/24)

temp_above_35_avg <- temp_rh_data %>%
  filter(tempC >= 35) %>%
  group_by(status, logger_id) %>%
  summarise(obs = n(),
            hrs = obs/12,
            days = hrs/24) %>%
  ungroup(.) %>%
  group_by(status) %>%
  summarise(avg_days = mean(days),
            sd = sd(days))

#View(temp_above_35)

avg_days_below_80rh <- ggplot(rh_below_80_avg, aes(status, avg_days, color = status)) +
  geom_point() +
  geom_bar(stat = "identity") +
  invasion_color +
  invasion_fill +
  #xlab("Invasion status") +
  ylab("Average time below 80% RH (days)") +
  def_theme +
  theme(legend.position = "none") +
  NULL

avg_days_above_35_tempC <- ggplot(temp_above_35_avg, aes(status, avg_days, color = status)) +
  geom_point() +
  geom_bar(stat = "identity") +
  #geom_errorbar(data = temp_above_35_avg,
  #aes(ymin=avg_days_above_35_tempC - sd,
  #ymax=avg_days_above_35_tempC + sd)) +
  invasion_color +
  invasion_fill +
  #xlab("Invasion status") +
  ylab("Average time above 35 C (days)") +
  def_theme +
  theme(legend.position = "none") +
  NULL

avg_time_days_temp_rh_barchart <- cowplot::plot_grid(avg_days_below_80rh, avg_days_above_35_tempC, ncol = 2)

#ggsave(plot = avg_time_days_temp_rh_barchart, "figures/tick-survival-assay/avg_time_days_temp_rh_barchart.png")

avg_days_rh_boxplot <- ggplot(rh_below_80, aes(status, days, color = status)) +
  geom_boxplot() +
  invasion_color +
  invasion_fill +
  def_theme +
  ylab("time below 80 rh (days)") +
  theme(legend.position = "none") +
  NULL

#View(rh_below_80)

#ggsave(plot = avg_days_rh_boxplot, "figures/tick-survival-assay/avg_days_rh_boxplot.png")

avg_days_temp_boxplot <- ggplot(temp_above_35, aes(status, days, color = status)) +
  geom_boxplot() +
  invasion_color +
  invasion_fill +
  def_theme +
  ylab("time above 35 C (days)") +
  theme(legend.position = "none") +
  NULL

#ggsave(plot = avg_days_temp_boxplot, "figures/tick-survival-assay/avg_days_temp_boxplot.png")

avg_days_temp_rh_boxplot <- cowplot::plot_grid(avg_days_rh_boxplot, avg_days_temp_boxplot, ncol = 2)

ggsave(plot = avg_days_temp_rh_boxplot, "figures/tick-survival-assay/avg_days_temp_rh_boxplot.png", height = 7, width = 7)

nymph_surv_min_rh <- cowplot::plot_grid(nymphs_intervals, avg_min_rhs_days_intervals, ncol = 1)
adult_F_surv_min_rh <- cowplot::plot_grid(adult_F_intervals, avg_min_rhs_days_intervals, ncol = 1)
adult_M_surv_min_rh <- cowplot::plot_grid(adult_M_intervals, avg_min_rhs_days_intervals, ncol = 1)

nymph_surv_max_temp <- cowplot::plot_grid(nymphs_intervals, avg_max_temps_days_intervals, ncol = 1)
adult_F_surv_max_temp <- cowplot::plot_grid(adult_F_intervals, avg_max_temps_days_intervals, ncol = 1)
adult_M_surv_max_temp <- cowplot::plot_grid(adult_M_intervals, avg_max_temps_days_intervals, ncol = 1)

#### analysis of bag specific survial w/ temp/rh ####

temp_rh_data_bag_specific <- temp_rh_data %>%
  filter(RH >= 20, date != "2018-06-21") %>%
  group_by(date, status, logger_id, days) %>%
  summarise(max_tempC = max(tempC),
            min_rh = min(RH)
  ) %>%
   ungroup(.) %>%
   filter(days <=11) %>%
   group_by(logger_id, status) %>%
   summarise(obs = n(),
             avg_max_tempC = mean(max_tempC),
             max_tempC_sd = sd(max_tempC),
             max_temp_se = max_tempC_sd/sqrt(obs),
             avg_min_rh = mean(min_rh),
             min_rh_sd = sd(min_rh),
             min_rh_se = min_rh_sd/sqrt(obs)
  )

#View(temp_rh_data_bag_specific)

survival_for_temp_rh_11_days <- tick_survival %>%
  filter(days == "11") %>%
  select(logger_id, Invaded, nymph_survival, adult_F_survival, adult_M_survival)

survival_temp_rh <-  left_join(temp_rh_data_bag_specific, survival_for_temp_rh_11_days,
                          by = c("logger_id", "status"="Invaded"))

View(survival_temp_rh)
range(survival_temp_rh$avg_max_tempC)
#### 11 days first check figures for TEMP ####

nymph_temp_11_days <- ggplot(survival_temp_rh, aes(avg_max_tempC, nymph_survival,
                                                      color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_errorbarh(aes(xmin = avg_max_tempC - max_temp_se,
                     xmax = avg_max_tempC + max_temp_se)) +
  invasion_color +
  invasion_fill +
  ylab("Survival") +
  xlab(" ") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Nymphs")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 1 )) +
  scale_x_continuous(limits = c(30, 43)) +
  NULL

adult_F_temp_11_days <- ggplot(survival_temp_rh, aes(avg_max_tempC, adult_F_survival,
                                                      color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  #geom_errorbarh(aes(xmin = avg_max_tempC - max_temp_se,
                    #xmax = avg_max_tempC + max_temp_se)) +
  invasion_color +
  invasion_fill +
  ylab(" ") +
  xlab("Avg max temp C ") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Adult Females") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 1 )) +
  scale_x_continuous(limits = c(30, 43)) +
  NULL

adult_M_temp_11_days <- ggplot(survival_temp_rh, aes(avg_max_tempC, adult_M_survival,
                                                      color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  #geom_errorbarh(aes(xmin = avg_max_tempC - max_temp_se,
                     #xmax = avg_max_tempC + max_temp_se)) +
  invasion_color +
  invasion_fill +
  ylab(" ") +
  xlab(" ") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Adult Males") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 1 )) +
  scale_x_continuous(limits = c(30, 43)) +
  NULL

survival_temp_first_check <- cowplot::plot_grid(nymph_temp_11_days,
                                                adult_F_temp_11_days,
                                                adult_M_temp_11_days, ncol = 3)

ggsave(plot = survival_temp_first_check, height = 5, width = 10, "figures/tick-survival-assay/survival_temp_first_check.png")
#### 11 days survival for RH ####

nymph_rh_11_days <- ggplot(survival_temp_rh, aes(avg_min_rh, nymph_survival,
                                                      color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
                     xmax = avg_min_rh + min_rh_se)) +
  invasion_color +
  invasion_fill +
  ylab("Survival") +
  xlab(" ") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Nymphs")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 1 )) +
  NULL

adult_F_rh_11_days <- ggplot(survival_temp_rh, aes(avg_min_rh, adult_F_survival,
                                                 color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
                     xmax = avg_min_rh + min_rh_se)) +
  invasion_color +
  invasion_fill +
  ylab(" ") +
  xlab("Avg min RH") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Female Adults")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 1 )) +
  NULL

adult_M_rh_11_days <- ggplot(survival_temp_rh, aes(avg_min_rh, adult_M_survival,
                                                 color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(aes(color = status)) +
  geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
                     xmax = avg_min_rh + min_rh_se)) +
  invasion_color +
  invasion_fill +
  ylab(" ") +
  xlab(" ") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Male Adults")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 1 )) +
  NULL

survival_rh_first_check <- cowplot::plot_grid(nymph_rh_11_days, adult_F_rh_11_days, adult_M_rh_11_days, ncol = 3)

ggsave(plot = survival_rh_first_check, height = 5, width = 10, "figures/tick-survival-assay/survival_rh_first_check.png")
