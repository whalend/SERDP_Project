####### 2019 plot report data analysis #####

#### load packages ####
library(plyr); library(dplyr); library(ggplot2); library(readr)
library(stringi)

#+read in data ####
tick_data <- read_csv(file = "data/raw_data/2019_serdp_data/2019-only-tick-data-rev.csv")


tick_data <- tick_data %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))


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
invasion_color <- scale_color_manual(values = c("deepskyblue", "red"))
invasion_fill <- scale_color_manual(values = c("deepskyblue", "red"))

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

