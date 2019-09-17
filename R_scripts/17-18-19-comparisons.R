#### 2019 comparisons to 17 18 serdp southeast data

library(stringr); library(ggplot2); library(dplyr); library(readr)

plot_visit <- read_csv("data/raw_data/plot-visit-data-entry.csv")

summary(plot_visit)
na <- plot_visit %>% 
  filter(is.na(plot_visit$last_fire_year))

shelby <- plot_visit %>% 
  filter(installation=="shelby")

last_fire <- plot_visit %>% 
  filter(last_fire_year <= 2010) %>% 
  mutate(plot_id = paste(installation, plot_id, sep = " "))

invaded <- last_fire %>% 
  filter(imcy_inv=="invaded")

unique(last_fire$installation)

old_ticks <- read_csv("data/processed_data/ticks.csv")

old_ticks_stat <- old_ticks %>% 
  group_by(installation, plot_id, visit_date, life_stage) %>% 
  summarise(total_ticks = sum(count))

old_plot_ticks <- left_join(last_fire, old_ticks_stat)

sort(unique(old_plot_ticks$plot_id))
sort(unique(last_fire$plot_id))
old_plot_ticks[is.na(old_plot_ticks)] <- 0
old_plot_ticks <- old_plot_ticks[,-c(6)]

old <- old_plot_ticks %>% 
  group_by(imcy_inv) %>% 
  summarise(mean_ticks = mean(total_ticks))


new_ticks <- read_csv("data/raw_data/2019_serdp_data/2019-only-tick-data.csv")

# new_ticks <- new_ticks %>% 
#   mutate(date = lubridate::ymd(date),
#          visit_year = lubridate::year(date))

new_ticks$invaded[(new_ticks$invaded=="y")] <- "invaded"
new_ticks$invaded[(new_ticks$invaded=="n")] <- "native"

unique(new_ticks$plot_id)
new_ticks$plot_id[(new_ticks$plot_id=="i-1")] <- "i1"
new_ticks$plot_id[(new_ticks$plot_id=="i-2")] <- "i2"
new_ticks$plot_id[(new_ticks$plot_id=="i-3")] <- "i3"
new_ticks$plot_id[(new_ticks$plot_id=="i-4")] <- "i4"
new_ticks$plot_id[(new_ticks$plot_id=="i-5")] <- "i5"
new_ticks$plot_id[(new_ticks$plot_id=="n-1")] <- "n1"
new_ticks$plot_id[(new_ticks$plot_id=="n-2")] <- "n2"
new_ticks$plot_id[(new_ticks$plot_id=="n-3")] <- "n3"

colnames(new_ticks)[colnames(new_ticks)=="invaded"] <- "imcy_inv"
colnames(new_ticks)[colnames(new_ticks)=="date"] <- "visit_date"

new_ticks <- new_ticks %>% 
     mutate(visit_date = lubridate::ymd(visit_date))

new_plot_ticks <- new_ticks %>% 
  group_by(installation, plot_id, imcy_inv, visit_date, life_stage) %>% 
  summarise(total_ticks = sum(count))

write_csv(new_plot_ticks, "C:/Users/Steven/Desktop/serdp/2019 data/tsa_ticks_test.csv")

new_ticks_manual_edit <- read_csv("C:/Users/Steven/Desktop/serdp/2019 data/tsa_ticks_test_edited.csv")

new_ticks_manual_edit[is.na(new_ticks_manual_edit)] <- 0

new_ticks_manual_edit <- new_ticks_manual_edit %>% 
  mutate(visit_date = lubridate::mdy(visit_date))

new_ticks_manual_edit <- new_ticks_manual_edit %>% 
  mutate(last_fire_year = 1)

all_ticks <- rbind(old_plot_ticks, new_ticks_manual_edit)

all_ticks <- all_ticks %>% 
  mutate(visit_year = lubridate::year(visit_date)) %>% 
  filter(plot_id !="shelby f1") %>% 
  filter(plot_id !="shelby b1")

all_ticks$imcy_inv[(all_ticks$imcy_inv=="uninvaded")] <- "regional"

unique(all_ticks$imcy_inv)

  

invasion_color <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red", "deepskyblue"))
region_color <- scale_color_manual(values = c("green"))

all_plot_year <- ggplot(data = all_ticks, mapping = aes(x = visit_year, y = total_ticks, color = imcy_inv)) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'ticks per plot') +
  invasion_color +
  region_color +
  xlab("year") +
  invasion_color +
  invasion_fill +
  #theme(legend.position="none") +
  scale_x_continuous(breaks = c(2017,2018,2019)) +
  NULL

year_breakdown <- 
  ggplot() +
  geom_bar(data = all_ticks, aes(imcy_inv, color = imcy_inv, fill = imcy_inv), stat = "count", position = "dodge") +
  invasion_color +
  invasion_fill +
  theme_bw() +
  NULL
