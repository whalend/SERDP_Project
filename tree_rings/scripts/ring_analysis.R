## doing tree ring width analysis

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28))

treatment_color <- scale_color_manual(values = c("red","blue","purple","green"))

scaleFUN <- function(x) sprintf("%.0f", x)

#### NEW measuRING data ####

cross1_data <- read_csv("tree_rings/data/cross1_data_post66.csv")
View(cross1_data)
summary(cross1_data)

#filter(cross1_data, is.na(ring_width_mm))

cross1_data <- cross1_data %>% 
  mutate(treatment = case_when(grepl("AC", cs_id) ~ "AC",
                               grepl("AN", cs_id) ~ "AN",
                               grepl("DC", cs_id) ~ "DC",
                               grepl("DN", cs_id) ~ "DN"))

cross1_data <- cross1_data %>% 
  filter(cs_id !="30-B_DC_crossSection1")

# cross1_data_good <- cross1_data %>% 
#   filter(between(year, 2015, 2017))
# 
# cross1_most_accurate <- ggplot(cross1_data_good, aes(year, ring_width_mm, color = treatment)) +
#   geom_point(alpha = 0.2) + 
#   #scale_x_continuous(labels = scaleFUN) +
#   scale_x_continuous(limits = c(2014, 2018), breaks = c(2014, 2015, 2016, 2017, 2018)) +
#   stat_summary(fun.data = "mean_se", geom = "pointrange", 
#                position = position_dodge(width = 0.7)) +
#   treatment_color +
#   def_theme +
#   NULL
# 
# ggsave(plot = cross1_most_accurate, height = 5, width = 10, "tree_rings/figures/width_crossSection1_best_data.png")


cross1_all_time <- ggplot(cross1_data, aes(year, ring_width_mm, color = treatment)) +
  geom_point(alpha = 0.2) + 
  #scale_x_continuous(labels = scaleFUN) +
  scale_x_continuous(limits = c(2012, 2019), breaks = c(2013, 2014, 2015, 2016, 2017, 2018)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", 
               position = position_dodge(width = 0.7)) +
  treatment_color +
  def_theme +
  NULL

ggsave(plot = cross1_all_time, height = 5, width = 10, "tree_rings/figures/width_crossSection1_all_time.png")

filter(cross1_data, is.na(ring_width_mm) & year == 2015)
filter(cross1_data, ring_width_mm > 13)
filter(cross1_data, cs_id=="30-B_DC_crossSection1")

cross1_data %>% 
  group_by(treatment) %>% 
  summarise(count = length(treatment)/6)

bivens_trees <- read_csv("tree_rings/data/bivenstree.csv")

left_join(cross1_data, bivens_trees, by = "treatment")
