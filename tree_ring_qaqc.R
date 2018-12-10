## doing tree ring width analysis

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
#library(tidyverse)

ring_data <- read.csv("data/raw_data/tree_ring_widths.csv")

summary(ring_data)

unique(ring_data$year)

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28))
                   
treatment_color <- scale_color_manual(values = c("red","blue","purple","green"))

scaleFUN <- function(x) sprintf("%.0f", x)

ring_data_se <- ring_data %>% 
  group_by(treatment, year) %>%
  summarise(obs = n(),
            mean_ring_width = mean(ring_width),
            ring_width_sd = sd(ring_width))
            ring_width_se = ring_width_sd/sqrt(obs))
            
ring_data_2015 <- ring_data %>% 
  filter(year >= 2015)

ggplot(ring_data_2015, aes(year, ring_width, color = treatment)) +
  geom_point(alpha = 0.2) + 
  scale_x_continuous(limits = c(2014,2018)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", 
               position = position_dodge(width = 0.9)) +
  treatment_color +
  def_theme +
  NULL


ring_data_2013 <- ring_data %>% 
  filter(year >= 2013)


ggplot(ring_data_2013, aes(year, ring_width, color = treatment)) +
  geom_point() + 
  #scale_x_continuous(labels = scaleFUN) +
  scale_x_continuous(limits = c(2012,2018)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", 
               position = position_dodge(width = 0.9)) +
  treatment_color +
  def_theme +
  NULL
