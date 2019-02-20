## doing tree ring width analysis

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(stringr)
library(readxl)

theme_set(theme_bw())

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28))

treatment_color <- scale_color_manual(values = c("red","blue","purple","green"))

year_color <- scale_color_manual(values = c("red","blue","purple","green"))

scaleFUN <- function(x) sprintf("%.0f", x)

#### NEW measuRING data ####

cross1_data <- read_csv("tree_rings/data/cross1_data_post66.csv")
#View(cross1_data)
summary(cross1_data)

#filter(cross1_data, is.na(ring_width_mm))

cross1_data$cs_id[cross1_data$cs_id=="2-G1_AN_crossSection1"] <- "7-G_AC_crossSection1"
cross1_data$cs_id[cross1_data$cs_id=="2-G2_AN_crossSection1"] <- "2-G_AN_crossSection1"
cross1_data$cs_id[cross1_data$cs_id=="2-B_AN_crossSection1"] <- "7-B_AC_crossSection1"
cross1_data$cs_id[cross1_data$cs_id=="2-M_AN_crossSection1"] <- "7-M_AC_crossSection1"

cross1_data <- cross1_data %>% 
  filter(cs_id !="30-B_DC_crossSection1")

cross1_data <- cross1_data %>% 
  mutate(treatment = case_when(grepl("AC", cs_id) ~ "AC",
                               grepl("AN", cs_id) ~ "AN",
                               grepl("DC", cs_id) ~ "DC",
                               grepl("DN", cs_id) ~ "DN"),
         Plot = substr(cross1_data$cs_id, 1, 2) %>% 
           str_replace(., "-", ""),
         Ind. = substr(cross1_data$cs_id, 3, 4) %>% 
           str_replace(., "[_-]" , ""))
         
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

#####  keep cross1_all_time major #####

ggsave(plot = cross1_all_time, height = 5, width = 10, "tree_rings/figures/width_crossSection1_all_time.png")

filter(cross1_data, is.na(ring_width_mm) & year == 2015)
filter(cross1_data, ring_width_mm > 13)
filter(cross1_data, cs_id=="30-B_DC_crossSection1")

cross1_data %>% 
  group_by(treatment) %>% 
  summarise(count = length(treatment)/6)

bivens_trees <- read_csv("tree_rings/data/bivenstree.csv")

bivens_treatments <- read_csv("tree_rings/data/Bivens.treatments.csv")

left_join(bivens_trees, bivens_treatments, by = c("Plot"="Plot"))

tree_dbh_time <- ggplot(bivens_trees, aes(treatment, dbh_cm, color = treatment)) +
  geom_point(alpha = 0.2) + 
  #scale_x_continuous(labels = scaleFUN) +
  #scale_x_continuous(limits = c(2012, 2019), breaks = c(2013, 2014, 2015, 2016, 2017, 2018)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", 
               position = position_dodge(width = 0.7)) +
  treatment_color +
  def_theme +
  NULL

tree_diam_time <- ggplot(bivens_trees, aes(treatment, diam2_mm, color = treatment)) +
  geom_point(alpha = 0.2) + 
  #scale_x_continuous(labels = scaleFUN) +
  #scale_x_continuous(limits = c(2012, 2019), breaks = c(2013, 2014, 2015, 2016, 2017, 2018)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", 
               position = position_dodge(width = 0.7)) +
  treatment_color +
  def_theme +
  NULL
###### keep tree_diam_time #####
bivens_soil <- read_excel("tree_rings/data/BivensSoilMoisture2013_2017.xlsx")

bivens_soil$Date <- as.Date(bivens_soil$Date, format = "%Y%m%d")

bivens_soil <- bivens_soil[-c(6:16384)]

bivens_soil <- bivens_soil %>%
  mutate(Plot = as.character(Plot)) %>% 
  mutate(year = lubridate::year(Date)) %>% 
  mutate(month = lubridate::month(Date))

bivens_soil <- bivens_soil %>% 
  mutate(Treatment = case_when(Treatment=="A" ~ "AN",
                               Treatment=="D" ~ "DN",
                               TRUE ~ Treatment))

soil_summary_treatment <- bivens_soil %>% 
  group_by(Treatment) %>% 
  summarise(average_vwc = mean(percent_VWC,
                               na.rm = T))

soil_rw_treatment <- left_join(cross1_summary, soil_summary_treatment, by = c("year", "treatment"="Treatment"))

soil_summary_plot <- bivens_soil %>%
  filter(between(month,3,9)) %>% 
  group_by(Plot, Treatment, year) %>% 
  summarise(average_vwc = mean(percent_VWC,
                               na.rm = T)) 
  
soil_rw_plot <- left_join(cross1_summary, soil_summary_plot, by = c("Plot", "year", "treatment"="Treatment"))

soil_all_time <- ggplot(soil_summary_plot, aes(year, average_vwc, color = Treatment)) +
  geom_point(alpha = 1, size = 1.85) + 
  scale_x_continuous(labels = scaleFUN) +
  scale_x_continuous(limits = c(2012, 2020), breaks = c(2013, 2014, 2015, 2016, 2017, 2018)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", 
               position = position_dodge(width = 0.7)) +
  #treatment_color +
  theme_excel() +
  def_theme +
  NULL

###### make box plot, keep. also do boxplot for ring widths ####

cross1_summary <- cross1_data %>% 
  group_by(Plot, year, treatment) %>% 
  summarise(average_RW_mm = mean(ring_width_mm, na.rm = T))

soil_rw_plot_2015 <- soil_rw_plot %>% 
  filter(year==2015)

soil_rw_plot_2015_figure <- ggplot(soil_rw_plot_2015, aes(average_vwc, average_RW_mm, color = treatment)) +
  geom_point(alpha = 1, size = 1.5) + 
  #stat_summary(fun.data = "mean_se", geom = "pointrange", 
               #position = position_dodge(width = 0.7)) +
  treatment_color +
  #def_theme +
  NULL

rw_soil_trees <- left_join(cross1_data, soil_summary_plot, by = c("year", "treatment"="Treatment", "Plot"))

rw_soil_trees_AN <- rw_soil_trees %>% 
  filter(between(year,2014,2017)) %>% 
  filter(treatment=="AN")

rw_soil_trees_DN <- rw_soil_trees %>% 
  filter(between(year,2014,2017)) %>% 
  filter(treatment=="DN")

rw_soil_trees_AC <- rw_soil_trees %>% 
  filter(between(year,2014,2017)) %>% 
  filter(treatment=="AC")

rw_soil_trees_DC <- rw_soil_trees %>% 
  filter(between(year,2014,2017)) %>% 
  filter(treatment=="DC")

rw_diam_soil_density %>% 
  filter(between(year,2014,2017)) %>% 
  mutate(f_year = as.factor(year)) %>% 
  ggplot(aes(diameter, ring_width_mm)) +
  geom_point(aes(color = f_year), alpha = .7, size = 2) +
  geom_smooth(aes(color = f_year), method = "lm", se = F) +
  facet_grid(.~treatment)
rw_diam_soil_density
##### keep maybe use treatment or treatment/year facet #####

rw_soil_trees %>% 
  filter(between(year,2014,2017)) %>% 
  mutate(f_year = as.factor(year)) %>% 
  ggplot(aes(average_vwc, ring_width_mm)) +
  geom_point(aes(color = year), alpha = .7, size = 2) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(year~treatment)
  
rw_soil_trees_AN_figure <- ggplot(rw_soil_trees_AN, aes(average_vwc, ring_width_mm, color = as.factor(year))) +
  geom_point(alpha = 1, size = 1.5) + 
  geom_smooth(data = filter(rw_soil_trees_AN, year==2017), aes(linetype = treatment), se = F, method = "lm") + 
  geom_smooth(data = filter(rw_soil_trees_AN, year==2016), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_AN, year==2015), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_AN, year==2014), aes(linetype = treatment), se = F, method = "lm") +
  scale_x_continuous(0, 35) +
  scale_y_continuous(0, 15) +
  year_color +
  def_theme +
  NULL


rw_soil_trees_AN_figure <- ggplot(rw_soil_trees_AN, aes(average_vwc, ring_width_mm, color = as.factor(year))) +
  geom_point(alpha = 1, size = 1.5) + 
  geom_smooth(data = filter(rw_soil_trees_AN, year==2017), aes(linetype = treatment), se = F, method = "lm") + 
  geom_smooth(data = filter(rw_soil_trees_AN, year==2016), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_AN, year==2015), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_AN, year==2014), aes(linetype = treatment), se = F, method = "lm") +
  year_color +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(limits = c(0, 15)) +
  def_theme +
  NULL

rw_soil_trees_DN_figure <- ggplot(rw_soil_trees_DN, aes(average_vwc, ring_width_mm, color = as.factor(year))) +
  geom_point(alpha = 1, size = 1.5) + 
  geom_smooth(data = filter(rw_soil_trees_DN, year==2017), aes(linetype = treatment), se = F, method = "lm") + 
  geom_smooth(data = filter(rw_soil_trees_DN, year==2016), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_DN, year==2015), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_DN, year==2014), aes(linetype = treatment), se = F, method = "lm") +
  year_color +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(limits = c(0, 15)) +
  def_theme +
  NULL

rw_soil_trees_AC_figure <- ggplot(rw_soil_trees_AC, aes(average_vwc, ring_width_mm, color = as.factor(year))) +
  geom_point(alpha = 1, size = 1.5) + 
  geom_smooth(data = filter(rw_soil_trees_AC, year==2017), aes(linetype = treatment), se = F, method = "lm") + 
  geom_smooth(data = filter(rw_soil_trees_AC, year==2016), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_AC, year==2015), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_AC, year==2014), aes(linetype = treatment), se = F, method = "lm") +
  year_color +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(limits = c(0, 15)) +
  def_theme +
  NULL

rw_soil_trees_DC_figure <- ggplot(rw_soil_trees_DC, aes(average_vwc, ring_width_mm, color = as.factor(year))) +
  geom_point(alpha = 1, size = 1.5) + 
  geom_smooth(data = filter(rw_soil_trees_DC, year==2017), aes(linetype = treatment), se = F, method = "lm") + 
  geom_smooth(data = filter(rw_soil_trees_DC, year==2016), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_DC, year==2015), aes(linetype = treatment), se = F, method = "lm") +
  geom_smooth(data = filter(rw_soil_trees_DC, year==2014), aes(linetype = treatment), se = F, method = "lm") +
  year_color +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(limits = c(0, 15)) +
  def_theme +
  NULL

##### making 4 panel figure of treatment by year #####

rw_soil_treatment_panels <- cowplot::plot_grid(rw_soil_trees_AN_figure, rw_soil_trees_DN_figure, rw_soil_trees_AC_figure, rw_soil_trees_DC_figure, ncol = 2)

###### calculating yearly diameters from ring widths #####

cross1_data
bivens_trees_specs <- bivens_trees %>%
  mutate(Plot = as.character(Plot)) %>% 
  select(c(1,2,3,4,8,9,10,11,12))

calculating_diameter <- left_join(cross1_data, bivens_trees_specs)
# calculating_diameter %>% 
#   select(year, Plot, Ind., ring_width_mm, diam2_mm) %>% 
#   tidyr::spread(year, ring_width_mm) %>% 
#   tidyr::gather(year, ring_width_mm, c(-Plot, -Ind.))

calculating_diameter <- calculating_diameter %>% 
  select(year, Plot, Ind., treatment, ring_width_mm, diam2_mm) %>% 
  tidyr::spread(year, ring_width_mm) %>% 
  mutate(diam_mm_2018 = diam2_mm,
         diam_mm_2017 = diam_mm_2018-2*`2018`,
         diam_mm_2016 = diam_mm_2017-2*`2017`,
         diam_mm_2015 = diam_mm_2016-2*`2016`,
         diam_mm_2014 = diam_mm_2015-2*`2015`) %>% 
  mutate(`2018` = diam_mm_2018,
         `2017` = diam_mm_2017,
         `2016` = diam_mm_2016,
         `2015` = diam_mm_2015,
         `2014` = diam_mm_2014) %>% 
  select(Plot:`2018`, -diam2_mm) %>%
  tidyr::gather(year, diameter, c(-Plot, -Ind., -treatment))

calculating_diameter <- calculating_diameter %>% 
  mutate(year = as.integer(year))

rw_diameter_data <- left_join(cross1_data, calculating_diameter)

rw_diameter_data <- rw_diameter_data %>% 
  filter(year!=2013) %>%
  filter(year!=2018) %>%
  mutate(rw_diam_ratio = ring_width_mm/diameter,
         Plot = as.integer(Plot))

annual_diameter_figure <- ggplot(rw_diameter_data, aes(year, diameter, color = treatment)) +
  geom_point() +
  treatment_color +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.7)) +
  def_theme +
  NULL

##### same story different trailer park #####

annual_something_figure <- ggplot(rw_diameter_data, aes(diameter, ring_width_mm, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  treatment_color +
  def_theme +
  NULL

rw_mod_data <- filter(rw_diam_soil_density, !is.na(ring_width_mm))
rw_diam_mod <- lm(ring_width_mm~diameter, data = rw_mod_data)
summary(rw_diam_mod)
diam_resids <- resid(rw_diam_mod)

#plot(rw_diam_mod)

rw_diam_mod$diam_resids <- diam_resids

rw_mod_data <- left_join(
  rw_mod_data,
  rw_diam_soil_density %>% 
    filter(year==2014) %>% select(Plot, year, Ind., diameter) %>% 
    rename(diam2014 = diameter)
)


m2 <- lm(ring_width_mm~treatment + average_vwc+tree_density + diam2014, data = rw_mod_data)
summary(m2)

mm1 <- lme4::lmer(ring_width_mm ~ average_vwc+ tree_density + diam2014 + (1|Plot), data = rw_mod_data)
summary(mm1)  

rw_diam_soil_density_model_one <- lm(ring_width_mm~average_vwc+tree_density+as.factor(year)+diameter, data = rw_diam_soil_density)
summary(rw_diam_soil_density_model_one)

anova(rw_diam_soil_density_model_one)

rw_diam_soil_density <- left_join(rw_diam_soil, tree_density %>% 
                                    mutate(Plot= as.character(Plot)))

rw_diam_soil <- left_join(rw_diameter_data %>% 
                            mutate(Plot= as.character(Plot)), rw_soil_trees)

annual_ratio_figure <- ggplot(rw_diameter_data, aes(year, rw_diam_ratio, color = treatment)) +
  geom_point() +
  treatment_color +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.7)) +
  def_theme +
  NULL


         

#View(calculating_diameter) ####

rw_diameter_yearly_2017 <- ggplot(calculating_diameter, aes(diam_mm_2017, rw_mm_2017, color = treatment)) +
  geom_point(size = 1.3) +
  geom_smooth(se = F) +
  treatment_color +
  def_theme +
  NULL

rw_diameter_yearly_2016 <- ggplot(calculating_diameter, aes(diam_mm_2015, rw_mm_2016, color = treatment)) +
  geom_point(size = 1.3) +
  geom_smooth(se = F) +
  treatment_color +
  def_theme +
  NULL 

rw_diameter_yearly_2015 <- ggplot(calculating_diameter, aes(diam_mm_2015, rw_mm_2015, color = treatment)) +
  geom_point(size = 1.3) +
  geom_smooth(se = F) +
  treatment_color +
  def_theme +
  NULL

rw_diameter_yearly_2014 <- ggplot(calculating_diameter, aes(diam_mm_2014, rw_mm_2014, color = treatment)) +
  geom_point(size = 1.3) +
  geom_smooth(se = F) +
  treatment_color +
  def_theme +
  NULL
stop ##### 

rw_diameter_yearly_cowplot <- cowplot::plot_grid(rw_diameter_yearly_2014,rw_diameter_yearly_2015,rw_diameter_yearly_2016,rw_diameter_yearly_2017, ncol = 2)

tree_density <- bivens_trees %>% 
  group_by(Plot) %>% 
  summarise(tree_density = n_distinct(Ind.))
tail(tree_density)

tree_density_all <- left_join(rw_diameter_data, tree_density)

tree_density_2017 <- tree_density_all %>% 
  filter(year==2017)

tree_density_figure_one <- ggplot(rw_diam_soil_density, aes(average_vwc, tree_density, color = treatment)) +
  geom_point(size = 1.3, alpha = .5) +
  geom_smooth(method = "lm", se = T) +
  #facet_grid(~year) +
  treatment_color +
  def_theme +
  NULL
#drew said this on

density_soil_moisture <- left_join(soil_summary_plot, tree_density %>% 
                                     mutate(Plot = as.character(Plot)))

tree_density_figure_two <- ggplot(density_soil_moisture, aes(average_vwc, tree_density, color = Treatment)) +
  geom_point(size = 1.3, alpha = .5) +
  geom_smooth(method = "lm", se = T) +
  #facet_grid(~year) +
  treatment_color +
  def_theme +
  NULL
