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
invasion_color <- scale_color_manual(values = c("deepskyblue", "red"))
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
  mutate(plot_id = substr(###########3

  steven stop here sleepy. trying to string out last two characters for plot id to eventually get invaded/native status column.

quadrat_data_2019_stats <- quadrat_data_2019 %>% 
  group_by(installation, date, plot_id, transect_id) %>% 
  filter(!is.na(woody_veg_ht1), !is.na(woody_veg_ht2), !is.na(woody_veg_ht3)) %>% 
  summarise(avg_woody_veg_ht = mean(woody_veg_ht1, woody_veg_ht2, woody_veg_ht3, na.rm = T))
            
            
            avg_herb_veg_ht = sum(herb_veg_ht1, herb_veg_ht2, herb_veg_ht3),
            avg_litter_ht = sum(litter_ht1, litter_ht2, litter_ht3))

TEST HI 

