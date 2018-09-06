#' # Script for doing QA/QC on the 1m and 25cm quadrat data

#' ## Quadrat Data
#' Percent cover
#'
#'    - litter
#'    - bare ground
#'    - green
#'
#' Heights
#'
#'    - woody vegetation
#'    - grassy/herbaceous vegetation
#'    - litter (depth)
#'

#+ load processed plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#+ 1m quadrat data ####
quadrat_data <- read_csv("data/raw_data/quadrat1m.csv")
quadrat_data$visit_date <- as.Date(quadrat_data$date, format = "%m/%d/%Y")
# quadrat_data$date <- gsub("-","", as.character(quadrat_data$visit_date))

# as.Date(gsub("-","", as.character(quadrat_data$date)), format = "%Y%m%d")

# plot_visit_data <- left_join(plot_visit_data,
#           quadrat_data %>% select(installation, plot_id, visit_date) %>%
#                 filter(visit_date>"2017-06-01") %>%
#                 unique(.))
# plot_visit_data <- plot_visit_data %>%
#       mutate(date = gsub("-","", visit_date),
#              visit_id = paste(date, installation, plot_id, sep = "_"))

# write_csv(plot_visit_data, "data/plot_visit_data.csv")

lubridate::year(quadrat_data$visit_date)
lubridate::month(quadrat_data$visit_date)
lubridate::day(quadrat_data$visit_date)

write_csv(quadrat_data, "data/quadrat1m.csv")
quadrat_data <- filter(quadrat_data, visit_date > "2017-06-01")# discard early data collection at Camp Blanding

summary(quadrat_data)

quadrat_data$woody_veg_ht1[is.na(quadrat_data$woody_veg_ht1)] <- 0
# quadrat_data$woody_veg_ht1 <- as.numeric(quadrat_data$woody_veg_ht1)
quadrat_data$veg_ht1[is.na(quadrat_data$veg_ht1)] <- 0
# quadrat_data$veg_ht1 <- as.numeric(quadrat_data$veg_ht1)
quadrat_data$pct_green[quadrat_data$pct_green=="<1"] <- 0.5
quadrat_data$pct_green <- as.numeric(quadrat_data$pct_green)

hist(quadrat_data$pct_green)
hist(quadrat_data$pct_litter)

quadrat_summaries <- quadrat_data %>%
      group_by(installation, plot_id) %>%
      summarise(
            avg_woody_ht1 = mean(woody_veg_ht1),
            avg_woody_ht_all = mean(
                  c(woody_veg_ht1,woody_veg_ht2,woody_veg_ht3), na.rm = T),
            avg_herb_ht1 = mean(veg_ht1),
            avg_herb_ht_all = mean(c(veg_ht1,veg_ht2,veg_ht3), na.rm = T),
            avg_litter_depth1 = mean(litter_ht1),
            avg_litter_depth_all =
                  mean(c(litter_ht1,litter_ht2,litter_ht3), na.rm = T),
            avg_pct_green = mean(pct_green),
            avg_pct_litter = mean(pct_litter),
            avg_pct_wood = mean(pct_wood),
            avg_pct_bare = mean(pct_bare))

quadrat10m_summaries <- quadrat_data %>%
      filter(quadrat_id == "n10"|quadrat_id == "e10"|quadrat_id=="s10"|quadrat_id=="w10") %>%
      group_by(installation, plot_id) %>%
      summarise(
            avg_woody_ht1 = mean(woody_veg_ht1),
            avg_woody_ht_all = mean(
                  c(woody_veg_ht1,woody_veg_ht2,woody_veg_ht3), na.rm = T),
            avg_herb_ht1 = mean(veg_ht1),
            avg_herb_ht_all = mean(c(veg_ht1,veg_ht2,veg_ht3), na.rm = T),
            avg_litter_depth1 = mean(litter_ht1),
            avg_litter_depth_all =
                  mean(c(litter_ht1,litter_ht2,litter_ht3), na.rm = T),
            avg_pct_green = mean(pct_green),
            avg_pct_litter = mean(pct_litter),
            avg_pct_wood = mean(pct_wood),
            avg_pct_bare = mean(pct_bare))

summary(quadrat_summaries)
summary(quadrat10m_summaries)

#' ## Compare All Quadrat Data to Subset of Data
#' Test for differences in means of data summarized across all quadrats (12) to data from the quadrats at 10-meters (4).
#'
#+ compare all quadrats to subset of 4 at 10m ####
par(mfrow=c(1,2))
hist(log(quadrat_summaries$avg_woody_ht1))
hist(log(quadrat10m_summaries$avg_woody_ht1))
t.test(log(quadrat_summaries$avg_woody_ht1),log(quadrat10m_summaries$avg_woody_ht1), var.equal = F)

hist(log(quadrat_summaries$avg_woody_ht_all))
var(log(quadrat_summaries$avg_woody_ht_all))
hist(log(quadrat10m_summaries$avg_woody_ht_all))
var(log(quadrat10m_summaries$avg_woody_ht_all))
t.test(log(quadrat_summaries$avg_woody_ht_all),log(quadrat10m_summaries$avg_woody_ht_all), var.equal = F)

hist(log(quadrat_summaries$avg_herb_ht1))
var(log(quadrat_summaries$avg_herb_ht1))
hist(log(quadrat10m_summaries$avg_herb_ht1))
var(log(quadrat10m_summaries$avg_herb_ht1))
t.test(log(quadrat_summaries$avg_herb_ht1),
       log(quadrat10m_summaries$avg_herb_ht1), var.equal = F)

hist(log(quadrat_summaries$avg_herb_ht_all))
var(log(quadrat_summaries$avg_herb_ht_all))
hist(log(quadrat10m_summaries$avg_herb_ht_all))
var(log(quadrat10m_summaries$avg_herb_ht_all))
t.test(log(quadrat_summaries$avg_herb_ht_all),
       log(quadrat10m_summaries$avg_herb_ht_all), var.equal = F)

hist(log(quadrat_summaries$avg_litter_depth1))
var(log(quadrat_summaries$avg_litter_depth1))
hist(log(quadrat10m_summaries$avg_litter_depth1))
var(log(quadrat10m_summaries$avg_litter_depth1))
t.test(log(quadrat_summaries$avg_litter_depth1),
       log(quadrat10m_summaries$avg_litter_depth1), var.equal = F)

hist(log(quadrat_summaries$avg_litter_depth_all))
var(log(quadrat_summaries$avg_litter_depth_all))
hist(log(quadrat10m_summaries$avg_litter_depth_all))
var(log(quadrat10m_summaries$avg_litter_depth_all))
t.test(log(quadrat_summaries$avg_litter_depth_all),
       log(quadrat10m_summaries$avg_litter_depth_all), var.equal = T)

hist((quadrat_summaries$avg_pct_green))
var((quadrat_summaries$avg_pct_green))
hist((quadrat10m_summaries$avg_pct_green))
var((quadrat10m_summaries$avg_pct_green))
t.test((quadrat_summaries$avg_pct_green),
       (quadrat10m_summaries$avg_pct_green), var.equal = F)

hist(log(quadrat_summaries$avg_pct_litter))
var(log(quadrat_summaries$avg_pct_litter))
hist(log(quadrat10m_summaries$avg_pct_litter))
var(log(quadrat10m_summaries$avg_pct_litter))
t.test(log(quadrat_summaries$avg_pct_litter),
       log(quadrat10m_summaries$avg_pct_litter), var.equal = F)

hist(log1p(quadrat_summaries$avg_pct_bare))
var((quadrat_summaries$avg_pct_bare))
hist(log1p(quadrat10m_summaries$avg_pct_bare))
var((quadrat10m_summaries$avg_pct_bare))
t.test(log1p(quadrat_summaries$avg_pct_bare),
       log1p(quadrat10m_summaries$avg_pct_bare), var.equal = F)