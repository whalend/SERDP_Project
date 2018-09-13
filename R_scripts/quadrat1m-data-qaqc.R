#' # Script for doing QA/QC on the 1m quadrat data

#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)

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
summary(quadrat_data)
str(quadrat_data)

#' Data Issues
#' * ~~4 `NA's` in 'date'~~
#' * ~~116 `NA's` in 'woody_veg_ht1~~
#' * ~~'herb_veg_ht' & 'pct_green' columns are reading in as character when they should be numeric~~
#' * 3 `NA's` in 'pct_litter'
#' * 63 `NA's` in 'pct_bare'
#' * ~~need unique plot id~~
#' * ~~need to filter out data from pilot plot visits, i.e. before 20170601~~
#'
#' Since we took up to three measurements of vegetation height and litter depth we deemed that if there wasn't anything meeting that category then the first measurement would be 0 while the remaining two would be recorded as `NA`. There shouldn't be any `NA's` in the first measurement column for these variables. Also, there should rarely (never?) be `NA's` for measures of percent cover.

#+ add unique plot id and investigate date NA's ####
quadrat_data <- quadrat_data %>%
      mutate(plot_id = paste(installation, plot_id, sep = " "))
unique(quadrat_data$plot_id)# "extra" plots from design tests
unique(plot_visit_data$plot_id)
quadrat_data$plot_id[quadrat_data$plot_id=="blanding cogan"] <- "blanding theater_cogon"

filter(quadrat_data, is.na(date))$plot_id
filter(plot_visit_data, plot_id=="blanding c1")
quadrat_data$date[is.na(quadrat_data$date)] <- 20180516

quadrat_data <- quadrat_data %>%
      filter(date > 20170601) # discard design test data

unique(plot_visit_data$plot_id) %in% unique(quadrat_data$plot_id)
#+ convert herb_veg_ht to numeric ###

summary(quadrat_data)

#+ investigate herb_veg_ht as character
sort(unique(quadrat_data$herb_veg_ht1))# "82I"
sort(unique(quadrat_data$herb_veg_ht2))# "74I"
sort(unique(quadrat_data$herb_veg_ht3))# "70I" & "nA"

filter(quadrat_data, herb_veg_ht1=="82I")$plot_id
quadrat_data$herb_veg_ht1[quadrat_data$herb_veg_ht1=="82I"] <- 82
filter(quadrat_data, herb_veg_ht2=="74I")$plot_id
quadrat_data$herb_veg_ht2[quadrat_data$herb_veg_ht2=="74I"] <- 74
filter(quadrat_data, herb_veg_ht3=="70I")$plot_id
quadrat_data$herb_veg_ht3[quadrat_data$herb_veg_ht3=="70I"] <- 70
filter(quadrat_data, herb_veg_ht3=="nA")$plot_id

quadrat_data <- quadrat_data %>%
      mutate(herb_veg_ht1 = as.numeric(herb_veg_ht1),
       herb_veg_ht2 = as.numeric(herb_veg_ht2),
       herb_veg_ht3 = as.numeric(herb_veg_ht3))

summary(quadrat_data)

#+ check NA's in ht1 columns and convert to 0 as appropriate ####

summary(filter(quadrat_data, is.na(woody_veg_ht1)))
# View(filter(quadrat_data, is.na(woody_veg_ht1), date < 20180101))
quadrat_data$woody_veg_ht1[is.na(quadrat_data$woody_veg_ht1)] <- 0

summary(filter(quadrat_data, is.na(herb_veg_ht1)))
quadrat_data$herb_veg_ht1[is.na(quadrat_data$herb_veg_ht1)] <- 0

## check 'pct_green' as character
unique(quadrat_data$pct_green)
quadrat_data$pct_green[quadrat_data$pct_green=="<1"] <- 0.5
quadrat_data$pct_green <- as.numeric(quadrat_data$pct_green)


## check pct litter, bare, green NA's ####
filter(quadrat_data, is.na(pct_litter)) %>%
      select(plot_id, date, quadrat_id, litter_ht1, pct_litter, pct_green, pct_bare)
## All from the same plots/quadrats, probably the ones we whiffed on recording data.

#+ write out processed data ####
write_csv(quadrat_data, "data/processed_data/quadrat1m.csv")


#+ initial data summaries and comparing sampling effort ####
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


#' There are no statistical differences in the means of these variables, which are aggregated to the plot level, based on these t-tests.


#' ## Biomass data sampled at 25cm quadrats
#'
