# Tyndal AFB GIS

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
tyndall_fires <- st_read("data/Tyndall/tyndall_fire_2010_2018.shp")
tyndall_fires2 <- st_read("data/Tyndall/tyndall_fire_2000_2009.shp")

tyndall_fires$inst_name = "Tyndall AFB"
# summary(tyndall_fires)
tyndall_fires <- tyndall_fires %>%
      select(inst_name,
             fArea_ac = areaSize,
             fCause = causeType,
             fType = fireType,
             fDate = fireStartD) %>%
      mutate(fDate = as.Date(as.character(fDate), format = "%Y%m%d"),
             fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate),
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"
             ),
             purpose = NA
      ) %>%
      select(inst_name:fDate, season, purpose, fYear, fMonth)
# tyndall_fires

summary(tyndall_fires2)
tyndall_fires2$inst_name = "Tyndall AFB"
tyndall_fires2 <- tyndall_fires2 %>%
      select(inst_name,
             fArea_ac = Acres,
             fCause = CAUSE,
             fType = BURN_TYPE,
             fDate = DET_DATE) %>%
      mutate(fDate = as.Date(as.character(fDate), format = "%Y-%m-%d"),
             fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate),
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"
             ),
             purpose = NA
      ) %>%
      select(inst_name:fDate, season, purpose, fYear, fMonth)

# tyndall_fires2

tyndall_fires <- rbind(tyndall_fires, tyndall_fires2)

rm(tyndall_fires2)

summary(tyndall_fires)
st_write(tyndall_fires, "data/Tyndall/tyndall_fires_2000_2018.shp")


plot_locations <- st_read("data/plot_visit.shp")# all plot locations/visits

plot_locations <- plot_locations %>%
      dplyr::rename(inst = instal,
                    visit_date = vist_dt,
                    last_fire = lst_fr_,
                    yrs_since_fire = yrs_sn_,
                    visit_yr = vist_yr,
                    imcy = imcy_nv) %>%
      dplyr::select(-instllt, -instl__) %>%
      dplyr::filter(!is.na(visit_yr))

tyndall_plots <- filter(plot_locations, inst=="tyndall")
## plots visited in 2017
tyndall2017 <- filter(tyndall_plots, visit_yr==2017)

## plots visited in 2018
tyndall2018 <- filter(tyndall_plots, visit_yr==2018)

tyndall_fires$FID <- seq(1,nrow(tyndall_fires),1)

fires2018 <- st_join(st_transform(tyndall2018, crs = st_crs(tyndall_fires)),
                 tyndall_fires %>%
                       filter(fDate > "2003-06-27",
                              fDate < max(tyndall2018$visit_date))
                 )

fri2018 <- fires2018 %>%
      group_by(plot_id, inst_nm, visit_date) %>%
      summarise(
            n_fires = length(plot_id),
            fri15yr = 15/n_fires,
            d_since_fire = max(visit_date) - max(fDate),
            w_since_fire = d_since_fire/7,
            y_since_fire = w_since_fire/52,
            frindex = (1/fri15yr)/y_since_fire
                )

fires2017 <- st_join(st_transform(tyndall2017, crs = st_crs(tyndall_fires)),
                 tyndall_fires %>%
                       filter(fDate > "2002-07-20",
                              fDate < max(tyndall2017$visit_date))
                 )

fri2017 <- fires2017 %>%
      group_by(plot_id, inst_nm, visit_date) %>%
      summarise(
            n_fires = length(plot_id),
            fri15yr = 15/n_fires,
            d_since_fire = max(visit_date) - max(fDate),
            w_since_fire = d_since_fire/7,
            y_since_fire = (w_since_fire/52),
            frindex = (1/fri15yr)/y_since_fire
      )
tyndall_fri <- rbind(fri2017, fri2018)
summary(tyndall_fri)

## Write shapefile
st_write(tyndall_fri, "data/Tyndall/tyndall_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(tyndall_fri), "data/Tyndall/tyndall_15yr_fri.csv")
