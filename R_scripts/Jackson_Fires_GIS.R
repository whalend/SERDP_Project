# Fort Jackson

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
shp_list <- list.files("data/FtJackson/prescribedBurn/", pattern = "[shp]$", full.names = F)
path <- "data/FtJackson/prescribedBurn/"

yrs_15_18_list <- shp_list[2:5]

yr14 <- st_read(paste(path, shp_list[1], sep = ""))
yr10 <- st_read(paste(path, shp_list[14], sep = ""))
yr09 <- st_read(paste(path, shp_list[13], sep = ""))

yrs_02_08 <- shp_list[6:12]

# shp_list <- strsplit(shp_list, ".shp")


jackson_fires <- yr14
names(jackson_fires) <- tolower(names(jackson_fires))
jackson_fires <- jackson_fires %>%
      select(fDate = date_, fArea_ac = acres, fType = burn_type, comments) %>%
      mutate(fDate2 = as.character(fDate),
             fDate = lubridate::dmy(as.character(fDate)),
             fMonth = lubridate::month(fDate),
             fYear = lubridate::year(fDate),
             inst_name = "Fort Jackson",
             fCause = NA,
             season = case_when(
                   between(fMonth, 3,9) ~ "growing",
                   TRUE ~ "dormant"),
             purpose = NA
             ) %>%
      select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments)

filter(jackson_fires, fMonth==3)$fType
filter(jackson_fires, fMonth==8)$fType


names(yr09) <- tolower(names(yr09))
jackson_fires <- rbind(
      jackson_fires,
      yr09 %>%
            select(fDate = date_, fArea_ac = acres, fType = burn_type, comments) %>%
            mutate(fDate = lubridate::ymd(as.character(fDate)),
                   fMonth = lubridate::month(fDate),
                   fYear = lubridate::year(fDate),
                   inst_name = "Fort Jackson",
                   fCause = NA,
                   fType = tolower(as.character(fType)),
                   season = case_when(
                         between(fMonth, 3,9) ~ "growing",
                         TRUE ~ "dormant"),
                   purpose = NA
            ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments)
)

names(yr10) <- tolower(names(yr10))
jackson_fires <- rbind(
      jackson_fires,
      yr10 %>%
            select(fDate = date_, fArea_ac = acreage_, fType = burn_type, comments) %>%
            mutate(fDate = lubridate::ymd(as.character(fDate)),
                   fMonth = lubridate::month(fDate),
                   fYear = lubridate::year(fDate),
                   inst_name = "Fort Jackson",
                   fCause = NA,
                   fType = tolower(as.character(fType)),
                   season = case_when(
                         between(fMonth, 3,9) ~ "growing",
                         TRUE ~ "dormant"),
                   purpose = NA
            ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments)
)

summary(jackson_fires)




projection <- st_crs(jackson_fires)
for(shp in yrs_02_08){

      sf = st_read(paste(path, shp, sep = ""))
      names(sf) = tolower(names(sf))

      sf = sf %>%
            select(fDate = date_, fArea_ac = acreage, fType = burn_type,
                   comments) %>%
            mutate(comments = as.character(comments),
                   fDate2 = as.character(fDate),
                   fDate = lubridate::ymd(as.character(fDate)),
                   inst_name = "Fort Jackson",
                   fCause = NA,
                   season = NA,
                   purpose = NA,
                   fYear = lubridate::year(fDate),
                   fMonth = lubridate::month(fDate)
            ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments)

      sf = st_transform(sf, crs = projection)
      jackson_fires <- rbind(jackson_fires, sf)
      # assign(shp, sf)
}

# summary(jackson_fires)
# unique(jackson_fires$fYear)
# sf <- st_read(paste(path, yrs_15_18_list[1], sep = ""))
for(shp in yrs_15_18_list){

      sf = st_read(paste(path, shp, sep = ""))
      names(sf) = tolower(names(sf))

      sf = sf %>%
            select(fDate = yr_burned,
                   fArea_ac = acres,
                   season = burn_seaso,
                   comments) %>%
            mutate(fDate = as.character(fDate),
                   fDate = case_when(
                         nchar(fDate)==5 ~ paste(0, fDate, sep = ""),
                         TRUE ~ fDate),
                   fDate2 = fDate,
                   fDate = lubridate::mdy(fDate),
                   comments = as.character(comments),
                   inst_name = "Fort Jackson",
                   fCause = NA,
                   purpose = NA,
                   fType = NA,
                   fYear = lubridate::year(fDate),
                   fMonth = lubridate::month(fDate)
            ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments)

      sf = st_transform(sf, crs = projection)
      jackson_fires <- rbind(jackson_fires, sf)

}

# str(jackson_fires)
# summary(jackson_fires)
#
# unique(jackson_fires$fType)
# unique(jackson_fires$season)
# unique(jackson_fires$comments)
# unique(jackson_fires$purpose)
# unique(jackson_fires$fCause)

jackson_fires <- jackson_fires %>%
      mutate(fType = tolower(as.character(fType)),
             season = tolower(as.character(season)),
             )

jackson_fires <- jackson_fires %>%
      mutate(
            season = case_when(
                  fType == "growing" ~ "growing",
                  fType == "dormant" ~ "dormant",
                  TRUE ~ season
            ),
            purpose = case_when(
                  fType == "seed bed" ~ "seed bed",
                  fType == "shelterwood" ~ "shelterwood",
                  comments == "Research burn" ~ "research burn",
                  comments == "FJFD joint wildfire training" ~ "training",
                  TRUE ~ "NA"
            ),
            fCause = case_when(
                  comments == "helicopter" ~ "helicopter",
                  TRUE ~ "NA"
            ),
            fType = case_when(
                  comments == "wildfire" ~ "wildfire",
                  TRUE ~ "prescribed"
            )
            ) %>%
      select(-comments)

rm(list=ls(pattern = "yr"))
rm(projection)
rm(sf)

summary(jackson_fires)
st_write(jackson_fires, "data/FtJackson/jackson_fires_2002_2018.shp")


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

# unique(plot_locations$inst)
jackson_plots <- filter(plot_locations, inst=="jackson")

# jackson2017 <- filter(jackson_plots, visit_yr==2017)# no 2017 visit
jackson2018 <- filter(jackson_plots, visit_yr==2018)

jackson_fires$FID <- seq(1, nrow(jackson_fires), 1)


fires2018 <- st_join(st_transform(jackson2018, crs = st_crs(jackson_fires)),
                     jackson_fires %>%
                           filter(fDate > "2003-07-13",
                                  fDate < max(jackson2018$visit_date))
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


jackson_fri <- fri2018
summary(jackson_fri)

## Write shapefile
st_write(jackson_fri, "data/FtJackson/jackson_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(jackson_fri), "data/FtJackson/jackson_15yr_fri.csv")
