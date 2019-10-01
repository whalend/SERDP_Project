# Fort Benning

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
shp_list <- list.files("data/FtBenning/RxFireFY1997-2018/", pattern = "[shp]$", full.names = F)

path <- "data/FtBenning/RxFireFY1997-2018/"

shp_list <- strsplit(shp_list, ".shp")

benning_fires <- st_read(paste(path, shp_list[1], ".shp", sep = ""))
# benning_fires <- benning_fires %>%
      # mutate(fYear = str_sub(paste(shp_list[3]), start = 15, end = 18))

projection <- st_crs(benning_fires)
for(shp in shp_list[2:length(shp_list)]){

      sf = st_read(paste(path, shp, ".shp", sep = ""))
      # sf = mutate(sf, fYear = str_sub(paste(shp), start = 15, end = 18))
      sf = st_transform(sf, crs = projection)
      # benning_fires <- st_join(benning_fires, sf)
      assign(shp, sf)
}

benning_fires <- benning_fires %>%
      mutate(inst_name = "Fort Benning",
             fYear = lubridate::year(Date_Burn),
             fMonth = lubridate::month(Date_Burn),
             fCause = NA,
             fType = "prescribed",
             purpose = NA,
             season = NA
             ) %>%
      select(inst_name,
             fArea_ac = Acres,
             fCause, fType,
             fDate = Date_Burn,
             season, purpose, fYear, fMonth)


rx_fires <- rbind(
      # unique(RxFireFY2002QAQC$BURN_DATE)
      RxFireFY2002QAQC %>%
            rename(fDate = BURN_DATE) %>%
            mutate(
                  fDate = as.character(fDate),
                  fDate = case_when(
                        fDate=="12/26/2001, 12/27/2001" ~ "12/26/2001",
                        fDate=="12/03/2001, 12/06/2001" ~ "12/06/2001",
                        fDate=="01/30/2002, 01/31/2002" ~ "01/31/2002",
                        fDate=="02/05/2002, 03/06/2002" ~ "03/06/2002",
                        fDate=="04/07/2002, 04/08/2002" ~ "04/08/2002",
                        fDate=="03/20/2002, 03/28/2002" ~ "03/28/2002",
                        fDate=="04/16/2002, 04/22-24/2002" ~ "04/22/2002",
                  TRUE ~ fDate
            ),
            fDate = as.Date(fDate, format = "%m/%d/%Y")) %>%
            select(fDate, fType = BURN_TYPE, fArea_ac = Acres),

      # unique(RxFireFY2003QAQC$BURN_DATE)
      RxFireFY2003QAQC %>%
            rename(fDate = BURN_DATE) %>%
            mutate(
                  fDate = as.character(fDate),
                  fDate = as.Date(fDate, format = "%m/%d/%Y")
                  ) %>%
            select(fDate, fType = BURN_TYPE, fArea_ac = Acres),

      # unique(RxFireFY2004QAQC$BURN_DATE)
      RxFireFY2004QAQC %>%
            rename(fDate = BURN_DATE) %>%
            mutate(
                  fDate = as.character(fDate),
                  fDate = as.Date(fDate, format = "%m/%d/%Y")
            ) %>%
            select(fDate, fType = BURN_TYPE, fArea_ac = Acres),

      # unique(RxFireFY2005QAQC$BURN_DATE)
      RxFireFY2005QAQC %>%
            rename(fDate = BURN_DATE) %>%
            mutate(
                  fDate = as.character(fDate),
                  fDate = case_when(
                        fDate=="01/11-12/2005" ~ "01/12/2005",
                        fDate=="12/18-19/2004" ~ "12/19/2004",
                        fDate=="04/28-29/2005" ~ "04/29/2005",
                        TRUE ~ fDate
            ),
            fDate = as.Date(fDate, format = "%m/%d/%Y")
            ) %>%
            select(fDate, fType = BURN_TYPE, fArea_ac = Acres),

      # unique(RxFireFY2006QAQC$BURN_DATE)
      RxFireFY2006QAQC %>%
            rename(fDate = BURN_DATE) %>%
            mutate(fDate = as.character(fDate),
                   fDate = case_when(
                         fDate=="02/15-16/2006" ~ "02/15/2006",
                         fDate=="02/14-15/2006" ~ "02/15/2006",
                         fDate=="03/23-24/2006" ~ "03/24/2006",
                         TRUE ~ fDate
                   ),
                   fDate = as.Date(fDate, format = "%m/%d/%Y")
                   ) %>%
            select(fDate, fType = BURN_TYPE, fArea_ac = Acres),


      # unique(RxFireFY2007QAQC$BURN_DATE)
      RxFireFY2007QAQC %>%
            rename(fDate = BURN_DATE) %>%
            mutate(fDate = as.character(fDate),
                   fDate = as.Date(fDate, format = "%m/%d/%Y")
            ) %>%
      select(fDate, fType = BURN_TYPE, fArea_ac = Acres),

      # unique(RxFireFY2008QAQC$date_burn)
      RxFireFY2008QAQC %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2009$date_burn)
      RxFireFY2009 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2010$date_burn)
      RxFireFY2010 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2011$date_burn)
      RxFireFY2011 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2012$date_burn)
      RxFireFY2012 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2013$date_burn)
      RxFireFY2013 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2014$date_burn)
      RxFireFY2014 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2015$date_burn)
      RxFireFY2015 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(date_burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2016$Date_Burn)
      RxFireFY2016 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(Date_Burn)) %>%
            select(fDate, fType, fArea_ac = Acres),

      # unique(RxFireFY2017$Date_Burn)
      RxFireFY2017 %>%
            mutate(fType = "prescribed",
                   fDate = as.Date(Date_Burn)) %>%
            select(fDate, fType, fArea_ac = Acres)
)

# benning_fires
rx_fires <- rx_fires %>%
      mutate(inst_name = "Fort Benning",
             fCause = NA,
             season = NA,
             purpose = NA,
             fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate)
             ) %>%
      select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth)

rx_fires <- rbind(rx_fires, benning_fires)
benning_fires <- rx_fires %>%
      mutate(fType = tolower(fType))
summary(benning_fires)

rm(list = ls(pattern = "Rx"))


# Wildfire shapefiles ####

shp_list <- list.files("data/FtBenning/WildFireFY1997-2018/", pattern = "[shp]$", full.names = F)

path <- "data/FtBenning/WildFireFY1997-2018/"

shp_list <- shp_list[c(1,6:length(shp_list))]

wildfires <- st_read(paste(path, shp_list[1], sep = ""))
wildfires <- wildfires %>%
      rename(fDate = DATE_,
             fArea_ac = ACRES) %>%
      mutate(inst_name = "Fort Benning",
             fType = "wildfire",
             fCause = NA,
             season = NA,
             purpose = NA,
             fYear = NA,
             fMonth = NA,
             fDate = as.character(fDate)
             ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth)

shp_list <- strsplit(shp_list, ".shp")

for(shp in shp_list[2:length(shp_list)]){

      sf = st_read(paste(path, shp, ".shp", sep = ""))
      sf = sf %>%
            rename(fDate = BURN_DATE,
                  fArea_ac = Acres,
                  fType = BURN_TYPE) %>%
            mutate(inst_name = "Fort Benning",
                   fCause = NA,
                   season = NA,
                   purpose = NA,
                   fYear = NA,
                   fMonth = NA,
                   fDate = as.character(fDate)
            ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth)
      # sf = mutate(sf, fYear = str_sub(paste(shp), start = 15, end = 18))
      sf = st_transform(sf, crs = projection)
      wildfires <- rbind(wildfires, sf)
      # assign(shp, sf)
}

unique(wildfires$fDate)
wildfires <- wildfires %>%
      mutate(fDate = case_when(
            fDate=="02/27-28/2007" ~ "02/28/2007",
            fDate=="12/04-05/2006" ~ "12/05/2006",
            fDate=="05/09-11/2007" ~ "05/11/2007",
            fDate=="03/25-26/2007" ~ "03/26/2007",
            fDate=="07/04/2007?" ~ "07/04/2007",
            fDate=="07/14-18/2006" ~ "07/18/2006",
            fDate=="02/14-15/2006" ~ "02/15/2006",
            fDate=="10/2004" ~ "10/01/2004",
            fDate=="10/2001" ~ "10/01/2001",
            fDate=="03/20-21/2004" ~ "03/21/2004",
            fDate=="03/18-21/2004" ~ "03/20/2004",
            fDate=="10/17/2001 - 10/18/2001" ~ "10/18/2001",
            fDate=="11/14/2001 - 11/15/2001" ~ "11/15/2001",
            fDate=="11/08/2001 - 11/09/2001" ~ "11/09/2001",
            fDate=="11/09/2001 - 11/10/2001" ~ "11/10/2001",
            fDate=="10/30/2001 - 11/01/2001" ~ "11/01/2001",
            fDate=="10/25/2001, 10/27/2001" ~ "10/27/2001",
            fDate=="05/02/2001, 05/04/2001" ~ "05/04/2001",
            fDate=="NODATE" ~ "",
            fDate=="NO DATE" ~ "",
            TRUE ~ fDate
      ))

tmp <- wildfires[50:367,]
tmp2 <- wildfires[-(50:367),]

# unique(tmp$fDate)

wildfires <- rbind(
      tmp %>% mutate(fDate = as.Date(fDate, format = "%m/%d/%Y")),
      tmp2 %>% mutate(fDate = as.Date(fDate))
)

benning_fires <- rbind(
      benning_fires,
      wildfires
)

## Using growing season start of March 15th based on Gordon data
benning_fires <- benning_fires %>%
      mutate(fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate),
             tmp = as.numeric(paste(fMonth, lubridate::day(fDate), sep = "")),
             season = if_else(
                   between(tmp, 315,930), "growing","dormant")
             ) %>%
      select(-tmp)

summary(benning_fires)
filter(benning_fires, is.na(fDate))

st_write(benning_fires, "data/FtBenning/benning_fires_2000_2018.shp")


rm(list=ls(pattern="shp"))
rm(path)
rm(sf); rm(projection)
rm(list=ls(pattern="tmp"))
rm(rx_fires)
rm(wildfires)


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
benning_plots <- filter(plot_locations, inst=="benning")

benning2018 <- filter(benning_plots, visit_yr==2018)

benning_fires$FID <- seq(1, nrow(benning_fires), 1)


fires2018 <- st_join(st_transform(benning2018, crs = st_crs(benning_fires)),
                     benning_fires %>%
                           filter(fDate > "2003-07-28",
                                  fDate < max(benning2018$visit_date))
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

benning_fri <- fri2018
summary(benning_fri)

## Write shapefile
st_write(benning_fri, "data/FtBenning/benning_15yr_fri.shp")

## Write data frame
readr::write_csv(as.data.frame(benning_fri), "data/FtBenning/benning_15yr_fri.csv")
