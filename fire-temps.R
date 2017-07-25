# Fire Temperatures
library(plyr); library(dplyr)
library(readr); library(ggplot2)


# Load Bivens Test Burn Data ----------------------------------------------
ground <- read_csv("data/fire-temperatures/bivens-testburn-20170407-ground.csv")
names(ground) <- c("date","time","tempC")# rename columns
# str(ground)

twelve <- read_csv("data/fire-temperatures/bivens-testburn-20170407-12cm.csv")
names(twelve) <- c("date","time","tempC")

fifty <- read_csv("data/fire-temperatures/bivens-testburn-20170407-50cm.csv")
names(fifty) <- c("date","time","tempC")

par(mfrow=c(3,1))
# plot(ground$time, ground$tempC)
# plot(twelve$time, twelve$tempC)
# plot(fifty$time, fifty$tempC)

# Slice Test Burn Temperature ####
test_burn_standing <- rbind(
      filter(ground, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "0cm"),
      filter(twelve, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "12cm"),
      filter(fifty, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "50cm")
)
test_burn_standing$fuel_type <- "standing"
# ggplot(fire1, aes(time, tempC, col = location)) +
#       geom_line() +
#       theme_bw() +
#       ggtitle("Standing Fuel - Bivens")


test_burn_thatch <- rbind(
      filter(ground, between(time, 37630,38000)) %>%
            mutate(location = "0cm"),
      filter(twelve, between(time, 37630,38000)) %>%
            mutate(location = "12cm"),
      filter(fifty, between(time, 37630,38000)) %>%
            mutate(location = "50cm")
)
test_burn_thatch$fuel_type <- "litter"
# ggplot(fire2, aes(time, tempC, col = location)) +
#       geom_line() +
#       theme_bw() +
#       ggtitle("Litter Fuel - Bivens")


# library(lubridate)


# Load FABIO Burn Data ----------------------------------------------------
fabio_burns_biomass <- readxl::read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "FABIO_Burns")
fabio_burns_biomass <- select(fabio_burns_biomass, fire_id:biomass_type)

# Experimental Burns July 5, 2017: Fire IDs 1-8 ####

# load data from FABIO 1 sensors
fabio1_0cm <- read_csv("data/fire-temperatures/fabio1-0cm-20170705.csv")
names(fabio1_0cm) <- c("date","time","tempC")
fabio1_0cm <- mutate(fabio1_0cm, location = "0cm", probe_type = "old", fabio_id = as.factor(1))
fabio1_25cm <- read_csv("data/fire-temperatures/fabio1-25cm-20170705.csv")
names(fabio1_25cm) <- c("date","time","tempC")
fabio1_25cm <- mutate(fabio1_25cm, location = "25cm", probe_type = "old", fabio_id = as.factor(1))
fabio1_50cm <- read_csv("data/fire-temperatures/fabio1-50cm-20170705.csv")
names(fabio1_50cm) <- c("date","time","tempC")
fabio1_50cm <- mutate(fabio1_50cm, location = "50cm", probe_type = "old", fabio_id = as.factor(1))

# load data from FABIO 2 sensors
fabio2_0cm <- read_csv("data/fire-temperatures/fabio2-0cm-20170705.csv")
names(fabio2_0cm) <- c("date","time","tempC")
fabio2_0cm <- mutate(fabio2_0cm, location = "0cm", probe_type = "old", fabio_id = as.factor(2))
fabio2_25cm <- read_csv("data/fire-temperatures/fabio2-25cm-20170705.csv")
names(fabio2_25cm) <- c("date","time","tempC")
fabio2_25cm <- mutate(fabio2_25cm, location = "25cm", probe_type = "old", fabio_id = as.factor(2))
fabio2_50cm <- read_csv("data/fire-temperatures/fabio2-50cm-20170705.csv")
names(fabio2_50cm) <- c("date","time","tempC")
fabio2_50cm <- mutate(fabio2_50cm, location = "50cm", probe_type = "old", fabio_id = as.factor(2))

# plot all data from sensors
par(mfrow=c(3,1))
plot(fabio1_0cm$time, fabio1_0cm$tempC, main = "FABIO 1 Burns 7/5/2017")
plot(fabio1_25cm$time, fabio1_25cm$tempC)
plot(fabio1_50cm$time, fabio1_50cm$tempC)

plot(fabio2_0cm$time, fabio2_0cm$tempC, main = "FABIO 2 Burns 7/5/2017")
plot(fabio2_25cm$time, fabio2_25cm$tempC)
plot(fabio2_50cm$time, fabio2_50cm$tempC)

# Slice Experimental Burn Temperatures: Fire IDs 1-8 ####
fireid1 <- rbind(
      filter(fabio1_0cm, between(time, 45330,45400)),
      filter(fabio1_25cm, between(time, 45330,45400)),
      filter(fabio1_50cm, between(time, 45330,45400))
      ) %>%
      mutate(fire_id = 1)
fireid1 <- left_join(fireid1, fabio_burns, by = "fire_id")

fireid2 <- rbind(
      filter(fabio2_0cm, between(time, 47500,48000)),
      filter(fabio2_25cm, between(time, 47500,48000)),
      filter(fabio2_50cm, between(time, 47500,48000))
      ) %>%
      mutate(fire_id = 2)
fireid2 <- left_join(fireid2, fabio_burns, by = "fire_id")

fireid3 <- rbind(
      filter(fabio2_0cm, between(time, 53820,53940)),
      filter(fabio2_25cm, between(time, 53820,53940)),
      filter(fabio2_50cm, between(time, 53820,53940))
      ) %>%
      mutate(fire_id = 3)
fireid3 <- left_join(fireid3, fabio_burns, by = "fire_id")

fireid4 <- rbind(
      filter(fabio1_0cm, between(time, 54930,55000)),
      filter(fabio1_25cm, between(time, 54930,55000)),
      filter(fabio1_50cm, between(time, 54930,55000))
      ) %>%
      mutate(fire_id = 4)
fireid4 <- left_join(fireid4, fabio_burns, by = "fire_id")

fireid5 <- rbind(
      filter(fabio2_0cm, between(time, 55900,56100)),
      filter(fabio2_25cm, between(time, 55900,56100)),
      filter(fabio2_50cm, between(time, 55900,56100))
      ) %>%
      mutate(fire_id = 5)
fireid5 <- left_join(fireid5, fabio_burns, by = "fire_id")

fireid6 <- rbind(
      filter(fabio1_0cm, between(time, 59600,60240)),
      filter(fabio1_25cm, between(time, 59600,60240)),
      filter(fabio1_50cm, between(time, 59600,60240))
      ) %>%
      mutate(fire_id = 6)
fireid6 <- left_join(fireid6, fabio_burns, by = "fire_id")

fireid7 <- rbind(
      filter(fabio_blanding_0cm, between(time, 48500,48880)),
      filter(fabio_blanding_25cm, between(time, 48500,48880)),
      filter(fabio_blanding_50cm, between(time, 48500,48880))
      ) %>%
      mutate(fire_id = 7)
fireid7 <- left_join(fireid7, fabio_burns, by = "fire_id")

fireid8 <- rbind(
      filter(fabio_blanding_0cm, between(time, 49620,50200)),
      filter(fabio_blanding_25cm, between(time, 49620,50200)),
      filter(fabio_blanding_50cm, between(time, 49620,50200))
      ) %>%
      mutate(fire_id = 8)
fireid8 <- left_join(fireid8, fabio_burns, by = "fire_id")


# Experimental Burns July 12, 2017: Fire IDs 9-17 ####

# load data from FABIO 1 sensors
fabio1_0cm_new <- read_csv("data/fire-temperatures/fabio1-0cm-new-20170712.csv")
names(fabio1_0cm_new) <- c("date","time","tempC")
fabio1_0cm_old <- read_csv("data/fire-temperatures/fabio1-0cm-old-20170712.csv")
names(fabio1_0cm_old) <- c("date","time","tempC")
fabio1_25cm_new <- read_csv("data/fire-temperatures/fabio1-25cm-new-20170712.csv")
names(fabio1_25cm_new) <- c("date","time","tempC")
fabio1_25cm_old <- read_csv("data/fire-temperatures/fabio1-25cm-old-20170712.csv")
names(fabio1_25cm_old) <- c("date","time","tempC")
fabio1_50cm_new <- read_csv("data/fire-temperatures/fabio1-50cm-new-20170712.csv")
names(fabio1_50cm_new) <- c("date","time","tempC")
fabio1_50cm_old <- read_csv("data/fire-temperatures/fabio1-50cm-old-20170712.csv")
names(fabio1_50cm_old) <- c("date","time","tempC")

# load data from FABIO 2 sensors
fabio1_0cm_new
fabio1_0cm_old
fabio1_25cm_new
fabio1_25cm_old
fabio1_50cm_new
fabio1_50cm_old

# Load FABIO Prescribed Burn Data 2017/07/06 ------------------------------
fabio_blanding_0cm <- read_csv("data/fire-temperatures/fabio-bland-c-0cm-20170706.csv")
names(fabio_blanding_0cm) <- c("date","time","tempC")
fabio_blanding_0cm <- mutate(fabio_blanding_0cm, location = "0cm", probe_type = "old")
fabio_blanding_25cm <- read_csv("data/fire-temperatures/fabio-bland-c-25cm-20170706.csv")
names(fabio_blanding_25cm) <- c("date","time","tempC")
fabio_blanding_25cm <- mutate(fabio_blanding_25cm, location = "25cm", probe_type = "old")
fabio_blanding_50cm <- read_csv("data/fire-temperatures/fabio-bland-c-50cm-20170706.csv")
names(fabio_blanding_50cm) <- c("date","time","tempC")
fabio_blanding_50cm <- mutate(fabio_blanding_50cm, location = "50cm", probe_type = "old")

par(mfrow=c(3,1))
plot(fabio_blanding_0cm$time, fabio_blanding_0cm$tempC, main = "FABIO Prescribed Fire 7/6/2017")
plot(fabio_blanding_25cm$time, fabio_blanding_25cm$tempC)
plot(fabio_blanding_50cm$time, fabio_blanding_50cm$tempC)

# Slice FABIO Prescribed Fire Temperature Data ####
fabio_blanding_prescribe <- rbind(
      filter(fabio_blanding_0cm, between(time, 45000,45150)) %>%
            mutate(location = "0cm"),
      filter(fabio_blanding_25cm, between(time, 45000,45150)) %>%
            mutate(location = "25cm"),
      filter(fabio_blanding_50cm, between(time, 45000,45150)) %>%
            mutate(location = "50cm")
)


# Load Flame Height Sensor Data -------------------------------------------
flame_veg_data <- readxl::read_excel("~/Dropbox (UF)/SERDP-Project/data/serdp-plot-data.xlsx", sheet = "flame_sensors")

flame1_0cm <- read_csv("data/fire-temperatures/bland-c-f1-0cm-20170706.csv")
names(flame1_0cm) <- c("date","time","tempC")
flame1_25cm <- read_csv("data/fire-temperatures/bland-c-f1-25cm-20170706.csv")
names(flame1_25cm) <- c("date","time","tempC")
flame1_50cm <- read_csv("data/fire-temperatures/bland-c-f1-50cm-20170706.csv")
names(flame1_50cm) <- c("date","time","tempC")

flame3_0cm <- read_csv("data/fire-temperatures/bland-c-f3-0cm-20170706.csv")
names(flame3_0cm) <- c("date","time","tempC")
flame3_25cm <- read_csv("data/fire-temperatures/bland-c-f3-25cm-20170706.csv")
names(flame3_25cm) <- c("date","time","tempC")
# flame3_50cm <- read_csv("data/fire-temperatures/bland-c-f3-50cm-20170706.csv")
# names(flame3_50cm) <- c("date","time","tempC")
## This logger had failure in correctly recording temperature.

flame4_0cm <- read_csv("data/fire-temperatures/bland-c-f4-0cm-20170706.csv")
names(flame4_0cm) <- c("date","time","tempC")
flame4_25cm <- read_csv("data/fire-temperatures/bland-c-f4-25cm-20170706.csv")
names(flame4_25cm) <- c("date","time","tempC")
flame4_50cm <- read_csv("data/fire-temperatures/bland-c-f4-50cm-20170706.csv")
names(flame4_50cm) <- c("date","time","tempC")

flame5_0cm <- read_csv("data/fire-temperatures/bland-c-f5-0cm-20170706.csv")
names(flame5_0cm) <- c("date","time","tempC")
flame5_25cm <- read_csv("data/fire-temperatures/bland-c-f5-25cm-20170706.csv")
names(flame5_25cm) <- c("date","time","tempC")
flame5_50cm <- read_csv("data/fire-temperatures/bland-c-f5-50cm-20170706.csv")
names(flame5_50cm) <- c("date","time","tempC")

flame6_0cm <- read_csv("data/fire-temperatures/bland-c-f6-0cm-20170706.csv")
names(flame6_0cm) <- c("date","time","tempC")
flame6_25cm <- read_csv("data/fire-temperatures/bland-c-f6-25cm-20170706.csv")
names(flame6_25cm) <- c("date","time","tempC")
flame6_50cm <- read_csv("data/fire-temperatures/bland-c-f6-50cm-20170706.csv")
names(flame6_50cm) <- c("date","time","tempC")

flame7_0cm <- read_csv("data/fire-temperatures/bland-c-f7-0cm-20170706.csv")
names(flame7_0cm) <- c("date","time","tempC")
flame7_25cm <- read_csv("data/fire-temperatures/bland-c-f7-25cm-20170706.csv")
names(flame7_25cm) <- c("date","time","tempC")
flame7_50cm <- read_csv("data/fire-temperatures/bland-c-f7-50cm-20170706.csv")
names(flame7_50cm) <- c("date","time","tempC")

flame8_0cm <- read_csv("data/fire-temperatures/bland-c-f8-0cm-20170706.csv")
names(flame8_0cm) <- c("date","time","tempC")
flame8_25cm <- read_csv("data/fire-temperatures/bland-c-f8-25cm-20170706.csv")
names(flame8_25cm) <- c("date","time","tempC")
flame8_50cm <- read_csv("data/fire-temperatures/bland-c-f8-50cm-20170706.csv")
names(flame8_50cm) <- c("date","time","tempC")


# Slice Fire Temperature Data ---------------------------------------------


flame1 <- rbind(
      filter(flame1_0cm, between(time, 43200,43400)) %>%
            mutate(location = "0cm (ground)"),
      filter(flame1_25cm, between(time, 43200,43400)) %>%
            mutate(location = "25cm"),
      filter(flame1_50cm, between(time, 43200,43400)) %>%
            mutate(location = "50cm")
      ) %>%
      mutate(id = "flame1")
flame1 <- left_join(flame1, filter(flame_veg_data, date == "2017-07-06"),
                    by = "id")

flame6 <- rbind(
      filter(flame6_0cm, between(time, 43280,43700)) %>%
            mutate(location = "0cm (ground)"),
      filter(flame6_25cm, between(time, 43280,43700)) %>%
            mutate(location = "25cm"),
      filter(flame6_50cm, between(time, 43280,43700)) %>%
            mutate(location = "50cm")
      ) %>%
      mutate(id = "flame6")
flame6 <- left_join(flame6, filter(flame_veg_data, date == "2017-07-06"),
                    by = "id")

flame8 <- rbind(
      filter(flame8_0cm, between(time, 40800,41100)) %>%
            mutate(location = "0cm (ground)"),
      filter(flame8_25cm, between(time, 40800,41100)) %>%
            mutate(location = "25cm"),
      filter(flame8_50cm, between(time, 40800,41100)) %>%
            mutate(location = "50cm")
      ) %>%
      mutate(id = "flame8")
flame8 <- left_join(flame8, filter(flame_veg_data, date == "2017-07-06"),
                    by = "id")


# Plot Experimental Fire Temperatures -------------------------------------
ggplot(rbind(test_burn_standing,test_burn_thatch), aes(time, tempC, col = location)) +
      geom_line() +
      theme_bw() +
      facet_grid(.~ fuel_type, scales = "free_x") +
      ylab("Temperature ºC") +
      xlab("Time of Day (24 hour)") +
      ggtitle("Test Burn")
# ggsave("figures/bivens-test-burn.png", dpi = 600)

filter(fireid1, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(fireid2, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(fireid3, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
fireid4 %>% group_by(location) %>%
      summarize(max_temp = max(tempC))
fireid5 %>% group_by(location) %>%
      summarize(max_temp = max(tempC))
fireid6 %>% group_by(location) %>%
      summarize(max_temp = max(tempC))
filter(fireid7, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(fireid8, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))


# Plot Prescribed Fire Temperatures ---------------------------------------
ggplot(fabio_blanding_prescribe, aes(time, tempC, col = location)) +
      geom_line() +
      theme_bw() +
      # facet_grid(.~ fuel_type, scales = "free_x") +
      ylab("Temperature ºC") +
      xlab("Time of Day (24 hour)") +
      ggtitle("FABIO Prescribed Fire Temperatures 7/6/2017")


filter(fabio_blanding_prescribe, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(flame1, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(flame6, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(flame8, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))

