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

# library(lubridate)


# Plot All Sensor Data ----------------------------------------------------
par(mfrow=c(3,1))
plot(ground$time, ground$tempC)
plot(twelve$time, twelve$tempC)
plot(fifty$time, fifty$tempC)


# Slice Fire Temperature Data ---------------------------------------------
fire1 <- rbind(
      filter(ground, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "0cm (ground)"),
      filter(twelve, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "12cm"),
      filter(fifty, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "50cm")
)
fire1$fuel_type <- "standing"
# ggplot(fire1, aes(time, tempC, col = location)) +
#       geom_line() +
#       theme_bw() +
#       ggtitle("Standing Fuel - Bivens")


fire2 <- rbind(
      filter(ground, between(time, 37630,38000) & between(tempC, 20,1500)) %>%
            mutate(location = "0cm (ground)"),
      filter(twelve, between(time, 37630,38000) & between(tempC, 20,1500)) %>%
            mutate(location = "12cm"),
      filter(fifty, between(time, 37630,38000) & between(tempC, 20,1500)) %>%
            mutate(location = "50cm")
)
fire2$fuel_type <- "litter"
# ggplot(fire2, aes(time, tempC, col = location)) +
#       geom_line() +
#       theme_bw() +
#       ggtitle("Litter Fuel - Bivens")


# Plot Temperature by Fuel Type -------------------------------------------
ggplot(rbind(fire1,fire2), aes(time, tempC, col = location)) +
      geom_line() +
      theme_bw() +
      facet_grid(.~ fuel_type, scales = "free_x") +
      ylab("Temperature ºC") +
      xlab("Time of Day (24 hour)") +
      ggtitle("Test Burn")
ggsave("figures/bivens-test-burn.png", dpi = 600)

