library(tidyverse)

bigelow_whelan <- read_csv("~/Downloads/doi_10.5061_dryad.vm692vr__v1/Fire_intensity_2016.csv")
str(bigelow_whelan)

summary(bigelow_whelan$`K Type Max`)
hist(bigelow_whelan$`K Type Max`)
range(bigelow_whelan$`K Type Max`)



summary(bigelow_whelan$`Seconds Over`)
hist(bigelow_whelan$`Seconds Over`)
abline(v = mean(bigelow_whelan$`Seconds Over`))
abline(v = median(bigelow_whelan$`Seconds Over`))

## Fires grouped by date, i.e. plot within date
library(lubridate)
bigelow_whelan <- bigelow_whelan %>%
      mutate(date_time = mdy_hm(`First Temp Spike`),
             fire_date = date(date_time)
             )

bigelow_whelan %>%
      group_by(fire_date) %>%
      summarise(maxt = max(`K Type Max`),
                avg_maxt = mean(`K Type Max`),
                # r_maxt = range(`K Type Max`),
                max_sabv = max(`Seconds Over`),
                avg_sabv = mean(`Seconds Over`))
