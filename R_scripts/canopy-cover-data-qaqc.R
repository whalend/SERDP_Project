#' # Script for doing QA/QC on the canopy cover data

#' ## Canopy Cover Data
#'
#+ load canopy cover data ####
canopy_cover <- read_csv("~/Dropbox (UF)/SERDP-Project/data/densiometer-canopy-cover.csv")
summary(canopy_cover)
canopy_cover$date <- as.Date(canopy_cover$date, format = "%m/%d/%y")
write_csv(canopy_cover, "data/canopy-cover.csv")

canopy_cover_summary <- canopy_cover %>%
      group_by(installation, plot_id) %>%
      summarise(avg_canopy_cover = mean(pct_canopy_cover),
                sd_canopy_cover = sd(pct_canopy_cover),
                n_obs = 12)

#+ join data summarised at plot level ####
plot_data1 <- left_join(ungroup(plot_data), ungroup(canopy_cover_summary))
plot_data1 <- left_join(ungroup(plot_data), ungroup(quadrat_summaries))