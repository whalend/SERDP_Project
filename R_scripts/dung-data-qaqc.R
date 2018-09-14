#' # Script for doing QA/QC on dung data

#' ## Host Abundance Estimates
#+ dung data ####
dung_data <- read_csv("data/raw_data/dung.csv")

# filter(plotid %in% c("n","s","e"))

anti_join(plot_visit_data, dung_data)

d <- dung_data %>%
      group_by(installation, plot_id) %>%
      summarise(n_transect = n_distinct(location))
summary(d)
