#' # Script for doing QA/QC on tick data


#' Tick Abundance Estimates

#+ tick data ####
tick_data <- read_csv("data/raw_data/ticks.csv")

tick_data$plot_id <- tolower(tick_data$plot_id)
tick_data$plot_id[tick_data$plot_id=="c"] <- "c1"

tick_data <- left_join(
tick_data,
select(plot_data, installation,plot_id,fire_year:full_names),
by = c("installation", "plot_id")
)
summary(tick_data)
filter(tick_data, is.na(years_since_fire))
# "extra" ticks at Moody AFB were from an area last burned in 2005
tick_data$years_since_fire[tick_data$installation=="moody"] <- 2017-2005


ggplot(tick_data %>%
group_by(installation,years_since_fire,Species) %>%
summarise(tick_number = sum(count)),
aes(as.factor(years_since_fire), tick_number)) +
# geom_point(position = "jitter") +
geom_bar(fill = "#124873", stat = "identity", position = "dodge") +
# facet_grid(installation~., scales = "free_y") +
xlab("Years since fire") +
ylab("Tick abundance") +
theme_bw() +
theme(axis.text = element_text(size = 18),
axis.title = element_text(size = 22))

ggplot(tick_data %>%
group_by(full_names, years_since_fire, Species) %>%
summarise(tick_number = sum(count)),
aes(as.factor(years_since_fire), tick_number, fill = Species)) +
# geom_point(position = "jitter") +
geom_bar(stat = "identity", position = "dodge") +
facet_grid(full_names~., scales = "free_y") +
xlab("Years since fire") +
ylab("Tick abundance") +
theme_bw() +
theme(axis.text = element_text(size = 18),
axis.title = element_text(size = 22))
