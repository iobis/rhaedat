library(rhaedat)
library(ggplot2)
library(dplyr)

# override areas

list_areas <- list(
  list(name = "anz", xlim = c(110, 185), ylim = c(-50, -10))
)

df <- events() %>%
  filter(countryName %in% c("AUSTRALIA", "NEW ZEALAND"))

stats <- df %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$events)
scale_upper <- (ceiling(max(stats$events) / 5) * 5)
events_scale <- scale_radius(
  limits = c(1, scale_upper),
  range = c(1.5, 8),
  breaks = c(1, seq(5, scale_upper, by = 5))
)

color <- "#aaaaaa" #ff704d

for (area in list_areas) {
  path <- paste0("demo/output/events_", area$name, ".png")
  message(path)
  make_map(df, area = area, type = "events", scale = events_scale, line_color = "black", line_width = 0.75, color = color) +
    labs(title = "Events")
  ggsave(path, height = 8, width = 12, scale = 0.8)
}
