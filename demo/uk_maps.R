library(rhaedat)
library(ggplot2)
library(dplyr)
library(mapdata)
library(rnaturalearth)

area <- list(name = "uk", xlim = c(-13, 3), ylim = c(49, 62))
list_syndromes2 <- list_syndromes[c(2, 3, 6, 8)]
ev <- events_uk()

world_data <- ne_countries(type = "countries", scale = "large")
world <- geom_polygon(data = world_data, mapping = aes(x = long, y = lat, group = group), fill = "gray80", colour = "gray80", size = 0)
  
make_maps <- function(start = 0, end = as.integer(format(Sys.Date(), "%Y"))) {
  df <- ev %>% filter(eventYear >= start & eventYear <= end)
  stats <- df %>% 
    group_by(syndromeName, longitude, latitude) %>%
    summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
  #events_scale <- scale_radius(limits = range(stats$events), range = c(1.5, 8), breaks = c(1, seq(5, max(stats$events), by = 5)))
  years_scale <- scale_radius(limits = c(min(stats$years), ceiling(max(stats$years) / 5) * 5), range = c(1.5, 8), breaks = c(1, seq(5, ceiling(max(stats$years) / 5) * 5, by = 5)))
  for (syn in list_syndromes2) {
    df2 <- df %>% filter(syndromeName == syn$name)
    #events_path <- paste0("demo/output/events_", area$name, "_", syn$name, "_", start, "-", end, ".png")
    years_path <- paste0("demo/output/years_", area$name, "_", syn$name, "_", start, "-", end, ".png")
    title <- paste0(syn$name, " (", ifelse(start == 0, "- ", paste0(start, " - ")), end, ")")
    message(years_path)
    make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = syn$color, world = world) +
      ggtitle(title)
    ggsave(years_path, height = 8, width = 12, scale = 0.8)
  }
}

make_maps()
make_maps(1998, 2007)
make_maps(2008, 2017)
make_maps(2000, 2017)



