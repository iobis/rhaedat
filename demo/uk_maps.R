library(rhaedat)
library(ggplot2)
library(dplyr)
library(mapdata)

area <- list(name = "uk", xlim = c(-13, 3), ylim = c(49, 62))
list_syndromes2 <- list_syndromes[c(2, 3, 6, 8)]
ev <- events_uk()

make_maps <- function(start = 0, end = 2017) {
  df <- ev %>% filter(eventYear >= start & eventYear <= end)
  stats <- df %>% 
    group_by(syndromeName, longitude, latitude) %>%
    summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
  events_scale <- scale_radius(limits = range(stats$events), range = c(1.5, 8), breaks = c(1, seq(5, max(stats$events), by = 5)))
  world <- borders("worldHires", colour = "gray80", fill = "gray80", size = 0, xlim = area$xlim, ylim = area$ylim)
  for (syn in list_syndromes2) {
    df2 <- df %>% filter(syndromeName == syn$name)
    path <- paste0("demo/output/events_", area$name, "_", syn$name, "_", start, "-", end, ".png")
    title <- paste0(to_toxin(syn$name), " (", ifelse(start == 0, "- ", paste0(start, " - ")), end, ")")
    message(path)
    make_map(df2, area = area, type = "events", scale = events_scale, line_color = "black", line_width = 0.75, color = syn$color, world = world) +
      ggtitle(title)
    ggsave(path, height = 8, width = 12, scale = 0.8)
  }
}

make_maps(0, 2017)
make_maps(1998, 2007)
make_maps(2008, 2017)
make_maps(2000, 2017)



