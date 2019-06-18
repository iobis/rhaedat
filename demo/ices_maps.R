library(rhaedat)
library(ggplot2)
library(dplyr)

# override areas

list_areas <- list(
  list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75))
)

line_colors <- c("black")

df <- events_ices()

# full maps

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$years)
years_scale <- scale_radius(limits = c(1, 35), range = c(1.5, 8), breaks = c(1, 10, 20, 30))

for (lineColor in line_colors) {
  for (syn in list_syndromes) {
    for (area in list_areas) {
      df2 <- df %>% filter(syndromeName == syn$name)
      path <- paste0("demo/output/years_", area$name, "_", syn$name, "_", lineColor, ".png")
      message(path)
      makeMap(df2, area = area, type = "years", scale = years_scale, lineColor = lineColor, lineWidth = 0.75, color = "#ff704d")
      ggsave(path, height = 8, width = 12, scale = 0.8)
    }    
  }
}

### 5 year period maps

years_scale2 <- scale_radius(limits = c(1, 5), range = c(1.5, 4))

for (lineColor in line_colors) {
  for (syn in list_syndromes) {
    for (area in list_areas) {
      df2 <- df %>% filter(syndromeName == syn$name)
      path <- paste0("demo/output/years_5years_", area$name, "_", syn$name, "_", lineColor, ".png")
      message(path)
      makeMap(df2, area = area, type = "years", lineColor = lineColor, lineWidth = 0.65, color = "#ff704d", faceted = TRUE, scale = years_scale2)#, scale = years_scale2
      ggsave(path, height = 8, width = 12, scale = 0.8)
    }    
  }
}
