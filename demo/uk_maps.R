library(rhaedat)
library(ggplot2)
library(dplyr)

area <- list(name = "uk", xlim = c(-13, 3), ylim = c(49, 62))

mapSyndromes <- list(
  list(name = "ASP"),
  list(name = "AZP"),
  list(name = "Aerosolized toxins effects"),
  list(name = "Cyanobacterial toxins effects"),
  list(name = "DSP"),
  list(name = "NSP"),
  list(name = "OTHER"),
  list(name = "PSP")
)

lineColors <- c("black")

### 2005 - 2017

df <- ukEvents() %>% filter(eventYear >= 2005 & eventYear <= 2017)

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$years)
years_scale <- scale_radius(limits = c(1, 13), range = c(1.5, 8), breaks = c(1, 5, 10, 13))

for (lineColor in lineColors) {
  for (syn in mapSyndromes) {
    df2 <- df %>% filter(syndromeName == syn$name)
    path <- paste0("demo/output/years_", area$name, "_", syn$name, "_2005-2017_", lineColor, ".png")
    message(path)
    makeMap(df2, area = area, type = "years", scale = years_scale, lineColor = lineColor, lineWidth = 0.75, color = "#ff704d")
    ggsave(path, height = 8, width = 12, scale = 0.8)
  }
}

### 2000 - 2017

df <- events_uk() %>% filter(eventYear >= 2000 & eventYear <= 2017)

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$years)
years_scale <- scale_radius(limits = c(1, 18), range = c(1.5, 8), breaks = c(1, 5, 10, 15))

for (lineColor in lineColors) {
  for (syn in mapSyndromes) {
    df2 <- df %>% filter(syndromeName == syn$name)
    path <- paste0("demo/output/years_", area$name, "_", syn$name, "_2000-2017_", lineColor, ".png")
    message(path)
    makeMap(df2, area = area, type = "years", scale = years_scale, lineColor = lineColor, lineWidth = 0.75, color = "#ff704d")
    ggsave(path, height = 8, width = 12, scale = 0.8)
  }
}
