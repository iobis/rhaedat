library(rhaedat)
library(ggplot2)
library(dplyr)

merge_no_other <- TRUE

area <- list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75))
if (merge_no_other) {
  syndromes <- unique(na.omit(events_ices()$syndromeName))
} else {
  syndromes <- c(unique(na.omit(events_ices()$syndromeName)), "no syndrome")
}

if (merge_no_other) {
  replacement <- "OTHER"
} else {
  replacement <- "no syndrome"  
}

# massmortalities

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(massMortal) %>%
  mutate(syndromeName = if_else(is.na(syndromeName), replacement, syndromeName))

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$events)
events_scale <- scale_radius(
  limits = c(1, max(stats$events)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$events), by = 5))
)

for (syn in syndromes) {
  df2 <- df %>% filter(syndromeName == syn)
  path <- paste0("demo/output/massmortalities_events_", area$name, "_", syn, ".png")
  message(path)
  make_map(df2, area = area, type = "events", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
    labs(title = "Events with mass mortalities", subtitle = syn)
  ggsave(path, height = 8, width = 12, scale = 0.8)
}

# aquacultureFishAffected

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(aquacultureFishAffected) %>%
  mutate(syndromeName = if_else(is.na(syndromeName), replacement, syndromeName))

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
#range(stats$events)
#events_scale <- scale_radius(
#  limits = c(1, max(stats$events)),
#  range = c(1.5, 8),
#  breaks = seq(1, max(stats$events), by = 2)
#)

for (syn in syndromes) {
  df2 <- df %>% filter(syndromeName == syn)
  path <- paste0("demo/output/aquaculturefishaffected_events_", area$name, "_", syn, ".png")
  message(path)
  make_map(df2, area = area, type = "events", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
    labs(title = "Events with aquaculture fish affected", subtitle = syn)
  ggsave(path, height = 8, width = 12, scale = 0.8)
}

# combined

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(massMortal | aquacultureFishAffected) %>%
  mutate(syndromeName = if_else(is.na(syndromeName), replacement, syndromeName))

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$events)

syn <- "OTHER"
df2 <- df %>% filter(syndromeName == syn)
path <- paste0("demo/output/massmortalities_aquaculturefishaffected_events_", area$name, "_", syn, ".png")
message(path)
make_map(df2, area = area, type = "events", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Events with mass mortalities or aquaculture fish affected", subtitle = syn)
ggsave(path, height = 8, width = 12, scale = 0.8)
