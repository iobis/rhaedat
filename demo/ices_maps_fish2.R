library(rhaedat)
library(ggplot2)
library(dplyr)

merge_no_other <- TRUE

area <- list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75))
area_europe <- list(name = "europe", xlim = c(-20, 42), ylim = c(35, 70))

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

# mass mortalities

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

# aquaculture fish affected

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(aquacultureFishAffected) %>%
  mutate(syndromeName = if_else(is.na(syndromeName), replacement, syndromeName))

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))

for (syn in syndromes) {
  df2 <- df %>% filter(syndromeName == syn)
  path <- paste0("demo/output/aquaculturefishaffected_events_", area$name, "_", syn, ".png")
  message(path)
  make_map(df2, area = area, type = "events", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
    labs(title = "Events with aquaculture fish affected", subtitle = syn)
  ggsave(path, height = 8, width = 12, scale = 0.8)
}

# mass mortalities, aquaculture fish affected

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

# aqualculture and natural fish (! not looking at massmortal)
# TODO: FIX

df <- events_ices() %>%
  filter(naturalFishAffected | aquacultureFishAffected)

stats <- df %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$years)

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$events), by = 5))
)

df2 <- df %>% filter(eventYear <= 2017)
make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#cccccc") +
  labs(title = "Events with natural or aquaculture fish affected (up to 2017)")
path <- paste0("demo/output/aquaculturenaturalfishaffected_years_atlantic_-2017.png")
ggsave(path, height = 8, width = 12, scale = 0.8)

df2 <- df %>% filter(eventYear >= 1998 & eventYear <= 2017)
make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#cccccc") +
  labs(title = "Events with natural or aquaculture fish affected (1998 - 2017)")
path <- paste0("demo/output/aquaculturenaturalfishaffected_years_atlantic_1998-2017.png")
ggsave(path, height = 8, width = 12, scale = 0.8)

df2 <- df %>% filter(longitude >= area_europe$xlim[1] & longitude <= area_europe$xlim[2] & latitude >= area_europe$ylim[1] & latitude <= area_europe$ylim[2])
stats <- df2 %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$years)
years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(2, max(stats$events), by = 2))
)
make_map(df2, area = area_europe, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#cccccc") +
  labs(title = "Events with natural or aquaculture fish affected")
path <- paste0("demo/output/aquaculturenaturalfishaffected_years_europe.png")
ggsave(path, height = 8, width = 12, scale = 0.8)


# split map effects (todo)
### (i) when mass mortalities has been ticked
### (ii) when other has been ticked
### (iii) when natural fish has been ticked
### (iv) when aquaculture fish has been ticked.

max((df %>% filter(massMortal) %>% group_by(gridCode) %>% summarize(years = length(unique(eventYear))))$years)
max((df %>% filter(otherEffect) %>% group_by(gridCode) %>% summarize(years = length(unique(eventYear))))$years)
max((df %>% filter(naturalFishAffected) %>% group_by(gridCode) %>% summarize(years = length(unique(eventYear))))$years)
max((df %>% filter(aquacultureFishAffected) %>% group_by(gridCode) %>% summarize(years = length(unique(eventYear))))$years)
events_scale <- scale_radius(limits = c(1, 25), range = c(1.5, 8), breaks = c(1, 5, 10, 20, 25))

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2])

make_map(df %>% filter(massMortal), area = area, type = "years", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Events with mass mortalities")
ggsave("demo/output/massmortal.png", height = 8, width = 12, scale = 0.8)

make_map(df %>% filter(otherEffect), area = area, type = "years", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Events with other effects")
ggsave("demo/output/othereffect.png", height = 8, width = 12, scale = 0.8)

make_map(df %>% filter(naturalFishAffected), area = area, type = "years", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Events with natural fish affected")
ggsave("demo/output/naturalfish.png", height = 8, width = 12, scale = 0.8)

make_map(df %>% filter(aquacultureFishAffected), area = area, type = "years", scale = events_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Events with aquaculture fish affected")
ggsave("demo/output/aquaculturefish.png", height = 8, width = 12, scale = 0.8)





