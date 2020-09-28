library(rhaedat)
library(ggplot2)
library(dplyr)
library(glue)

# ICES Atlantic fish affected

area <- list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75))

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(naturalFishAffected | aquacultureFishAffected)

stats <- df %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$years), by = 5))
)

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events affecting natural or aquaculture fish")

ggsave("demo/output/ices_atlantic_fish_years.png", height = 8, width = 12, scale = 0.8)

# ICES Atlantic fish affected (1998 - 2017)

area <- list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75))

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(naturalFishAffected | aquacultureFishAffected) %>%
  filter(eventYear >= 1998 & eventYear <= 2017)

stats <- df %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events affecting natural or aquaculture fish (1998 - 2017)")

ggsave("demo/output/ices_atlantic_fish_years_1998-2017.png", height = 8, width = 12, scale = 0.8)

# ICES Europe fish affected (selected countries)

#area <- list(name = "europe", xlim = c(-25, 40), ylim = c(35, 70))
area <- list(name = "europe", xlim = c(-20, 35), ylim = c(48, 70))

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(naturalFishAffected | aquacultureFishAffected) %>%
  filter(countryName %in% c("BELGIUM", "DENMARK", "FAROE ISLANDS", "FINLAND", "FRANCE", "GERMANY", "IRELAND", "NETHERLANDS", "NORWAY", "POLAND", "SWEDEN", "UNITED KINGDOM")) %>%
  filter(countryName != "FRANCE" | gridCode %in% c("FR-01", "FR-02", "FR-03", "FR-04", "FR-05"))

stats <- df %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(2, max(stats$years), by = 2))
)

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events affecting natural or aquaculture fish")

ggsave("demo/output/ices_europe_fish_years.png", height = 8, width = 12, scale = 0.8)

# ICES Europe fish affected (1998 - 2017)

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(naturalFishAffected | aquacultureFishAffected) %>%
  filter(eventYear >= 1998 & eventYear <= 2017) %>%
  filter(countryName %in% c("BELGIUM", "DENMARK", "FAROE ISLANDS", "FINLAND", "FRANCE", "GERMANY", "IRELAND", "NETHERLANDS", "NORWAY", "POLAND", "SWEDEN", "UNITED KINGDOM")) %>%
  filter(countryName != "FRANCE" | gridCode %in% c("FR-01", "FR-02", "FR-03", "FR-04", "FR-05"))

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events affecting natural or aquaculture fish (1998 - 2017)")

ggsave("demo/output/ices_europe_fish_years_1998-2017.png", height = 8, width = 12, scale = 0.8)
