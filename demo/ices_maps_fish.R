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

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$years), by = 5))
)

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events affecting natural or aquaculture fish (1998 - 2017)")

ggsave("demo/output/ices_atlantic_fish_years_1998-2017.png", height = 8, width = 12, scale = 0.8)

# ICES Europe fish affected

area <- list(name = "europe", xlim = c(-25, 40), ylim = c(35, 70))

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
  breaks = c(1, seq(2, max(stats$years), by = 2))
)

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events affecting natural or aquaculture fish")

ggsave("demo/output/ices_europe_fish_years.png", height = 8, width = 12, scale = 0.8)

# ICES Europe fish affected (1998 - 2017)

area <- list(name = "europe", xlim = c(-25, 40), ylim = c(35, 70))

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(naturalFishAffected | aquacultureFishAffected) %>%
  filter(eventYear >= 1998 & eventYear <= 2017)

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
  labs(title = "Years with events affecting natural or aquaculture fish (1998 - 2017)")

ggsave("demo/output/ices_europe_fish_years_1998-2017.png", height = 8, width = 12, scale = 0.8)

# selected syndromes and countries

area <- list(name = "europe", xlim = c(-25, 40), ylim = c(35, 70))
countries <- c("Portugal", "Spain", "France", "Belgium", "Ireland", "United Kingdom", "Iceland", "Faroe Islands", "Sweden", "Denmark", "Poland", "Finland") # excluding Netherlands
excluded <- c("SE-01", "SE-02", "SE-03", "SE-04", "SE-05", "SE-06", "DK-02", "DK-04", "DK-05", "DK-06", "DK-07", "DK-08", "DK-09", "DK-10")
syns <- c("PSP", "DSP", "ASP", "AZP", "Cyanobacterial toxins effects")

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(!(gridCode %in% excluded))

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$years), by = 5))
)

for (syn in syns) {
  df2 <- df %>% filter(syndromeName == syn)
  make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
    labs(title = glue("Years with events, {syn}"))
  ggsave(glue("demo/output/ices_europe_years_selectedcountries_{syn}.png"), height = 8, width = 12, scale = 0.8)
}

# selected syndromes and countries, since 1998

area <- list(name = "europe", xlim = c(-25, 40), ylim = c(35, 70))
countries <- c("Portugal", "Spain", "France", "Belgium", "Ireland", "United Kingdom", "Iceland", "Faroe Islands", "Sweden", "Denmark", "Poland", "Finland") # excluding Netherlands
excluded <- c("SE-07", "SE-08", "DK-02", "DK-04", "DK-05", "DK-06", "DK-07", "DK-08", "DK-09", "DK-10")
syns <- c("PSP", "DSP", "ASP", "AZP", "Cyanobacterial toxins effects")

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(!(gridCode %in% excluded)) %>%
  filter(eventYear >= 1998)

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$years), by = 5))
)

for (syn in syns) {
  df2 <- df %>% filter(syndromeName == syn)
  make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
    labs(title = glue("Years with events, {syn}, 1998 - "))
  ggsave(glue("demo/output/ices_europe_years_selectedcountries_{syn}_1998-.png"), height = 8, width = 12, scale = 0.8)
}

# selected syndromes and countries (b)

area <- list(name = "europe", xlim = c(-25, 40), ylim = c(35, 70))
countries <- c("Germany", "Denmark", "Norway", "Sweden")
excluded <- c("SE-01", "SE-02", "SE-03", "SE-04", "SE-05", "SE-06", "DK-01", "DK-03")
syns <- c("PSP", "DSP", "ASP", "AZP", "Cyanobacterial toxins effects")

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(!(gridCode %in% excluded))

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$years), by = 5))
)

for (syn in syns) {
  df2 <- df %>% filter(syndromeName == syn)
  make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
    labs(title = glue("Years with events, {syn}"))
  ggsave(glue("demo/output/ices_europe_years_selectedcountries2_{syn}.png"), height = 8, width = 12, scale = 0.8)
}

# selected syndromes and countries (b), since 1998

area <- list(name = "europe", xlim = c(-25, 40), ylim = c(35, 70))
countries <- c("Germany", "Denmark", "Norway", "Sweden")
excluded <- c("SE-01", "SE-02", "SE-03", "SE-04", "SE-05", "SE-06", "DK-01", "DK-03")
syns <- c("PSP", "DSP", "ASP", "AZP", "Cyanobacterial toxins effects")

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(!(gridCode %in% excluded)) %>%
  filter(eventYear >= 1998)

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$years), by = 5))
)

for (syn in syns) {
  df2 <- df %>% filter(syndromeName == syn)
  make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
    labs(title = glue("Years with events, {syn}, 1998 -"))
  ggsave(glue("demo/output/ices_europe_years_selectedcountries2_{syn}_1998-.png"), height = 8, width = 12, scale = 0.8)
}



