library(rhaedat)
library(ggplot2)
library(dplyr)
library(glue)

# selected syndromes and countries

area <- list(name = "europe", xlim = c(-10, 35), ylim = c(50, 70))
syns <- c("PSP", "DSP", "ASP", "AZP", "Cyanobacterial toxins effects")
countries <- c("Netherlands", "Germany", "Norway", "Sweden", "Denmark")
excluded <- c("SE-01", "SE-02", "SE-03", "SE-04", "SE-05", "SE-06", "DK-01", "DK-03")

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

for (i in 1:length(list_syndromes)) {
  if (list_syndromes[[i]]$name %in% syns) {
    syn <- list_syndromes[[i]]$name
    col <- list_syndromes[[i]]$color
    df2 <- df %>% filter(syndromeName == syn)
    make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = col) +
      labs(title = glue("Years with events, {syn}"))
    ggsave(glue("demo/output/ices_europe_years_{syn}.png"), height = 8, width = 12, scale = 0.8)
  }
}

# selected syndromes and countries, since 1998

df <- events_ices() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(!(gridCode %in% excluded)) %>%
  filter(eventYear >= 1998)

for (i in 1:length(list_syndromes)) {
  if (list_syndromes[[i]]$name %in% syns) {
    syn <- list_syndromes[[i]]$name
    col <- list_syndromes[[i]]$color
    df2 <- df %>% filter(syndromeName == syn)
    make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = col) +
      labs(title = glue("Years with events, {syn}, 1998 - "))
    ggsave(glue("demo/output/ices_europe_years_{syn}_1998-.png"), height = 8, width = 12, scale = 0.8)
  }
}

# all of haedat (up to 2017)

area <- list(name = "world", xlim = c(-180, 180), ylim = c(-80, 85))

df <- events() %>%
  filter(eventYear <= 2017)

stats <- df %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

years_scale <- scale_radius(
  limits = c(1, max(stats$years)),
  range = c(1.5, 8),
  breaks = c(1, seq(5, max(stats$years), by = 10))
)

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events (up to 2017)")

ggsave("demo/output/haedat_years_-2017.png", height = 8, width = 12, scale = 0.8)

# all of haedat (1998 - 2017)

df <- events() %>%
  filter(eventYear >= 1998 & eventYear <= 2017)

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#ff704d") +
  labs(title = "Years with events (1998 - 2017)")

ggsave("demo/output/haedat_years_1998-2017.png", height = 8, width = 12, scale = 0.8)

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

# selected syndromes and countries (c)
# updated September 2020, up to 2017, removed "FR-12", "FR-13", "FR-14", "FR-15", "FR-16", "FR-17", 
area <- list(name = "europe", xlim = c(-30, 5), ylim = c(28, 68))
countries <- c("Iceland", "Faroe Islands", "United Kingdom", "Ireland", "France", "Portugal", "Spain")
excluded <- c("ES-12", "ES-13", "ES-14", "ES-15", "ES-16", "ES-17", "ES-18", "ES-19", "ES-20", "ES-21", "ES-22", "ES-23", "ES-24", "ES-25", "ES-26", "ES-27")
syns <- c("PSP", "DSP", "ASP", "AZP", "Ciguatera")

df <- events() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(!(gridCode %in% excluded)) %>%
  filter(eventYear <= 2017)

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
  color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn))]
  make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = color) +
    labs(title = glue("Years with events, {syn}"))
  ggsave(glue("demo/output/ices_europe_years_selectedcountries_{syn}.png"), height = 8, width = 12, scale = 0.8)
}

# selected syndromes and countries (d)

area <- list(name = "baltic", xlim = c(9, 31), ylim = c(53, 66))
countries <- c("Denmark", "Germany", "Sweden", "Finland", "Latvia", "Lithuania", "Estonia", "Poland")
excluded <- c("DK-2", "DK-4", "DK-5", "DK-6", "DK-7", "DK-8", "DK-9", "DK-10", "DE-03", "DE-04", "SE-07", "SE-08")
syns <- c("Cyanobacterial toxins effects")

df <- events() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(eventYear <= 2017) %>%
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
  color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn))]
  make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = color) +
    labs(title = glue("Years with events, {syn}, up to 2017"))
  ggsave(glue("demo/output/ices_baltic_years_{syn}_-2017.png"), height = 8, width = 12, scale = 0.8)
}

# selected syndromes and countries (e)

area <- list(name = "europe", xlim = c(2, 33), ylim = c(50, 73))
countries <- c("Belgium", "Netherlands", "Denmark", "Germany", "Norway", "Sweden")
excluded <- c("DE-1", "DE-2", "DK-1", "DK-3", "SE-1", "SE-2", "SE-3", "SE-4", "SE-5", "SE-6")
syns <- c("PSP", "DSP", "ASP", "AZP", "Cyanobacterial toxins effects")

df <- events() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(eventYear <= 2017) %>%
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
  color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn))]
  make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = color) +
    labs(title = glue("Years with events, {syn}"))
  ggsave(glue("demo/output/ices_europe_years_selectedcountries2_{syn}.png"), height = 8, width = 12, scale = 0.8)
}

### same but mass mortalities

df <- events() %>%
  filter(longitude >= area$xlim[1] & longitude <= area$xlim[2] & latitude >= area$ylim[1] & latitude <= area$ylim[2]) %>%
  filter(countryName %in% toupper(countries)) %>%
  filter(eventYear <= 2017) %>%
  filter(!(gridCode %in% excluded)) %>%
  filter(massMortal | aquacultureFishAffected)

stats <- df %>% 
  group_by(longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
  arrange(desc(years), desc(events))
stats

make_map(df, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = "#cccccc") +
  labs(title = glue("Years with events, natural or aquaculture fish affected"))
ggsave(glue("demo/output/ices_europe_years_selectedcountries2_fishaffected.png"), height = 8, width = 12, scale = 0.8)

