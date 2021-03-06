library(rhaedat)
library(stringr)
library(ggplot2)
library(scales)
library(dplyr)

colors <- c(
  "#aecde9",
  "#cdb2ff",
  "#b280b4",
  "#8099e4",
  "#e07f7e",
  "#f4be9a",
  "#fedf81",
  #"#b39980",
  #"#7f7f7f",
  #"#b2b299",
  "#77a877",
  "#b7d5a6"
  #"#f3c3db"
)

regions_all <- c(
  "Region 1 (ECA)",
  "Region 2 (CCA)",
  "Region 3 (SAM)",
  "Region 4 (WCA)",
  "Region 5 (ANZ)",
  "Region 6 (SEA)",
  "Region 7 (NAS)",
  "Region 8 (Indian Ocean)",
  "Region 9 (Benguela)",
  "Region 10 (West Africa)",
  "Region 11 (MED)",
  "Region 12 (EUR)",
  "Region 13 (PAC)"
)

regions <- c(
  "Region 1 (ECA)",
  "Region 2 (CCA)",
  "Region 3 (SAM)",
  "Region 4 (WCA)",
  "Region 5 (ANZ)",
  "Region 6 (SEA)",
  "Region 7 (NAS)",
  #"Region 8 (Indian Ocean)",
  #"Region 9 (Benguela)",
  #"Region 10 (West Africa)",
  "Region 11 (MED)",
  "Region 12 (EUR)"
  #"Region 13 (PAC)"
)

df <- events() %>%
  filter(regionName %in% regions) %>%
  filter(is.na(syndromeName) | !str_detect(syndromeName, "Cyanobacterial")) %>%
  filter(eventYear >= 1985)

stats <- df %>%
  filter(eventYear > 1950) %>%
  mutate(regionName = factor(regionName, levels = regions)) %>%
  group_by(eventYear, regionName) %>%
  summarize(events = length(unique(eventName))) %>%
  select(year = eventYear, region = regionName, events)
  
ggplot() +
  geom_bar(data = stats, aes(x = year, y = events, fill = region), stat = "identity", width = 0.8) +
  scale_fill_manual(values = colors) +
  facet_grid(vars(region), scales = "free") +
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1), n = 3)))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

ggsave("demo/output/region_events_split.png", height = 13, width = 7, scale = 0.8)

# combined with sampling effort (see sampling_effort.R as well)

effort <- read.csv("demo/temp/effort.csv", stringsAsFactors = FALSE) %>%
  mutate(region = regions_all[hab_region]) %>%
  filter(date_year >= 1985)

max_events <- stats %>% group_by(region) %>% summarize(max_events = max(events))
max_effort <- effort %>% group_by(region) %>% summarize(max_effort = max(effort))

effort <- effort %>%
  filter(date_year >= 1985) %>%
  mutate(region = regions_all[hab_region]) %>%
  filter(region %in% regions) %>%
  left_join(max_events, by = c("region")) %>%
  left_join(max_effort, by = c("region")) %>%
  mutate(effort = effort / max_effort * max_events) %>%
  mutate(region = factor(region, levels = regions))
  
ggplot() +
  geom_bar(data = stats, aes(x = year, y = events, fill = region), stat = "identity", width = 0.8) +
  scale_fill_manual(values = colors) +
  geom_line(data = effort, aes(x = date_year, y = effort, color = "effort")) +
  facet_grid(vars(region), scales = "free") +
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1), n = 3)))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_manual(name = "sampling effort", values = c(effort = "#000000"), labels = NULL)

ggsave("demo/output/region_events_effort.png", height = 13, width = 7, scale = 0.8)

# not rescaled

fact <- 10

effort <- read.csv("demo/temp/effort.csv", stringsAsFactors = FALSE) %>%
  mutate(region = regions_all[hab_region]) %>%
  filter(date_year >= 1985) %>%
  mutate(region = regions_all[hab_region]) %>%
  filter(region %in% regions) %>%
  left_join(max_events, by = c("region")) %>%
  left_join(max_effort, by = c("region")) %>%
  #mutate(effort = effort / max_effort * max_events) %>%
  mutate(effort_scaled = effort / fact) %>%
  mutate(region = factor(region, levels = regions))

bf <- function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1), n = 3)))

ggplot() +
  geom_bar(data = stats, aes(x = year, y = events, fill = region), stat = "identity", width = 0.8) +
  scale_fill_manual(values = colors) +
  geom_line(data = effort, aes(x = date_year, y = effort_scaled, color = "effort")) +
  facet_grid(vars(region), scales = "free") +
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_y_continuous(breaks = bf, sec.axis = sec_axis(~.*fact, name = "sampling effort (cell months)", breaks = bf)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_manual(name = "sampling effort", values = c(effort = "#000000"), labels = NULL)

ggsave("demo/output/region_events_effort_yaxes.png", height = 13, width = 9, scale = 0.8)

# data output

openxlsx::write.xlsx(stats, file = "demo/output/events.xlsx")
openxlsx::write.xlsx(effort %>% select(region, date_year, effort), file = "demo/output/effort.xlsx")

