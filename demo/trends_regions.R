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
  scale_x_continuous(breaks = pretty_breaks(n = 10))
#ggsave("demo/output/region_events.png", height = 7, width = 10)

ggplot() +
  geom_bar(data = stats, aes(x = year, y = events, fill = region), stat = "identity", width = 0.8) +
  scale_fill_manual(values = colors) +
  facet_grid(vars(region), scales = "free") +
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1), n = 3)))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))
ggsave("demo/output/region_events_split.png", height = 13, width = 7, scale = 0.8)

