library(rhaedat)
library(ggplot2)
library(dplyr)
library(mapdata)
library(rnaturalearth)
library(tidyr)

area <- list(name = "uk", xlim = c(-13, 3), ylim = c(49, 62))

ev <- events_uk() %>%
  filter(massMortal | aquacultureFishAffected | naturalFishAffected) %>%
  select(eventYear, longitude, latitude, massMortal, aquacultureFishAffected, naturalFishAffected) %>%
  gather(component, affected, massMortal, aquacultureFishAffected, naturalFishAffected) %>%
  filter(affected) %>%
  distinct() %>%
  group_by(longitude, latitude, component) %>%
  summarize(years = length(unique(eventYear)))

world_data <- ne_countries(country = "united kingdom", scale = "large")
world <- geom_polygon(data = world_data, mapping = aes(x = long, y = lat, group = group), fill = "gray80", colour = "gray80", size = 0)

ggplot() +
  world +
  geom_point(data = ev, aes(longitude, latitude, shape = component, color = component, size = years), stroke = 1.5) +
  scale_shape_manual(values = c(2, 3, 4)) +
  scale_color_manual(values = c("#1395BA", "#0E3C55", "#F16C20")) +
  coord_quickmap(xlim = area$xlim, ylim = area$ylim) +
  scale_size(range = c(3, 7), breaks = c(1, max(ev$years))) +
  guides(size = guide_legend(override.aes = list(shape = 4))) +
  theme_void()

ggsave("demo/output/uk_affected.png", width = 7, height = 7)
