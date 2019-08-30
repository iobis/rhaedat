library(dggridR)
library(robis)
library(tidyverse)
library(viridis)
library(mapproj)
library(scales)

countries <- map_data("world")
data <- occurrence(hab = TRUE, exclude = "bath_issue")

dggs <- dgconstruct(projection = "ISEA", area = 25000, resround = "down")
data$cell <- dgtransform(dggs, data$decimalLatitude, data$decimalLongitude)

stats <- data %>% group_by(cell) %>% summarise(records = n(), species = length(unique(speciesid)))
grid <- dgcellstogrid(dggs, stats$cell, frame = TRUE)
grid <- merge(grid, stats, by.x = "cell", by.y = "cell")

ggplot() + 
  geom_polygon(data = grid, aes(x = long, y = lat, group = group, fill = records), color = "black", size = 0) +
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), fill = "#dddddd", color = NA) +
  scale_fill_viridis(trans = "log10") +
  coord_quickmap() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  xlim(c(-180, 180)) +
  xlab("longitude") +
  ylab("latitude")

ggsave("demo/output/obis.png", height = 8, width = 12, scale = 1)
