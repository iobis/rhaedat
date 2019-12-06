library(robis)
library(ggplot2)
library(dplyr)

occ <- occurrence(c("Ostreopsis", "Gambierdiscus", "Fukuyoa"))
occ_sorted <- occ[rev(order(occ$genus)),]

world <- map_data("world")

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd", colour = "#aaaaaa", size = 0) +
  geom_point(data = occ_sorted, aes(decimalLongitude, decimalLatitude, color = genus, shape = genus), size = 2) +
  scale_color_manual(values = c("#bf3939", "#56B4E9", "#e3bc2d")) +
  scale_shape_manual(values = c(0, 2, 3)) +
  coord_quickmap() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )
ggsave("demo/output/ostreopsis_map_3.png", width = 12, height = 7, scale = 1)
