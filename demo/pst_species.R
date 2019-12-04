library(robis)
library(ggplot2)
library(dplyr)
library(dggridR)
library(viridis)

al <- occurrence("Alexandrium") %>% mutate(taxon = "Alexandrium")
gy <- occurrence("Gymnodinium catenatum") %>% mutate(taxon = "Gymnodinium catenatum")
py <- occurrence("Pyrodinium") %>% mutate(taxon = "Pyrodinium")

world <- map_data("world")

colors <- c("#bf3939", "#9aad31", "#e3bc2d")
occ <- bind_rows(al, gy, py)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd", colour = "#aaaaaa", size = 0.2) +
  geom_point(data = occ, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = 2) +
  scale_fill_manual(values = colors) +
  coord_quickmap() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )
ggsave("demo/output/pst_map.png", width = 12, height = 7, scale = 1)

# hex map version

dggs <- dgconstruct(projection = "ISEA", res = 6)
occ$cell <- dgtransform(dggs, occ$decimalLatitude, occ$decimalLongitude)

stats <- occ %>% group_by(taxon, cell) %>% summarise(records = n())
grid <- dgcellstogrid(dggs, stats$cell, frame = TRUE)
grid <- merge(grid, stats, by.x = "cell", by.y = "cell")

ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd", colour = "#aaaaaa", size = 0.2) +
  geom_polygon(data = grid, aes(x = long, y = lat, group = group, fill = taxon), color = "black", size = 0) +
  scale_fill_manual(values = colors) +
  facet_wrap(vars(taxon), ncol = 2) +
  coord_quickmap() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.y = element_blank()
  ) +
  xlim(c(-180, 180)) +
  xlab("longitude") +
  ylab("latitude")
ggsave("demo/output/pst_map_hex.png", width = 12, height = 7, scale = 1)
