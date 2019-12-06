library(robis)
library(ggplot2)
library(dplyr)
library(dggridR)
library(viridis)

al <- occurrence("Alexandrium") %>% mutate(taxon = "Alexandrium")
gy <- occurrence("Gymnodinium catenatum") %>% mutate(taxon = "Gymnodinium catenatum")
py <- occurrence("Pyrodinium") %>% mutate(taxon = "Pyrodinium")

world <- map_data("world")

colors <- c("#bf3939", "#6bb02a", "#ffa808")
occ <- bind_rows(al, gy, py)

global <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.2) +
  geom_point(data = occ, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = 1.5) +
  scale_fill_manual(values = colors) +
  coord_quickmap() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_rect(fill = "#fafafa"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

europe_xmin <- -20
europe_xmax <- 40
europe_ymin <- 30
europe_ymax <- 70

europe <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.2) +
  geom_point(data = occ, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = 1.5) +
  scale_fill_manual(values = colors) +
  coord_cartesian(xlim = c(europe_xmin, europe_xmax), ylim = c(europe_ymin, europe_ymax)) +
  theme_nothing() +
  theme(
    panel.background = element_rect(fill = "#ffffff")
  )

global_grob <- ggplotGrob(global)
europe_grob <- ggplotGrob(europe)

inset_xmin <- -230
inset_xmax <- -90
inset_ymin <- -100
inset_ymax <- 0

global + 
  annotation_custom(grob = europe_grob, xmin = inset_xmin, xmax = inset_xmax, ymin = inset_ymin, ymax = inset_ymax) +
  geom_rect(aes(xmin = europe_xmin, xmax = europe_xmax, ymin = europe_ymin, ymax = europe_ymax), linetype = "dashed", color = "grey20", fill = NA, size = 0.3) +
  geom_rect(aes(xmin = inset_xmin, xmax = inset_xmax, ymin = inset_ymin, ymax = inset_ymax), linetype = "dashed", color = "grey20", fill = NA, size = 0.3) +
  geom_segment(aes(x = inset_xmin, xend = europe_xmin, y = inset_ymax, yend = europe_ymax), linetype = "dashed", color = "grey20", size = 0.3) +
  geom_segment(aes(x = inset_xmax, xend = europe_xmax, y = inset_ymin, yend = europe_ymin), linetype = "dashed", color = "grey20", size = 0.3) +
  coord_quickmap(xlim = c(-230, 180), ylim = c(-100, 90))
  
ggsave("demo/output/pst_map.png", width = 12, height = 7, scale = 1)


######### deprecated hex map version

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
