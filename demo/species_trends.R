library(robis)
library(dplyr)
library(ggplot2)
library(sf)
library(dggridR)
library(viridis)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/6.1.0/share/proj")

taxon <- "Pseudo-nitzschia"
hab <- TRUE

# create grid

dggs <- dgconstruct(projection = "ISEA", area = 70000, resround = "down")
grid_original <- dgearthgrid(dggs, frame = FALSE, wrapcells = FALSE)
grid_sf <- st_as_sf(grid_original)
grid_sf$cell <- as.numeric(names(grid_original))
grid <- st_wrap_dateline(grid_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=230"))
plot(grid)

# fetch obis data

if (hab) {
  occ <- occurrence(taxon, fields = c("decimalLongitude", "decimalLatitude", "scientificName", "date_year"))
} else {
  occ <- occurrence(hab = TRUE, fields = c("decimalLongitude", "decimalLatitude", "scientificName", "date_year"))
}
stations <- occ %>%
  group_by(decimalLongitude, decimalLatitude) %>%
  summarize(records = n())
stations$cell <- dgtransform(dggs, stations$decimalLatitude, stations$decimalLongitude)
stats <- stations %>% group_by(cell) %>% summarize(records = sum(records))

# join data

grid <- grid %>%
  left_join(stats, by = "cell")

# map

world <- ne_countries(scale = "medium", returnclass = "sf")

p1 <- ggplot() + 
  geom_sf(data = grid, aes(fill = records), color = "black", size = 0) +
  geom_sf(data = world, fill = "#dddddd", color = NA) +
  scale_fill_viridis(trans = "log10", na.value = "#fafafa") +
  coord_sf(crs = 54009) + # 54030, 54009
  theme(
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  xlab("") + ylab("")

# time plot

timestats <- occ %>%
  filter(date_year >= 1950) %>%
  group_by(date_year) %>%
  summarize(records = n())

p2 <- ggplot() +
  geom_bar(data = timestats, aes(x = date_year, y = records), stat = "identity", width = 1, fill = "#8EAF9D") + # 19323C
  xlab("year") +
  xlim(c(1950, 2020)) +
  theme(
    panel.background = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

if (hab) {
  title <- paste0("OBIS records of HAB species (n=", format(nrow(occ), big.mark=","), ")")
} else {
  title <- paste0("OBIS records of ", taxon, " (n=", format(nrow(occ), big.mark=","), ")")
}
g <- arrangeGrob(p1, p2, ncol = 1, top = textGrob(title, gp = gpar(fontsize = 12)), heights = c(1.8, 1))

if (hab) {
  ggsave("output/obis_hab.png", g)
} else {
  ggsave(paste0("output/obis_", taxon, ".png"), g)
}
