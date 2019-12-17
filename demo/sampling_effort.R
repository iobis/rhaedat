library(robis)
library(sf)
library(ggplot2)
library(dplyr)
library(dggridR)

res <- 8

# download occurrences

occ <- read.csv("demo/temp/occurrence_minimal_phytoplankton_20191205.csv", stringsAsFactors = FALSE)
#occ <- occurrence(c("Prasinophyceae"), "Chromista", "Cyanobacteria"))
region_shapes <- st_read("demo/temp/hab_regions/hab_regions.shp")

# assign regions

occ_sf <- occ %>%
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) %>%
  st_join(region_shapes, join = st_intersects)
occ_sf$decimalLongitude <- st_coordinates(occ_sf)[,1]
occ_sf$decimalLatitude <- st_coordinates(occ_sf)[,2]

# day, month, year

#occ_sf <- occ_sf %>%
#  mutate(date_day = substr(as.POSIXct(date_mid /1000, origin="1970-01-01"), 1, 10))

occ_sf <- occ_sf %>%
  mutate(date_day = substr(parseddate, 1, 10), date_year = as.numeric(substr(parseddate, 1, 4)), date_month = substr(parseddate, 1, 7))
  
# assign cells

dggs <- dgconstruct(projection = "ISEA", res = res)
occ_sf$cell <- dgtransform(dggs, occ_sf$decimalLatitude, occ_sf$decimalLongitude)

# summarize

#stats <- occ_sf %>%
#  group_by(date_year, hab_region, date_month, cell) %>%
#  summarize(records = n())

occ_small <- occ_sf %>%
  as.data.frame() %>%
  select(date_year, date_month, hab_region, cell) %>%
  filter(!is.na(hab_region))

stats <- occ_small %>%
  distinct(date_year, date_month, hab_region, cell) %>%
  group_by(hab_region, date_year) %>%
  summarize(effort = n())

ggplot() +
  geom_line(data = stats, aes(x = date_year, y = effort)) +
  facet_wrap(~hab_region, scales = "free") +
  xlim(c(1985, 2020))
ggsave("demo/output/sampling_effort.png", width = 12, height = 8, scale = 0.9)

write.csv(stats, "demo/temp/effort.csv")

# visualize regions and hex grids

library(rmapshaper)
library(rgeos)
library(dggridR)
library(stringr)

eur <- region_shapes %>% filter(hab_region == 12)
#eur_sp <- as(eur, "Spatial")
eur_simple <- eur %>%
  ms_dissolve() %>%
  ms_simplify()
object.size(eur_simple)
plot(eur_simple)

cells <- occ_small %>%
  filter(hab_region == 12) %>%
  filter(str_detect(date_month, "2010")) %>%
  group_by(date_month, cell) %>%
  summarize()

grid <- dgcellstogrid(dggs, cells$cell) %>% mutate(cell = as.numeric(cell))

cells <- cells %>% left_join(grid, on = "cell")

ggplot() +
  geom_sf(data = eur_simple, color = NA, fill = "#bbbbbb") +
  geom_polygon(data = cells, aes(long, lat, group = group), fill = NA, color = "#cc3300") +
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  facet_wrap(~date_month, nrow = 3)

ggsave("demo/output/sampling_effort_europe.png", width = 12, height = 10, scale = 0.9)

