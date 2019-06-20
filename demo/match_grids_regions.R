library(rhaedat)
library(rgdal)
library(rgeos)
library(dplyr)
library(mapview)
library(geosphere)
library(sf)
library(nngeo)

# fetch grids

gr <- grids() %>%
  mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>%
  filter(!is.na(longitude)) %>%
  filter(longitude >= -180)
  
# fetch regions shapefile

if (!exists("regions")) {
  tf <- tempfile()
  td <- tempdir(check = FALSE)
  download.file("https://github.com/iobis/hab-regions/archive/master.zip", tf)
  unzip(zipfile = tf, exdir = td)
  regions <- readOGR(paste0(td, "/hab-regions-master/hab_regions"), "hab_regions")
}

# match grids

coordinates(gr) <- ~longitude+latitude
proj4string(gr) <- proj4string(regions)
gr_sf <- st_as_sf(gr)
regions_sf <- st_as_sf(regions)
d <- st_nn(gr_sf, regions_sf)
gr$region <- regions$hab_region[unlist(d)]

m <- mapview(gr, zcol = "region", col.regions = c("#900c3f", "#c70039", "#ff5733", "#ff8d1a", "#ffc300", "#eddd53", "#add45c", "#57c785", "#00baad", "#2a7b9b", "#3d3d6b", "#511849"))
mapshot(m, file = paste0(getwd(), "/demo/grids.png"))

statements <- paste("insert into grid_isin_region (gridCode, regionID) values ('", gr$gridCode, "', ", gr$region + 20, ");", sep = "")
write.table(statements, file = "statements.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
