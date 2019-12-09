library(robis)
library(ggplot2)
library(dplyr)
library(stringr)
library(mapdata)

world <- map_data("worldHires", xlim = c(-30, 50), ylim = c(10, 60))
bubble_size <- 3
fig_scale <- 0.6
fig_xlim <- c(-9, 36)
fig_ylim <- c(30, 46)

# Ostreopsis, Gambierdiscus, Fukuyoa

occ <- occurrence(c("Ostreopsis", "Gambierdiscus", "Fukuyoa"))
occ_sorted <- occ[rev(order(occ$genus)),]

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = occ_sorted, aes(decimalLongitude, decimalLatitude, fill = genus), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c("#bf3939", "#56B4E9", "#ffa808")) +
  coord_quickmap(xlim = fig_xlim, ylim = fig_ylim) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", legend.title = element_blank()
  )
ggsave("demo/output/map_ostreopsis_gambierdiscus_ostreopsis.png", width = 12, height = 7, scale = fig_scale)

# Dinophysis and Prorocentrum, excluding Prorocentrum cordatum

occ <- occurrence(c("Dinophysis", "Prorocentrum"), datasetid = "9e2005b5-0e20-4701-bbdb-bd441bd65ea3")
occ_filtered <- occ %>%
  filter(is.na(species) | species != "Prorocentrum cordatum") %>%
  mutate(taxon = ifelse(genus == "Dinophysis", "Dinophysis", "Prorocentrum, excluding Prorocentrum cordatum"))
occ_sorted <- occ_filtered[order(runif(nrow(occ_filtered))),]

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = occ_sorted, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c("#bf3939", "#56B4E9", "#ffa808")) +
  coord_quickmap(xlim = fig_xlim, ylim = fig_ylim) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", legend.title = element_blank()
  )
ggsave("demo/output/map_dinophysis_prorocentrum.png", width = 12, height = 7, scale = fig_scale)

# Dinophysis and Prorocentrum, excluding Prorocentrum cordatum (all OBIS data)

#occ <- occurrence(c("Dinophysis", "Prorocentrum"))#, datasetid = "9e2005b5-0e20-4701-bbdb-bd441bd65ea3")
#occ_filtered <- occ %>%
#  filter(is.na(species) | species != "Prorocentrum cordatum") %>%
#  mutate(taxon = ifelse(genus == "Dinophysis", "Dinophysis", "Prorocentrum, excluding Prorocentrum cordatum"))
#occ_sorted <- occ_filtered[order(runif(nrow(occ_filtered))),]
#
#ggplot() +
#  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
#  geom_point(data = occ_sorted, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = bubble_size) +
#  scale_fill_manual(values = c("#bf3939", "#56B4E9", "#ffa808")) +
#  coord_quickmap(xlim = fig_xlim, ylim = fig_ylim) +
#  theme(
#    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#    panel.background = element_rect(fill = "#fafafa"),
#    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
#    legend.position = "bottom", legend.title = element_blank()
#  )
#ggsave("demo/output/map_dinophysis_prorocentrum_allobisdata.png", width = 12, height = 7, scale = fig_scale)

# Alexandrium and Gymnodinium catenatum

occ <- occurrence(c("Alexandrium", "Gymnodinium catenatum"), datasetid = "9e2005b5-0e20-4701-bbdb-bd441bd65ea3")
occ_filtered <- occ %>%
  mutate(taxon = ifelse(genus == "Alexandrium", "Alexandrium", "Gymnodinium catenatum"))
occ_sorted <- occ_filtered[order(occ_filtered$genus),]

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = occ_sorted, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c("#bf3939", "#56B4E9", "#ffa808")) +
  coord_quickmap(xlim = fig_xlim, ylim = fig_ylim) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", legend.title = element_blank()
  )
ggsave("demo/output/map_alexandrium_gymnodinium_catenatum.png", width = 12, height = 7, scale = fig_scale)

# Pseudo-nitzschia (good data)

occ <- occurrence(c("Pseudo-nitzschia"), datasetid = "9e2005b5-0e20-4701-bbdb-bd441bd65ea3")
occ_filtered <- occ %>%
  filter(!is.na(species)) %>%
  filter(str_detect(identificationVerificationStatus, "good") | str_detect(identificationVerificationStatus, "probable")) %>%
  mutate(taxon = "Pseudo-nitzschia (species level)")
occ_sorted <- occ_filtered[order(occ_filtered$genus),]

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = occ_sorted, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c("#bf3939", "#56B4E9", "#ffa808")) +
  coord_quickmap(xlim = fig_xlim, ylim = fig_ylim) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", legend.title = element_blank()
  )
ggsave("demo/output/map_pseudonitzschia_good_probable.png", width = 12, height = 7, scale = fig_scale)

# Pseudo-nitzschia (all data)

occ <- occurrence(c("Pseudo-nitzschia"), datasetid = "9e2005b5-0e20-4701-bbdb-bd441bd65ea3")
occ_filtered <- occ %>%
  mutate(taxon = "Pseudo-nitzschia")
occ_sorted <- occ_filtered[order(occ_filtered$genus),]

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = occ_sorted, aes(decimalLongitude, decimalLatitude, fill = taxon), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c("#bf3939", "#56B4E9", "#ffa808")) +
  coord_quickmap(xlim = fig_xlim, ylim = fig_ylim) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", legend.title = element_blank()
  )
ggsave("demo/output/map_pseudonitzschia_all.png", width = 12, height = 7, scale = fig_scale)

