library(robis)
library(ggplot2)
library(dplyr)
library(stringr)
library(mapdata)

world <- map_data("worldHires", xlim = c(-30, 50), ylim = c(10, 60))
df <- occurrence(datasetid = "9e2005b5-0e20-4701-bbdb-bd441bd65ea3")
coord_med <- coord_quickmap(xlim = c(-10, 36.5), ylim = c(30, 46))
theme_med <- theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", legend.title = element_blank()
  )

# map 1: DSP species

df_dsp <- df %>%
  filter(genus %in% c("Dinophysis", "Phalacroma", "Prorocentrum")) %>%
  filter(is.na(species) | species != "Prorocentrum cordatum") %>%
  mutate(group = ifelse(genus %in% c("Dinophysis", "Phalacroma"), "Dinophysis / Phalacroma", "Prorocentrum"))

df_dsp <- df_dsp %>%
  arrange(runif(nrow(df_dsp)))

table(df_dsp$species)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_dsp, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = 2.5) +
  scale_fill_manual(values = c("#bf3939", "#56b4e9", "#ffa808")) +
  coord_med +
  theme_med +
  ggtitle("DSP")
ggsave("demo/output/map_med_1_dsp.png", width = 12, height = 7, scale = 1)

# map 2: PSP

df_psp <- df %>%
  filter(genus == "Alexandrium" | species == "Gymnodinium catenatum") %>%
  filter(is.na(species) | species != "Alexandrium pseudogonyaulax") %>%
  mutate(group = genus)

df_psp <- df_psp %>%
  arrange(genus)

table(df_psp$species)
table(df_psp$genus)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_psp, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = 2.5) +
  scale_fill_manual(values = c("#bf3939", "#56b4e9", "#ffa808")) +
  coord_med +
  theme_med +
  ggtitle("PSP")
ggsave("demo/output/map_med_2_psp.png", width = 12, height = 7, scale = 1)

# map 3: ASP

df_asp <- df %>%
  filter(
    (genus == "Pseudo-nitzschia" & !is.na(species) & identificationVerificationStatus %in% c("1 - good", "2 - probable")) |
    (genus == "Halamphora") |
    (species == "Nitzschia bizertensis")
  ) %>%
  mutate(group = ifelse(genus == "Pseudo-nitzschia", "Pseudo-nitzschia", "Nitzschia / Halamphora"))

df_asp <- df_asp %>%
  arrange(desc(group))

df_asp %>% select(genus, species, identificationVerificationStatus)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_asp, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = 2.5) +
  scale_fill_manual(values = c("#bf3939", "#56b4e9", "#ffa808")) +
  coord_med +
  theme_med +
  ggtitle("ASP")
ggsave("demo/output/map_med_3_asp.png", width = 12, height = 7, scale = 1)

# map 4: possible ASP

df_asp2 <- df %>%
  filter(
    (genus == "Pseudo-nitzschia") |
    (genus == "Halamphora") |
    (species == "Nitzschia bizertensis")
  ) %>%
  mutate(group = ifelse(genus == "Pseudo-nitzschia", "Pseudo-nitzschia", "Nitzschia / Halamphora"))

df_asp2 <- df_asp2 %>%
  arrange(desc(group))

df_asp2 %>% select(genus, species, identificationVerificationStatus)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_asp2, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = 2.5) +
  scale_fill_manual(values = c("#bf3939", "#56b4e9", "#ffa808")) +
  coord_med +
  theme_med +
  ggtitle("Possible ASP")
ggsave("demo/output/map_med_4_possibleasp.png", width = 12, height = 7, scale = 1)

# map 5: Ostreopsis and ciguatera species

df_ost <- df %>%
  filter(genus %in% c("Ostreopsis", "Fukuyoa", "Gambierdiscus")) %>%
  mutate(group = genus)

df_ost <- df_ost %>%
  arrange(desc(group))

table(df_ost$species)
table(df_ost$genus)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_ost, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = 2.5) +
  scale_fill_manual(values = c("#56b4e9", "#ffa808", "#bf3939")) +
  coord_med +
  theme_med +
  ggtitle("Ostreopsis and ciguatera")
ggsave("demo/output/map_med_5_ostreopsis.png", width = 12, height = 7, scale = 1)

# map 6: other toxicity

taxa <- c("Alexandrium pseudogonyaulax", "Amphidinium", "Amphidinium carterae", "Amphidinium klebsii", "Azadinium", "Azadinium dexteroporum", "Azadinium poporum", "Chattonella", "Chattonella marina", "Chattonella marina var. antiqua", "Chattonella subsalsa", "Chrysochromulina leadbeateri", "Fibrocapsa japonica", "Gonyaulax spinifera", "Heterosigma akashiwo", "Karenia bicuneiformis", "Karenia brevis", "Karenia cristata", "Karenia mikimotoi", "Karenia papilionacea", "Karenia selliformis", "Karenia umbella", "Karlodinium", "Karlodinium armiger", "Karlodinium corsicum", "Karlodinium veneficum", "Lingulodinium polyedra", "Margalefidinium polykrikoides", "Pfiesteria piscicida", "Phaeocystis globosa", "Pheopolykrikos hartmannii", "Prorocentrum cordatum", "Protoceratium reticulatum", "Prymnesium calathiferum", "Prymnesium faveolatum", "Prymnesium parvum", "Prymnesium polylepis", "Pseudochattonella farcimen", "Pseudochattonella verruculosa", "Vicicitus globosus", "Vulcanodinium rugosum")

df_other <- df %>%
  filter(genus %in% taxa | species %in% taxa) %>%
  mutate(group = genus)

paste0(unique(df_other$scientificName)[order(unique(df_other$scientificName))], collapse = ";")

df_other[which(!(df_other$scientificName %in% taxa)),]
taxa[which(!(taxa %in% df_other$scientificName))]

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_other, aes(decimalLongitude, decimalLatitude), fill = "#bf3939", shape = 21, colour = "#ffffff", size = 2.5) +
  coord_med +
  theme_med +
  ggtitle("Other toxicity")
ggsave("demo/output/map_med_6_other.png", width = 12, height = 7, scale = 1)



