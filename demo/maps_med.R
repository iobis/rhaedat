library(robis)
library(ggplot2)
library(dplyr)
library(stringr)
library(mapdata)
library(rnaturalearth)
library(rhaedat)
library(colorspace)

bubble_size <- 3
fig_scale <- 0.6
fig_xlim <- c(-10, 36.5)
fig_ylim <- c(30, 46)
ann_size <- 1.8

# background

world <- ne_countries(type = "countries", scale = "large")
df <- occurrence(datasetid = "9e2005b5-0e20-4701-bbdb-bd441bd65ea3")
coord_med <- coord_quickmap(xlim = fig_xlim, ylim = fig_ylim)
theme_med <- theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#fafafa"),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
    legend.position = "bottom", legend.title = element_blank()
  )
ann <- bind_rows(
  data.frame(x = 12, y = 39.5, label = "Tyrrhenian\nSea", angle = 0, size = ann_size),
  data.frame(x = 16.3, y = 42.3, label = "Adriatic Sea", angle = -40, size = ann_size),
  data.frame(x = 18, y = 36, label = "Ionian Sea", angle = 0, size = ann_size),
  data.frame(x = 3.5, y = 40.6, label = "Balearic Sea", angle = 0, size = ann_size),
  data.frame(x = 9, y = 43.5, label = "Ligurian\nSea", angle = 0, size = ann_size),
  data.frame(x = 25.3, y = 38.5, label = "Aegean\nSea", angle = 0, size = ann_size)
)
ann_med <- annotate("text", x = ann$x, y = ann$y, label = ann$label, angle = ann$angle, size = ann_size)

# map 1: DSP species

df_dsp <- df %>%
  filter(genus %in% c("Dinophysis", "Phalacroma") | species %in% c("Prorocentrum lima", "Prorocentrum mexicanum")) %>%
  mutate(group = ifelse(genus %in% c("Dinophysis", "Phalacroma"), "Dinophysis / Phalacroma", "Prorocentrum"))

df_dsp <- df_dsp %>%
  arrange(runif(nrow(df_dsp)))

table(df_dsp$species)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_dsp, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c(darken("#1995bb", 0.2), lighten("#1995bb", 0.3))) +
  coord_med +
  theme_med +
  ggtitle("DSP") +
  ann_med

ggsave("demo/output/map_med_1_dsp.png", width = 12, height = 7, scale = fig_scale)

# map 2: PSP

df_psp <- df %>%
  filter(genus == "Alexandrium" | species == "Gymnodinium catenatum") %>%
  filter(is.na(species) | (species != "Alexandrium pseudogonyaulax" & species != "Alexandrium balechii")) %>%
  mutate(group = genus)

df_psp <- df_psp %>%
  arrange(genus)

table(df_psp$species)
table(df_psp$genus)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_psp, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c(darken("#b296e4", 0.35), lighten("#b296e4", 0.1))) +
  coord_med +
  theme_med +
  ggtitle("PSP") +
  ann_med

ggsave("demo/output/map_med_2_psp.png", width = 12, height = 7, scale = fig_scale)

# map 3: ASP

df_asp <- df %>%
  filter(
    (genus == "Pseudo-nitzschia" & !is.na(species) & identificationVerificationStatus %in% c("1 - good", "2 - probable")) |
    (species == "Nitzschia bizertensis")
  ) %>%
  mutate(group = genus)

df_asp <- df_asp %>%
  arrange(desc(group))

df_asp %>% select(genus, species, identificationVerificationStatus)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_asp, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c(darken("#f26c21", 0.2), lighten("#f26c21", 0.2))) +
  coord_med +
  theme_med +
  ggtitle("ASP") +
  ann_med

ggsave("demo/output/map_med_3_asp.png", width = 12, height = 7, scale = fig_scale)

# map 4: Ostreopsis and ciguatera species

df_ost <- df %>%
  filter(genus %in% c("Ostreopsis", "Fukuyoa", "Gambierdiscus")) %>%
  mutate(group = genus)

df_ost <- df_ost %>%
  arrange(desc(group))

table(df_ost$species)
table(df_ost$genus)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_ost, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c(darken("#ebc843", 0.2), darken("#ebc843", 0), lighten("#c02d1d", 0))) +
  coord_med +
  theme_med +
  ggtitle("Ostreopsis and ciguatera") +
  ann_med

ggsave("demo/output/map_med_4_ostreopsis.png", width = 12, height = 7, scale = fig_scale)

# map 5: other toxicity

df_other <- df %>%
  filter(dynamicProperties %in% c("syndrome=Other toxicity", "syndrome=Ichthyotoxic")) %>%
  mutate(group = str_replace(dynamicProperties, "syndrome=Other toxicity", "Other toxicity")) %>%
  mutate(group = str_replace(group, "syndrome=Ichthyotoxic", "Ichthyotoxicity"))
  
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_other, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c(darken("#a2b86c", 0.3), lighten("#a2b86c", 0.05))) +
  coord_med +
  theme_med +
  ggtitle("Ichthyotoxicity and other toxic events") +
  ann_med

ggsave("demo/output/map_med_5_other.png", width = 12, height = 7, scale = fig_scale)

# map 6: possible ASP, this uses all OBIS data!

if (!exists("df_asp2_obis")) {
  df_asp2_obis <- occurrence(c("Pseudo-nitzschia", "Halamphora", "Nitzschia"), geom = "POLYGON((-18 53,49 53,49 22,-18 22,-18 53))")
}

df_asp2 <- df_asp2_obis %>%
#  filter(
#    (genus == "Pseudo-nitzschia") |
#    (genus == "Halamphora") |
#    (species == "Nitzschia bizertensis")
#  ) %>%
  mutate(group = ifelse(genus == "Pseudo-nitzschia", "Pseudo-nitzschia", "Nitzschia / Halamphora"))

df_asp2 <- df_asp2 %>%
  arrange(desc(group))

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#eeeeee", colour = "#cccccc", size = 0.3) +
  geom_point(data = df_asp2, aes(decimalLongitude, decimalLatitude, fill = group), shape = 21, colour = "#ffffff", size = bubble_size) +
  scale_fill_manual(values = c(darken("#f26c21", 0.2), lighten("#f26c21", 0.2))) +
  coord_med +
  theme_med +
  ggtitle("Possible ASP") +
  ann_med

ggsave("demo/output/map_med_6_possibleasp.png", width = 12, height = 7, scale = fig_scale)



