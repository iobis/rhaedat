library(rhaedat)
library(dplyr)
library(stringr)
library(ggplot2)

df <- events() %>%
  filter(countryName == "UNITED STATES") %>%
  group_by(longitude, latitude, regionName) %>%
  summarize(n = n())

ices <- df %>% 
  filter((str_detect(regionName, "^ICES") | str_detect(regionName, "NEP"))) %>%
  filter(countryName == "UNITED STATES") %>%
  group_by(longitude, latitude, regionName) %>%
  summarize(n = n())

points <- events() %>%
  filter(countryName == "UNITED STATES") %>%
  group_by(longitude, latitude) %>%
  summarize(n = n())

ggplot() +
  geom_point(data = df, aes(longitude, latitude, color = regionName), size = 5) +
  facet_wrap(~regionName) +
  geom_point(data = points, aes(longitude, latitude), size = 2, color = "black")
  

