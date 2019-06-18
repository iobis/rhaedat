library(rhaedat)

data <- events()

for (country in "UNITED KINGDOM") {
  message(country)
  cdata <- data %>% filter(countryName == country)

  barplot(cdata, "grids", min_year = 1985, max_year = 2019, split = FALSE)
  ggsave(paste0("demo/output/barplot_grids_", country, ".png"), height = 10, width = 12, scale = 1)

  barplot(cdata, "grids", min_year = 1985, max_year = 2019, split = TRUE)
  ggsave(paste0("demo/output/barplot_grids_split_", country, ".png"), height = 10, width = 12, scale = 1)
}
