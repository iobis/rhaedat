library(rhaedat)

data <- events_ices()

barplot(data, "grids", min_year = 1985, split = FALSE) +
  ylab("grids with events")
ggsave("demo/output/ices_barplot_grids.png", height = 7, width = 12, scale = 1)

barplot(data, "grids", min_year = 1985, split = TRUE) +
  ylab("grids with events")
ggsave("demo/output/ices_barplot_grids_split.png", height = 10, width = 12, scale = 1)

