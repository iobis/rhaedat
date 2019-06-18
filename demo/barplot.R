library(rhaedat)

data <- events()

barplot(data, "events", min_year = 1985, split = FALSE)
ggsave("demo/output/barplot_events.png", height = 7, width = 12, scale = 1)

barplot(data, "events", min_year = 1985, split = TRUE)
ggsave("demo/output/barplot_events_split.png", height = 10, width = 12, scale = 1)

barplot(data, "grids", min_year = 1985, split = FALSE)
ggsave("demo/output/barplot_grids.png", height = 7, width = 12, scale = 1)

barplot(data, "grids", min_year = 1985, split = TRUE)
ggsave("demo/output/barplot_grids_split.png", height = 10, width = 12, scale = 1)

data2 <- events_ices()

barplot(data2, "grids", min_year = 1988, max_year = 2017, split = TRUE) +
  ggtitle("HAB events in the ICES countries between 1988 and 2017") +
  xlab("year") +
  ylab("grids with events") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "syndrome")
ggsave("demo/output/barplot_grids_split_ICES.png", height = 10, width = 12, scale = 1)
