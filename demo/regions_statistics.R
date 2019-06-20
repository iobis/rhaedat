library(rhaedat)
library(ggplot2)
library(dplyr)

data <- events()

regionplot(data) +
  ggtitle("HAB events by region") +
  xlab("region") +
  ylab("events composition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "syndrome")

ggsave("demo/output/barplot_regions.png", height = 10, width = 12, scale = 1)

regionplot(data, relative = FALSE) +
  ggtitle("HAB events by region") +
  xlab("region") +
  ylab("events") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "syndrome")

ggsave("demo/output/barplot_regions_absolute.png", height = 10, width = 12, scale = 1)

# events per region

data %>%
  group_by(regionName) %>%
  summarize(events = length(unique(eventName))) %>%
  filter(str_detect(regionName, "Region"))

# total events

length(unique(data$eventName))
