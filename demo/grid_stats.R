library(rhaedat)
library(tidyr)

df <- events()

stats <- df %>%
  filter(!is.na(regionName) & !is.na(gridCode)) %>%
  filter(eventYear >= 1985 & eventYear <= 2018) %>%
  group_by(regionName, eventYear) %>%
  summarize(ngrids = length(unique(gridCode))) %>%
  spread(regionName, ngrids) %>%
  replace(is.na(.), 0)

xlsx::write.xlsx2(stats, "demo/output/stats.xlsx")
