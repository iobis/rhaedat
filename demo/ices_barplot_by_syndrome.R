library(rhaedat)
library(dplyr)
library(egg)
library(stringr)

syndromes <- unique(na.omit(events_ices()$syndromeName))
atl <- events_ices(atlantic = TRUE)
countries <- unique(atl$countryName)

data <- atl %>%
  filter(!is.na(syndromeName))

for (syn in syndromes) {
  message(syn)
  df <- data %>% filter(syndromeName == syn)
  barplot_country(df, countries, "grids", min_year = 1985, max_year = 2019) +
    ylab("grids with events") +
    theme(legend.position = "none", strip.text.y = element_text(size = rel(0.8))) +
    labs(title = paste0(syn, " - number of grids with events per year"))
  ggsave(paste0("demo/output/ices_barplot_", syn, ".png"), height = 18, width = 7, scale = 1, dpi = 300)
}

# mass mortalities

for (syn in syndromes) {
  message(syn)
  df <- data %>% filter(syndromeName == syn & massMortal)
  if (nrow(df) > 0) {
    barplot_country(df, countries, "grids", min_year = 1985, max_year = 2019) +
      ylab("grids with events") +
      theme(legend.position = "none", strip.text.y = element_text(size = rel(0.8)), plot.title = element_text(size = rel(1.1))) +
      labs(title = paste0(syn, " - number of grids with events (mass mortalities)"))
    ggsave(paste0("demo/output/ices_barplot_massmortalities_", syn, ".png"), height = 18, width = 7, scale = 1, dpi = 300)
  }
}

# aquaculturefishaffected

for (syn in syndromes) {
  message(syn)
  df <- data %>% filter(syndromeName == syn & aquacultureFishAffected)
  if (nrow(df) > 0) {
    barplot_country(df, countries, "grids", min_year = 1985, max_year = 2019) +
      ylab("grids with events") +
      theme(legend.position = "none", strip.text.y = element_text(size = rel(0.8)), plot.title = element_text(size = rel(1.1))) +
      labs(title = paste0(syn, " - number of grids with events (aquaculture fish affected)"))
    ggsave(paste0("demo/output/ices_barplot_aquaculturefishaffected_", syn, ".png"), height = 18, width = 7, scale = 1, dpi = 300)
  }
}
