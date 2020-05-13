library(rhaedat)
library(ggplot2)
library(dplyr)

fixed_color <- TRUE

# override areas

list_areas <- list(
  list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75))
)

df <- events_ices() %>%
  filter(!is.na(syndromeName))
  
# full maps

stats <- df %>% 
  group_by(syndromeName, longitude, latitude) %>%
  summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
range(stats$years)
years_scale <- scale_radius(limits = c(1, 35), range = c(1.5, 8), breaks = c(1, 10, 20, 30))

for (syn in list_syndromes) {
  for (area in list_areas) {
    
    if (fixed_color) {
      color <- "#ff704d"
    } else {
      color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn$name))]
    }
    
    df2 <- df %>% filter(syndromeName == syn$name)
    path <- paste0("demo/output/years_", area$name, "_", syn$name, ".png")
    message(path)
    make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = color) +
      labs(title = "Years with events", subtitle = syn)
    ggsave(path, height = 8, width = 12, scale = 0.8)
  }    
}

### 5 year period maps

years_scale2 <- scale_radius(limits = c(1, 5), range = c(1.5, 4))

for (syn in list_syndromes) {
  for (area in list_areas) {
    
    if (fixed_color) {
      color <- "#ff704d"
    } else {
      color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn$name))]
    }

    df2 <- df %>% filter(syndromeName == syn$name)
    path <- paste0("demo/output/years_5years_", area$name, "_", syn$name, ".png")
    message(path)
    make_map(df2, area = area, type = "years", line_color = "black", line_width = 0.65, color = color, faceted = TRUE, scale = years_scale2) +
      labs(title = "Years with events", subtitle = syn)
    ggsave(path, height = 8, width = 12, scale = 0.8)
  }    
}

### 2011- maps

years_scale3 <- scale_radius(limits = c(1, 10), range = c(1.5, 8), breaks = c(1, 2, 5, 10))

for (syn in list_syndromes) {
  for (area in list_areas) {
    
    if (fixed_color) {
      color <- "#ff704d"
    } else {
      color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn$name))]
    }
    
    df2 <- df %>%
      filter(syndromeName == syn$name & eventYear >= 2011)
    path <- paste0("demo/output/years_from2011_", area$name, "_", syn$name, ".png")
    message(path)
    make_map(df2, area = area, type = "years", line_color = "black", line_width = 0.65, color = color, faceted = FALSE, scale = years_scale3) +
      labs(title = "Years with events", subtitle = syn)
    ggsave(path, height = 8, width = 12, scale = 0.8)
  }    
}
