library(rhaedat)
library(ggplot2)
library(dplyr)
library(stringr)
library(glue)
library(patchwork)
library(ggpmisc)

fixed_color <- TRUE
fixed_color_code <- "#c02d1d"

area <- list(name = "us", xlim = c(-170, -63), ylim = c(20, 74))

df <- events() %>%
  filter(!is.na(syndromeName)) %>%
  filter(countryName == "UNITED STATES")

years_scale <- scale_radius(limits = c(1, 30), range = c(1.5, 8), breaks = c(1, 10, 20, 30))
years_scale_5years <- scale_radius(limits = c(1, 5), range = c(1.5, 8))
years_scale2 <- scale_radius(limits = c(1, 30), range = c(1.5, 8), breaks = c(1, 10, 20, 30))
years_scale3 <- scale_radius(limits = c(1, 20), range = c(1.5, 8), breaks = c(1, 10, 20))

### panel with full and 5 years maps + histogram

make_panel <- function(df_subset, subject) {
  #AST, NST, DST, PST, AZT
  subject2 <- recode(subject, "Aerosolized toxins effects" = "Aerosolized toxins", "PSP" = "PST", "ASP" = "AST", "NSP" = "NST", "DSP" = "DST", "AZP" = "AZT")
  
  panel_labels <- c("A", "B", "C", "D", "E", "F")
  panels <- list()
  
  for (pi in 1:length(levels(df$period_alt))) {
    p <- levels(df$period_alt)[pi]
    label <- panel_labels[pi]
    df_period <- df_subset %>% filter(period_alt == p)
    m <- make_map(df_period, area = area, type = "years", line_color = "black", line_width = 0.65, color = fixed_color_code, faceted = FALSE, scale = years_scale_5years) +
      annotate("text", x = area$xlim[1] - 5, y = area$ylim[2], label = glue('bold("{subject2} {p}")'), vjust = 0, hjust = 0, parse = TRUE, size = 5) +
      annotate("text", x = area$xlim[2], y = area$ylim[2], label = glue('bold("{label}")'), vjust = 0.2, hjust = 0, parse = TRUE, size = 7)
    if (pi == 1) {
      m <- m + theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.text = element_text(size = 15), legend.title = element_text(size = 15), legend.background = element_blank(), legend.key = element_blank())
    } else {
      m <- m + theme(legend.position = "none")
    }
    panels[[pi]] <- m
  }
  
  label <- panel_labels[5]
  p <- levels(df$period_alt2)[1]
  df_period <- df_subset %>% filter(period_alt2 == levels(df$period_alt2)[1])
  m <- make_map(df_period, area = area, type = "years", line_color = "black", line_width = 0.65, color = fixed_color_code, faceted = FALSE, scale = years_scale) +
    theme(legend.position = "none") +
    annotate("text", x = area$xlim[1] - 5, y = area$ylim[2], label = glue('bold("{subject2} {p}")'), vjust = 0, hjust = 0, parse = TRUE, size = 5) +
    annotate("text", x = area$xlim[2], y = area$ylim[2], label = glue('bold("{label}")'), vjust = 0.2, hjust = 0, parse = TRUE, size = 7) +
    theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.text = element_text(size = 15), legend.title = element_text(size = 15), legend.background = element_blank(), legend.key = element_blank())
  panels[[5]] <- m
  
  stats <- df_period %>%
    group_by(eventYear) %>%
    summarize(events = length(unique(eventName)))
  
  missing <- setdiff(seq(1990, 2019), stats$eventYear)
  if (length(missing) > 0) {
    stats <- bind_rows(stats, data.frame(eventYear = missing, events = 0))
  }
  
  convert_lim <- function(lim) {
    lim[2] <- lim[2] * 1.1
    return(lim)
  }
  
  label <- panel_labels[6]
  m <- ggplot()
  if (sum(stats$events) > 0) {
    m <- m + geom_smooth(data = stats, aes(x = as.numeric(factor(eventYear, levels = as.character(seq(1990, 2019)))), y = events), method = "lm", se = TRUE, size = 1, color = "#000000", linetype = "dashed")
  }
  m <- m + geom_bar(data = stats %>% filter(events > 0), aes(x = factor(eventYear, levels = as.character(seq(1990, 2019))), y = events), width = 0.6, fill = fixed_color_code, color = "black", stat = "identity")
  if (sum(stats$events) > 0) {
    m <- m + stat_poly_eq(data = stats, formula = y ~ x, aes(x = as.numeric(eventYear), y = events, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), parse = TRUE, size = 5)
  }
  m <- m + xlab("Year") +
    ylab("Number of events") +
    scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015"), limits = as.character(seq(1990, 2019))) +
    #scale_y_continuous(limits = convert_lim) +
    annotate("text", x = "2019", y = max(stats$events), label = glue('bold("{label}")'), vjust = -1.4, hjust = 1, parse = TRUE, size = 7) +
    theme(
      axis.title.x = element_text(colour = "black", size = 15),
      axis.title.y = element_text(colour = "black", size = 15),
      axis.text.x = element_text(colour = "black", size = 15),
      axis.text.y = element_text(colour = "black", size = 15)
    ) +
    coord_cartesian(ylim = c(0, max(stats$events) * 1.1))
  panels[[6]] <- m
  
  (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]]) / (panels[[5]] | panels[[6]])
  
  subject3 <- str_replace_all(subject2, "/", "_")
  ggsave(glue("demo/output/panels_{subject3}.png"), width = 7, height = 9, scale = 2)
  
}

for (syn in sapply(list_syndromes, function(x) { return(x$name) })) {
  df_subset <- df %>% filter(syndromeName == syn)
  make_panel(df_subset, syn)
}

### Cochlodinium/Margalefidinium

df_subset <- events() %>%
  filter(
    str_detect(causativeSpecies, "Cochlodinium") |
      str_detect(causativeSpecies, "Margalefidinium")
  ) %>%
  filter(countryName == "UNITED STATES")

make_panel(df_subset, "Cochlodinium/Margalefidinium")

### Aureococcus/Aureoumbra

df_subset <- events() %>%
  filter(
    str_detect(causativeSpecies, "Aureococcus") |
    str_detect(causativeSpecies, "Aureoumbra")
  ) %>%
  filter(countryName == "UNITED STATES")

make_panel(df_subset, "Aureococcus/Aureoumbra")







### fish kills - TO BE FIXED!!!

#df3 <- events() %>%
#  filter(
#    aquacultureFishAffected |
#    naturalFishAffected
#  ) %>%
#  filter(countryName == "UNITED STATES")

#### full maps

# stats <- df %>% 
#   group_by(syndromeName, longitude, latitude) %>%
#   summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
# range(stats$years)
# 
# for (syn in list_syndromes) {
#   if (fixed_color) {
#     color <- fixed_color_code
#   } else {
#     color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn$name))]
#   }
#   df2 <- df %>% filter(syndromeName == syn$name)
#   path <- paste0("demo/output/years_", area$name, "_", syn$name, ".png")
#   message(path)
#   make_map(df2, area = area, type = "years", scale = years_scale, line_color = "black", line_width = 0.75, color = color) +
#     labs(title = "Years with events", subtitle = syn)
#   ggsave(path, height = 8, width = 12, scale = 0.4)
# }

### 5 year period maps

# for (syn in list_syndromes) {
#   if (fixed_color) {
#     color <- fixed_color_code
#   } else {
#     color <- list_cols[which(sapply(list_syndromes, function(x) x$name == syn$name))]
#   }
#   df2 <- df %>% filter(syndromeName == syn$name)
#   path <- paste0("demo/output/years_5years_alt_", area$name, "_", syn$name, ".png")
#   message(path)
#   make_map(df2, area = area, type = "years", line_color = "black", line_width = 0.65, color = color, faceted = "alternative", scale = years_scale_5years) +
#     labs(title = "Years with events", subtitle = syn)
#   ggsave(path, height = 8, width = 12, scale = 0.8)
# }

