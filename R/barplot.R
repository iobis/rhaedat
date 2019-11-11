library(ggplot2)

#' Events barplot
#'
#' @export
barplot <- function(data, type = "events", min_year = NULL, max_year = NULL, na_omit = TRUE, split = FALSE) {
  
  # prepare data and calculate stats
  
  if (!is.null(max_year)) {
    data <- data %>% filter(eventYear <= max_year)
  }
  if (!is.null(min_year)) {
    data <- data %>% filter(eventYear >= min_year)
  } else {
    data <- data %>% filter(eventYear >= 1900)
  }
  if (na_omit) {
    data <- data %>% filter(!is.na(data$syndromeName))
  }
  stats <- data %>%
    filter(!is.na(eventYear)) %>%
    group_by(syndromeName, eventYear) %>%
    summarize(events = length(unique(eventName)), grids = length(unique(gridCode)))
  
  stats$syndromeName[is.na(stats$syndromeName)] <- "NA"
  
  # fill table
  
  syndromes <- sapply(list_syndromes, function(x) { return(x$name) })
  
  # todo: speed up
  if (nrow(stats) > 0) {
    for (s in unique(c(stats$syndromeName, syndromes))) {
      for (y in seq(min(stats$eventYear), max(stats$eventYear))) {
        if (nrow(stats %>% filter(syndromeName == s & eventYear == y)) == 0) {
          stats <- bind_rows(stats, data.frame(syndromeName = s, eventYear = y, events = 0, grids = 0))
        }
      }
    }
  }

  # trim trailing empty years  

  stats <- stats %>% filter(eventYear <= max(stats$eventYear[which(stats$events > 0)]))

  # set colors  
  
  if (na_omit) {
    cols <- list_cols
    stats <- stats %>% filter(syndromeName != "NA")
    stats$syndromeName <- factor(stats$syndromeName, levels = syndromes)
  } else {
    cols <- c("#cccccc", list_cols)
    stats$syndromeName <- factor(stats$syndromeName, levels = c("NA", syndromes))
  }
  
  # plot
  
  p <- ggplot(stats) +
    geom_bar(aes_string(x = "eventYear", y = type , fill = "syndromeName"), stat = "identity", width = 1) +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) + 
    scale_x_continuous(breaks = function(x) unique(floor(pretty(seq((min(x) - 1), (max(x) + 1)))))) +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#eeeeee")
    ) +
    xlab("year") +
    labs(fill = "syndrome")
  if (!is.null(min_year) | !is.null(max_year)) {
    limits <- c(min(stats$eventYear), max(stats$eventYear))
    if (!is.null(min_year)) limits[1] <- min_year
    if (!is.null(max_year)) limits[2] <- max_year
    p <- p + xlim(limits)
  }
  if (split) {
    p <- p + facet_grid(syndromeName ~ .) + theme(strip.text.y = element_blank())
  }
  
  p
}

#' Events barplot by country
#'
#' @export
barplot_country <- function(data, countries, type = "events", min_year = NULL, max_year = NULL, na_omit = TRUE) {
  
  # prepare data and calculate stats
  
  if (!is.null(max_year)) {
    data <- data %>% filter(eventYear <= max_year)
  }
  if (!is.null(min_year)) {
    data <- data %>% filter(eventYear >= min_year)
  } else {
    data <- data %>% filter(eventYear >= 1900)
  }
  if (na_omit) {
    data <- data %>% filter(!is.na(data$syndromeName))
  }
  stats <- data %>%
    filter(!is.na(eventYear)) %>%
    group_by(syndromeName, countryName, eventYear) %>%
    summarize(events = length(unique(eventName)), grids = length(unique(gridCode)))
  
  stats$syndromeName[is.na(stats$syndromeName)] <- "NA"
  
  # fill table
  
  syndromes <- sapply(list_syndromes, function(x) { return(x$name) })
  
  # todo: speed up
  if (nrow(stats) > 0) {
    for (s in unique(c(stats$syndromeName, syndromes))) {
      for (y in seq(min(stats$eventYear), max(stats$eventYear))) {
        for (co in countries) {
          if (nrow(stats %>% filter(syndromeName == s & eventYear == y & countryName == co)) == 0) {
            stats <- bind_rows(stats, data.frame(syndromeName = s, eventYear = y, countryName = co, events = 0, grids = 0))
          }
        }
      }
    }
  }
  
  # trim trailing empty years  
  
  stats <- stats %>% filter(eventYear <= max(stats$eventYear[which(stats$events > 0)]))
  
  # set colors  
  
  if (na_omit) {
    cols <- list_cols
    stats <- stats %>% filter(syndromeName != "NA")
    stats$syndromeName <- factor(stats$syndromeName, levels = syndromes)
  } else {
    cols <- c("#cccccc", list_cols)
    stats$syndromeName <- factor(stats$syndromeName, levels = c("NA", syndromes))
  }
  
  # plot
  
  p <- ggplot(stats) +
    geom_bar(aes_string(x = "eventYear", y = type , fill = "syndromeName"), stat = "identity", width = 1) +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) + 
    scale_x_continuous(breaks = function(x) unique(floor(pretty(seq((min(x) - 1), (max(x) + 1)))))) +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#eeeeee")
    ) +
    xlab("year") +
    labs(fill = "syndrome")
  if (!is.null(min_year) | !is.null(max_year)) {
    limits <- c(min(stats$eventYear), max(stats$eventYear))
    if (!is.null(min_year)) limits[1] <- min_year
    if (!is.null(max_year)) limits[2] <- max_year
    p <- p + xlim(limits)
  }
  p <- p + facet_grid(countryName ~ .) #+ theme(strip.text.y = element_blank())

  p
}