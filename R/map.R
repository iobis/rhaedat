#' Make scale
#'
#' @export
make_scale <- function(limits, range = c(2, 10), breaks = NULL) {
  if (is.null(breaks)) {
    if (limits[2] < 5) {
      breaks <- seq(min(limits), max(limits))
    } else if (limits[2] <= 10) {
      breaks <- seq(min(limits), max(limits), by = 2)
    } else {
      breaks <- waiver()
    }
  }
  scale <- scale_radius(limits = limits, range = range, breaks = breaks)
  return(scale)
}

#' Make a map
#'
#' @export
make_map <- function(data, type = "events", area = NULL, color = "red", scale = NULL, line_color = "white", line_width = 0.8, faceted = FALSE) {

  world <- borders("world", colour = "gray80", fill = "gray80", size = 0)

  if (faceted) {
    stats <- data %>% 
      group_by(period, longitude, latitude) %>%
      summarize(events = length(unique(eventName)), years = length(unique(eventYear))) %>%
      filter(!is.na(period))
  } else {
    stats <- data %>% 
      group_by(longitude, latitude) %>%
      summarize(events = length(unique(eventName)), years = length(unique(eventYear)))
  }

  if (!is.null(area)) {
    coord <- coord_quickmap(xlim = area$xlim, ylim = area$ylim)
  } else {
    coord <- coord_quickmap()
  }

  if (is.null(scale)) {
    limits <- range(stats[,type])
    scale <- make_scale(limits)
  }
    
  p <- ggplot() +
    world +
    geom_point(
      data = stats %>% arrange_(.dots = paste0("desc(", type, ")")),
      aes_string(x = "longitude", y = "latitude", size = type),
      stroke = line_width,
      alpha = 1,
      shape = 21,
      fill = color,
      colour = line_color
    ) +
    xlab("longitude") +
    ylab("latitude") +
    coord +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white")
    ) +
    scale
  
  if (faceted) {
    p <- p + facet_wrap(period ~ ., ncol = 2, drop = FALSE)
  }
  
  return(p)
}
