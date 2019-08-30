#' Region barplot
#'
#' @export
regionplot <- function(data, na_omit = TRUE, relative = TRUE) {

  if (relative) {
    position <- "fill"
  } else {
    position <- "stack"
  }
  
  stats <- data %>%
    group_by(syndromeName, regionName) %>%
    summarize(events = length(unique(eventName))) %>%
    filter(str_detect(regionName, "Region"))

  syndromes <- sapply(list_syndromes, function(x) { return(x$name) })
  regions <- unique(stats$regionName)
  stats$regionName <- factor(stats$regionName, levels = regions[order(as.numeric(str_match(regions, "[0-9]+")[,1]))])
  
  stats$syndromeName[is.na(stats$syndromeName)] <- "NA"
  
  if (na_omit) {
    cols <- list_cols
    stats <- stats %>% filter(syndromeName != "NA")
    stats$syndromeName <- factor(stats$syndromeName, levels = syndromes)
  } else {
    cols <- c("#cccccc", list_cols)
    stats$syndromeName <- factor(stats$syndromeName, levels = c("NA", syndromes))
  }
  
  p <- ggplot(stats) +
    geom_bar(aes_string(x = "regionName", y = "events", fill = "syndromeName"), stat = "identity", position = position) +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#eeeeee")
    )

  p
}