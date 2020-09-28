library(rhaedat)
library(ggplot2)
library(stringr)
library(glue)
library(patchwork)
library(ggpmisc)
library(dplyr)

fixed_color <- TRUE
fixed_color_code <- "#c02d1d"
area <- list(name = "us", xlim = c(-170, -63), ylim = c(20, 74))
panel_labels <- c("A", "B", "C", "D", "E", "F")
n_grids <- 28
fillmissing <- TRUE
year_start <- 1989
year_end <- 2019
max_years <- year_end - year_start + 1
breaks_start <- 1990
breaks_end <- 2015

# fetch data

df <- events() %>%
  filter(!is.na(syndromeName)) %>%
  filter(countryName == "UNITED STATES")

# scales

years_scale <- scale_radius(limits = c(1, max_years), range = c(1.5, 8), breaks = c(1, 10, 20, 30))
years_scale_5years <- scale_radius(limits = c(1, 5), range = c(1.5, 8))

### panel with full and 5 years maps + histogram

make_map_5years <- function(df_period, area, label, subject2, p, pi) {
  m <- make_map(df_period, area = area, type = "years", line_color = "black", line_width = 0.65, color = fixed_color_code, faceted = FALSE, scale = years_scale_5years) +
    annotate("text", x = area$xlim[1] - 5, y = area$ylim[2], label = glue('bold("{subject2} {p}")'), vjust = 0, hjust = 0, parse = TRUE, size = 5) +
    annotate("text", x = area$xlim[2], y = area$ylim[2], label = glue('bold("{label}")'), vjust = 0.2, hjust = 0, parse = TRUE, size = 7)
  if (pi == 1) {
    m <- m + theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.text = element_text(size = 15), legend.title = element_text(size = 15), legend.background = element_blank(), legend.key = element_blank())
  } else {
    m <- m + theme(legend.position = "none")
  }
  return(m)
}

make_map_30years <- function(df_period, area, label, subject2, p) {
  m <- make_map(df_period, area = area, type = "years", line_color = "black", line_width = 0.65, color = fixed_color_code, faceted = FALSE, scale = years_scale) +
    theme(legend.position = "none") +
    annotate("text", x = area$xlim[1] - 5, y = area$ylim[2], label = glue('bold("{subject2} {p}")'), vjust = 0, hjust = 0, parse = TRUE, size = 5) +
    annotate("text", x = area$xlim[2], y = area$ylim[2], label = glue('bold("{label}")'), vjust = 0.2, hjust = 0, parse = TRUE, size = 7) +
    theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.text = element_text(size = 15), legend.title = element_text(size = 15), legend.background = element_blank(), legend.key = element_blank())
  return(m)
}

make_barchart <- function(df_period) {
  stats <- df_period %>%
    group_by(eventYear) %>%
    summarize(events = length(unique(eventName)))
  missing <- setdiff(seq(year_start, year_end), stats$eventYear)
  if (length(missing) > 0) {
    stats <- bind_rows(stats, data.frame(eventYear = missing, events = 0))
  }
  m <- ggplot()
  if (sum(stats$events) > 0) {
    m <- m + geom_smooth(data = stats, aes(x = as.numeric(factor(eventYear, levels = as.character(seq(year_start, year_end)))), y = events), method = "lm", se = TRUE, size = 1, color = "#000000", linetype = "dashed")
  }
  m <- m + geom_bar(data = stats %>% filter(events > 0), aes(x = factor(eventYear, levels = as.character(seq(year_start, year_end))), y = events), width = 0.6, fill = fixed_color_code, color = "black", stat = "identity")
  if (sum(stats$events) > 0) {
    m <- m + stat_poly_eq(data = stats, formula = y ~ x, aes(x = as.numeric(eventYear), y = events, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), parse = TRUE, size = 5)
  }
  m <- m + xlab("Year") +
    ylab("Number of events") +
    scale_x_discrete(breaks = as.character(seq(breaks_start, breaks_end, by = 5)), limits = as.character(seq(year_start, year_end))) +
    annotate("text", x = as.character(year_end), y = max(stats$events), label = glue('bold("{panel_labels[6]}")'), vjust = -1.4, hjust = 1, parse = TRUE, size = 7) +
    theme(
      axis.title.x = element_text(colour = "black", size = 15),
      axis.title.y = element_text(colour = "black", size = 15),
      axis.text.x = element_text(colour = "black", size = 15),
      axis.text.y = element_text(colour = "black", size = 15)
    ) +
    coord_cartesian(ylim = c(0, max(stats$events) * 1.1))
  return(m)  
}

make_barchart_new <- function(df_period) {
  stats <- df_period %>%
    filter(!is.na(gridCode)) %>%
    group_by(eventYear) %>%
    summarize(grids = length(unique(gridCode)))
  missing <- setdiff(seq(year_start, year_end), stats$eventYear)
  if (length(missing) > 0 & fillmissing) {
    stats <- bind_rows(stats, data.frame(eventYear = missing, grids = 0))
  }
  if (nrow(stats) < 1) next
  stats <- stats %>%
    mutate(noevents = n_grids - grids) %>%
    mutate(p = grids / (grids + noevents))
  model <- glm(cbind(grids, noevents) ~ eventYear, data = stats, family = binomial)
  pred <- predict(model, stats, type = "response", se.fit = TRUE)
  pred_link <- predict(model, stats, type = "link", se.fit = TRUE)
  ilink <- family(model)$linkinv
  stats <- stats %>%
    mutate(fit = pred$fit, se = pred$se.fit, fit_link = pred_link$fit, se_link = pred_link$se.fit) %>%
    mutate(low = fit - 2 * se, high = fit + 2 * se, low_link = ilink(fit_link - 2 * se_link), high_link = ilink(fit_link + 2 * se_link))
  a <- signif(model$coefficients[1], 4)
  b <- signif(model$coefficients[2], 4)
  pr <- signif(coef(summary(model))[2,4], 4)
  s <- ifelse(b < 0, "", "+")
  f <- paste("y == frac(1, 1 + e ^ -(", a, s, b, " * x)) ~~~~ p==", pr)
  m <- ggplot() +
    geom_ribbon(data = stats, aes(x = eventYear, ymin = low_link, ymax = high_link), alpha = 0.2) +
    geom_bar(data = stats, aes(x = eventYear, y = p), width = 0.6, fill = fixed_color_code, color = "black", stat = "identity") +
    geom_line(data = stats, aes(eventYear, fit)) +
    scale_x_continuous(breaks = seq(breaks_start, breaks_end, by = 5), limits = c(year_start - 1, year_end + 1)) +
    annotate("text", x = year_end, y = max(stats$p), label = glue('bold("{panel_labels[6]}")'), vjust = -2.1, hjust = 1, parse = TRUE, size = 7) +
    annotate("text", x = year_start, y = max(stats$p), label = f, vjust = -0.3, hjust = 0, parse = TRUE, size = 5) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.text.x = element_text(colour = "black", size = 15),
      axis.text.y = element_text(colour = "black", size = 15)
    ) +
    coord_cartesian(ylim = c(0, max(stats$p) * 1.1))
  return(m)
}

make_panel <- function(df_subset, subject) {

  subject2 <- recode(subject, "Aerosolized toxins effects" = "Aerosolized toxins", "PSP" = "PST", "ASP" = "AST", "NSP" = "NST", "DSP" = "DST", "AZP" = "AZT")
  panels <- list()
  
  # 5 year maps
  for (pi in 1:length(levels(df$period_alt))) {
    p <- levels(df$period_alt)[pi]
    label <- panel_labels[pi]
    df_period <- df_subset %>% filter(period_alt == p)
    m <- make_map_5years(df_period, area, label, subject2, p, pi)
    panels[[pi]] <- m
  }
  
  # 30 year map
  label <- panel_labels[5]
  p <- levels(df$period_alt2)[1]
  df_period <- df_subset %>% filter(period_alt2 == levels(df$period_alt2)[1])
  m <- make_map_30years(df_period, area, label, subject2, p)
  panels[[5]] <- m
  
  # barchart
  #panels[[6]] <- make_barchart(df_period)
  panels[[6]] <- make_barchart_new(df_period)
  
  (panels[[1]] | panels[[2]]) / (panels[[3]] | panels[[4]]) / (panels[[5]] | panels[[6]])
  subject3 <- str_replace_all(subject2, "/", "_")
  ggsave(glue("demo/output/panels_{subject3}.png"), width = 7, height = 9, scale = 2)
  
}

for (syn in sapply(list_syndromes, function(x) { return(x$name) })) {
  df_subset <- df %>% filter(syndromeName == syn)
  make_panel(df_subset, syn)
}









# 
# ### Cochlodinium/Margalefidinium
# 
# df_subset <- events() %>%
#   filter(
#     str_detect(causativeSpecies, "Cochlodinium") |
#       str_detect(causativeSpecies, "Margalefidinium")
#   ) %>%
#   filter(countryName == "UNITED STATES")
# 
# make_panel(df_subset, "Cochlodinium/Margalefidinium")
# 
# ### Aureococcus/Aureoumbra
# 
# df_subset <- events() %>%
#   filter(
#     str_detect(causativeSpecies, "Aureococcus") |
#     str_detect(causativeSpecies, "Aureoumbra")
#   ) %>%
#   filter(countryName == "UNITED STATES")
# 
# make_panel(df_subset, "Aureococcus/Aureoumbra")










### binomial regression - proof of concept
# check https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/

# n_grids <- 28
# syndromes <- sapply(list_syndromes, function(x) { return(x$name) })
# fillmissing <- FALSE
# 
# for (syn in syndromes) {
# 
#   stats <- df %>%
#     filter(syndromeName == syn & !is.na(gridCode)) %>%
#     group_by(eventYear) %>%
#     summarize(grids = length(unique(gridCode)))
#   missing <- setdiff(seq(1990, 2019), stats$eventYear)
#   if (length(missing) > 0 & fillmissing) {
#     stats <- bind_rows(stats, data.frame(eventYear = missing, grids = 0))
#   }
#   if (nrow(stats) < 1) next
#   stats <- stats %>%
#     mutate(noevents = n_grids - grids) %>%
#     mutate(p = grids / (grids + noevents))
# 
#   model <- glm(cbind(grids, noevents) ~ eventYear, data = stats, family = binomial)
#   #capture.output(summary(model), file = paste0("demo/output/", syn, ifelse(fillmissing, "_fillmissingyears", ""), ".txt"))
#   pred <- predict(model, stats, type = "response", se.fit = TRUE)
#   pred_link <- predict(model, stats, type = "link", se.fit = TRUE)
# 
#   ilink <- family(model)$linkinv
# 
#   stats <- stats %>%
#     mutate(fit = pred$fit, se = pred$se.fit, fit_link = pred_link$fit, se_link = pred_link$se.fit) %>%
#     mutate(low = fit - 2 * se, high = fit + 2 * se, low_link = ilink(fit_link - 2 * se_link), high_link = ilink(fit_link + 2 * se_link))
# 
#   ggplot() +
#     geom_ribbon(data = stats, aes(x = eventYear, ymin = low_link, ymax = high_link), alpha = 0.1) +
#     geom_point(data = stats, aes(eventYear, p)) +
#     geom_line(data = stats, aes(eventYear, fit)) +
#     theme_minimal() +
#     ylab("Proportion grids with events") +
#     xlab("Year") +
#     ggtitle(syn)
# 
#   #ggsave(paste0("demo/output/", syn, ifelse(fillmissing, "_filledmissingyears", ""), ".png"))
# 
# }
# 
