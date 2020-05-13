#' Syndromes list
#'
#' @export
list_syndromes <- list(
  list(name = "Aerosolized toxins effects", color = "#c02d1d"),
  list(name = "ASP", color = "#f26c21"),
  list(name = "AZP", color = "#edaa38"),
  list(name = "Ciguatera", color = "#ebc843"),
  list(name = "Cyanobacterial toxins effects", color = "#a2b86c"),
  list(name = "DSP", color = "#1995bb"),
  list(name = "NSP", color = "#177799"),
  list(name = "PSP", color = "#b296e4"),
  list(name = "OTHER", color = "#cccccc")
)

#' Color list
#'
#' @export
#list_cols <- c("#c02d1d", "#f26c21", "#edaa38", "#ebc843", "#a2b86c", "#1995bb", "#177799", "#b296e4", "#7648c2", "#cccccc")
list_cols <- c("#c02d1d", "#f26c21", "#edaa38", "#ebc843", "#a2b86c", "#1995bb", "#177799", "#b296e4", "#cccccc")
#list_cols <- c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2", "#aaaaaa")
#list_cols <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
#list_cols <- c("#900c3f", "#c70039", "#ff5733", "#ff8d1a", "#ffc300", "#eddd53", "#add45c", "#57c785", "#00baad", "#2a7b9b", "#3d3d6b", "#511849")

#' Area list
#'
#' @export
list_areas <- list(
  list(name = "europe", xlim = c(-25, 30), ylim = c(30, 75)),
  list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75)),
  list(name = "pacific", xlim = c(-175, -100), ylim = c(20, 75)),
  list(name = "allareas", xlim = c(-175, 40), ylim = c(10, 90))
)

#' Period list
#'
#' @export
list_periods <- c("1998 - 2002", "2003 - 2007", "2008 - 2012", "2013 - 2017")

#' Period list (alternative)
#'
#' @export
list_periods_alt <- c("2000 - 2004", "2005 - 2009", "2010 - 2014", "2015 - 2019")
list_periods_alt2 <- c("1990 - 2019")

#' Period list (including latest data)
#'
#' @export
list_periods_latest <- c("1998 - 2002", "2003 - 2007", "2008 - 2012", "2013 - 2017", "2018 -")
