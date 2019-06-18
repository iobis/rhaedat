#' Syndromes list
#'
#' @export
list_syndromes <- list(
  list(name = "ASP"),
  list(name = "AZP"),
  list(name = "Aerosolized toxins effects"),
  list(name = "Cyanobacterial toxins effects"),
  list(name = "DSP"),
  list(name = "NSP"),
  list(name = "PSP"),
  list(name = "CFP (Ciguatera Fish Poisoning)"),
  list(name = "CSP (Ciguatera Shellfish Poisoning)"),
  list(name = "OTHER")
)

#' Color list
#'
#' @export
#list_cols <- c("#c02d1d", "#f26c21", "#edaa38", "#ebc843", "#a2b86c", "#1995bb", "#177799", "#b296e4", "#7648c2", "#aaaaaa")
#list_cols <- c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2", "#aaaaaa")
#list_cols <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
list_cols <- c("#900c3f", "#c70039", "#ff5733", "#ff8d1a", "#ffc300", "#eddd53", "#add45c", "#57c785", "#00baad", "#2a7b9b", "#3d3d6b", "#511849")

#' Area list
#'
#' @export
list_areas <- list(
  list(name = "europe", xlim = c(-25, 30), ylim = c(30, 75)),
  list(name = "atlantic", xlim = c(-110, 40), ylim = c(20, 75)),
  list(name = "pacific", xlim = c(-175, -100), ylim = c(20, 75)),
  list(name = "allareas", xlim = c(-175, 40), ylim = c(10, 90))
)

