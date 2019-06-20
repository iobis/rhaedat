#' Fetch grids
#'
#' @export
grids <- function() {
  res <- GET("http://haedat.iode.org/api/grids", user_agent("robis - https://github.com/iobis/rhaedat"))
  stop_for_status(res)
  text <- content(res, "text", encoding = "UTF-8")
  df <- fromJSON(text, simplifyVector = TRUE)
  df <- df %>%
    mutate(
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude)
    )
  return(df)
}
