#' Fetch syndromes
#'
#' @export
syndromes <- function() {
  res <- GET("http://haedat.iode.org/api/syndromes", user_agent("robis - https://github.com/iobis/rhaedat"))
  stop_for_status(res)
  text <- content(res, "text", encoding = "UTF-8")
  df <- fromJSON(text, simplifyVector = TRUE)
  return(df)
}
