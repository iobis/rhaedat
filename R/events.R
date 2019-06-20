fix_syndromes <- function(data) {
  data$syndromeName <- replace(data$syndromeName, data$syndromeName %in% c("CFP (Ciguatera Fish Poisoning)", "CSP (Ciguatera Shellfish Poisoning)"), "Ciguatera")
  return(data)
}

#' Fetch events
#'
#' @export
events <- function() {
  res <- GET("http://haedat.iode.org/api/events", user_agent("robis - https://github.com/iobis/rhaedat"))
  stop_for_status(res)
  text <- content(res, "text", encoding = "UTF-8")
  df <- fromJSON(text, simplifyVector = TRUE)
  df <- df %>%
    mutate(
      eventDate = as.Date(eventDate),
      initialDate = as.Date(initialDate),
      finalDate = as.Date(finalDate),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      eventYear = as.numeric(eventYear),
      massMortal = as.logical(as.numeric(massMortal)),
      foamMucil = as.logical(as.numeric(foamMucil)),
      aquacultureFishAffected = as.logical(as.numeric(aquacultureFishAffected))
    ) %>%
    filter(longitude >= -180 & longitude <= 180 & latitude >= -90 & latitude <= 90)
  df$period <- sapply(df$eventYear, function(x) {
    if (is.na(x)) {
      return(NA)
    } else if (x >= 1998 & x <= 2002) {
      return("1998 - 2002")
    } else if (x >= 2003 & x <= 2007) {
      return("2003 - 2007")
    } else if (x >= 2008 & x <= 2012) {
      return("2008 - 2012")
    } else if (x >= 2013 & x <= 2017) {
      return("2013 - 2017")
    } else {
      return(NA)
    }
  })
  df <- fix_syndromes(df)
  return(df)
}

#' Fetch ICES region events
#'
#' @export
events_ices <- function() {
  df <- events()
  ices <- df %>% 
    filter((str_detect(regionName, "^ICES") | str_detect(regionName, "NEP")) & !is.na(syndromeName))
  return(ices)
}

#' Fetch UK events
#'
#' @export
events_uk <- function() {
  df <- events()
  uk <- df %>% 
    filter((str_detect(countryName, "UNITED KINGDOM")) & !is.na(syndromeName))
  return(uk)
}
