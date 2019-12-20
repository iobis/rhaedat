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
      aquacultureFishAffected = as.logical(as.numeric(aquacultureFishAffected)),
      humansAffected = as.logical(as.numeric(humansAffected)),
      fishAffected = as.logical(as.numeric(fishAffected)),
      naturalFishAffected = as.logical(as.numeric(naturalFishAffected)),
      planktonicAffected = as.logical(as.numeric(planktonicAffected)),
      benthicAffected = as.logical(as.numeric(benthicAffected)),
      birdsAffected = as.logical(as.numeric(birdsAffected)),
      shellfishAffected = as.logical(as.numeric(shellfishAffected))
    ) %>%
    filter((is.na(longitude) & is.na(latitude)) | longitude >= -180 & longitude <= 180 & latitude >= -90 & latitude <= 90) %>%
    arrange(gridCode, eventName)
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
  df$period <- factor(df$period, levels = list_periods)
  df <- fix_syndromes(df)
  return(df)
}

#' Fetch ICES region events
#'
#' @export
events_ices <- function(atlantic = FALSE) {
  df <- events()
  ices <- df %>% 
    filter((str_detect(regionName, "^ICES") | str_detect(regionName, "NEP")))
  if (atlantic) {
    return(ices %>% filter(!str_detect(regionName, "NEP")))
  } else {
    return(ices)
  }
}

#' Fetch UK events
#'
#' @export
events_uk <- function() {
  df <- events()
  uk <- df %>% 
    filter((str_detect(countryName, "UNITED KINGDOM")))
  return(uk)
}
