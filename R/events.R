fix_syndromes <- function(data) {
  data$syndromeName <- replace(data$syndromeName, data$syndromeName %in% c("CFP (Ciguatera Fish Poisoning)", "CSP (Ciguatera Shellfish Poisoning)"), "Ciguatera")
  return(data)
}

#' Fetch events
#'
#' @export
events <- function(latest_data = FALSE) {
  res <- GET("http://haedat.iode.org/api/events.php", user_agent("robis - https://github.com/iobis/rhaedat"))
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
      otherEffect = as.logical(as.numeric(otherEffect)),
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
    } else if (latest_data & x >= 2018) {
      return("2018 -")
    } else {
      return(NA)
    }
  })
  df$period_alt <- sapply(df$eventYear, function(x) {
    if (is.na(x)) {
      return(NA)
    } else if (x >= 2000 & x <= 2004) {
      return("2000 - 2004")
    } else if (x >= 2005 & x <= 2009) {
      return("2005 - 2009")
    } else if (x >= 2010 & x <= 2014) {
      return("2010 - 2014")
    } else if (x >= 2015 & x <= 2019) {
      return("2015 - 2019")
    } else {
      return(NA)
    }
  })
  df$period_alt2 <- sapply(df$eventYear, function(x) {
    if (is.na(x)) {
      return(NA)
    } else if (x >= 1990 & x <= 2019) {
      return("1990 - 2019")
    } else {
      return(NA)
    }
  })
  if (latest_data) {
    period_levels <- list_periods_latest
  } else {
    period_levels <- list_periods
  }
  df$period <- factor(df$period, levels = period_levels)
  df$period_alt <- factor(df$period_alt, levels = list_periods_alt)
  df$period_alt2 <- factor(df$period_alt2, levels = list_periods_alt2)
  df <- fix_syndromes(df) %>%
    filter(!(eventName %in% c("US-17-025", "US-16-031", "US-98-001"))) # Dave Kulis
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
