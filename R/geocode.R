#' Geocode strings of text via the Google API
#'
#' The function hits the google maps API and tries to geocode strings of text
#'
#' @param uid_location A data.frame with one column named "uid" - a vector unique ids
#'  and another column named "location" - a vector of strings of text to geocode
#'
#' @return
#' A data.frame with the uid, location, lat, lng, and type indicating the geocoding precision
#'
#' @export
#'
#' @examples
#' # Generate the data
#' uid <- paste0("city", 1:5)
#' location <- c("Boston, MA",
#'               "New York, NY",
#'               "Washington D.C.",
#'               "Philadelphia, PA",
#'               "Baltimore, MD"
#'               )
#' uid_location <- data.frame(uid, location)
#'
#' # Run geocoding funciton
#' library("plyr")
#' library("hiR")
#' geocoded_data <- ddply(uid_location, .(uid), geocode)
#' summary(geocoded_data)
#'
#' # Plot results
#' #param
#' par(family="HersheySans")
#'
#' #map
#' library("maps")
#' regions <- c("new hampshire",
#'                "massachusetts",
#'                "rhode island",
#'                "penn",
#'                "connecticut",
#'                "washington d.c",
#'                "new york",
#'                "new jersey",
#'                "delaware",
#'                "maryland"
#'                )
#' map("state", region=regions, col="grey80")
#'
#' #points + labels
#' points(geocoded_data$lng,
#'        geocoded_data$lat,
#'        pch=20,
#'        cex=2,
#'        col="steelblue")
#' text(geocoded_data$lng-0.5,
#'      geocoded_data$lat+0.3,
#'      labels=geocoded_data$location,
#'      cex=1,
#'      col="darkred")
#' title("Major Cities on the Eastern Seaboard")

geocode <- function(uid_location) {

    # libraries
    if(!require("rjson")){
        install.packages("rjson")
        library("rjson")
    }
    if(!require("RCurl")){
        install.packages("RCurl")
        library("RCurl")
    }
    if(!require("plyr")){
        install.packages("plyr")
        library("plyr")
    }

    # build query
    uid <- as.character(uid_location$uid)
    location <- as.character(uid_location$location)
    base_url <- "http://maps.googleapis.com/maps/api/geocode/json?address="
    geo_url <- paste0(base_url, URLencode(location), "&sensor=false")

    # geocode
    print(paste("Geocoding:", location))
    geo_text <- try(getURL(geo_url))
    if(class(geo_text)=="try-error"){
        geo_text = try(readLines(geo_url))
    }

    if (class(geo_text)=="try-error"){
        print(paste("having trouble reading this query:", uid))
    }
    #
    geo_json <- fromJSON(geo_text)

    if(geo_json$status == "OK"){
        lat = geo_json$results[[1]]$geometry$location$lat
        lng = geo_json$results[[1]]$geometry$location$lng
        type = geo_json$results[[1]]$geometry$location_type
        info <- data.frame(uid, location, lat, lng, type, stringsAsFactors=F)
        return(info)
      }
      else{
        if(geo_json$status == "OVER_QUERY_LIMIT") {
            stop(paste("Hit rate limit at:", uid, location))
        }
    }
    Sys.sleep(0.1)
}