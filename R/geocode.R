#' Geocode strings of text via the Google API
#'
#' The function hits the google maps API and tries to geocode strings of text
#'
#' @param uid_location A data.frame with one column named "uid" - a vector unique ids
#'  and another column named "location" - a vector of strings of text to geocode
#' @param service either yahoo or google, you can programmatically alternate to avoid rate limits
#' @param yahoo_appid Your yahoo_appid from https://developer.apps.yahoo.com/dashboard/createKey.html
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
#' #params
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

geocode <- function(uid_location, service="yahoo", yahoo_appid='') {

    if(service=="yahoo") {
        base_url <- paste0("http://where.yahooapis.com/geocode?appid=", yahoo_appid,"&flags=j" ,"&q=")
    }
    if (service=="google") {
        base_url <- "http://maps.googleapis.com/maps/api/geocode/json?sensor=false&address="
    }
    if(service!="yahoo" & service!="google"){
        stop("only google and yahoo geocoding apis are currently supported")
    }

    # build query
    uid <- as.character(uid_location$uid)
    location <- as.character(uid_location$location)
    geo_url <- paste0(base_url, URLencode(location))

    # geocode
    cat(paste("Geocoding:", location))
    cat("\n")
    geo_text <- try(getURL(geo_url))
    if(class(geo_text)=="try-error"){
        geo_text = try(readLines(geo_url))
    }
    if (class(geo_text)=="try-error"){
        cat(paste("having trouble reading this query:", uid))
        cat("\n")
    }
    #
    geo_json <- fromJSON(geo_text)
    if (service=="google") {
        if(geo_json$status == "OK"){
            lat <- geo_json$results[[1]]$geometry$location$lat
            lng <- geo_json$results[[1]]$geometry$location$lng
            quality <- geo_json$results[[1]]$geometry$location_type
            info <- data.frame(uid, location, lat, lng, quality, stringsAsFactors=F)
          }
          else{
            if(geo_json$status == "OVER_QUERY_LIMIT") {
                stop(paste("Hit rate limit at:", uid, location))
            }
            if(geo_json$status == "ZERO_RESULTS") {
                cat("no results for", uid, location, "\n")
                info <- data.frame(uid, location, lat=NA, lng=NA, quality=NA, stringsAsFactors=F)
            }

          }
    }
    if(service=="yahoo") {
        status <- geo_json$ResultSet$ErrorMessage
        if(status=="No error"){
            lat <- as.numeric(geo_json$ResultSet$Results[[1]]$latitude)
            lng <- as.numeric(geo_json$ResultSet$Results[[1]]$longitude)
            quality <- as.numeric(geo_json$ResultSet$Results[[1]]$quality)
            info <- data.frame(uid, location, lat, lng, quality, stringsAsFactors=F)
        } else if(status=="No result") {
            cat("no results for", uid, location, "\n")
            info <- data.frame(uid, location, lat=NA, lng=NA, quality=NA, stringsAsFactors=F)
        }
    }
    Sys.sleep(0.1)
    return(info)
}