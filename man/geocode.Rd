\name{geocode}
\alias{geocode}
\title{Geocode strings of text via the Google API}
\usage{
  geocode(uid_location, service = "google",
    yahoo_appid = "")
}
\arguments{
  \item{uid_location}{A data.frame with one column named
  "uid" - a vector unique ids and another column named
  "location" - a vector of strings of text to geocode}

  \item{service}{either yahoo or google, you can
  programmatically alternate to avoid rate limits}

  \item{yahoo_appid}{Your yahoo_appid from
  https://developer.apps.yahoo.com/dashboard/createKey.html}
}
\value{
  A data.frame with the uid, location, lat, lng, and type
  indicating the geocoding precision
}
\description{
  The function hits the google maps API and tries to
  geocode strings of text
}
\examples{
# Generate the data
uid <- paste0("city", 1:5)
location <- c("Boston, MA",
              "New York, NY",
              "Washington D.C.",
              "Philadelphia, PA",
              "Baltimore, MD"
              )
uid_location <- data.frame(uid, location)

# Run geocoding funciton
library("plyr")
library("hiR")
geocoded_data <- ddply(uid_location, .(uid), geocode)
summary(geocoded_data)

# Plot results
#params
par(family="HersheySans")

#map
library("maps")
regions <- c("new hampshire",
               "massachusetts",
               "rhode island",
               "penn",
               "connecticut",
               "washington d.c",
               "new york",
               "new jersey",
               "delaware",
               "maryland"
               )
map("state", region=regions, col="grey80")

#points + labels
points(geocoded_data$lng,
       geocoded_data$lat,
       pch=20,
       cex=2,
       col="steelblue")
text(geocoded_data$lng-0.5,
     geocoded_data$lat+0.3,
     labels=geocoded_data$location,
     cex=1,
     col="darkred")
title("Major Cities on the Eastern Seaboard")
}

