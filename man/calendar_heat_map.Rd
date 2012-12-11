\name{calendar_heat_map}
\alias{calendar_heat_map}
\title{Create a calendar heat map with a set number of breaks}
\usage{
  calendar_heat_map(dates, values, breaks, ncolors = 9,
    pal = "Spectral", varname = "Values",
    date_form = "\%Y-\%m-\%d")
}
\arguments{
  \item{dates}{Vector of dates.}

  \item{values}{Numeric vector of values per day.}

  \item{breaks}{Vector specifying values to breaks colors
  at (optional).}

  \item{ncolors}{Number of colors to use.}

  \item{pal}{Palette from RColorBrewer}

  \item{varname}{Name of variable for plot title.}

  \item{date_form}{Date format. Defaults to "\%Y-\%m-\%d"}
}
\description{
  This function creates a calendar heat map with custom
  break values, allowing for comparisions between multiple
  time series.
}
\examples{
date <- seq(from=as.Date("2010-01-01"), to=as.Date("2012-12-31"), by='day')
value <- rnorm(length(date), mean = 10, sd=1)
library("hiR")
calendar_heat_map(dates=date, values=value)
}

