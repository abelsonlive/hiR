\name{color_assign}
\alias{color_assign}
\title{Partition a numeric vector into a set of breaks and assign colors}
\usage{
  color_assign(var, n = 9, style = "jenks",
    pal = "Spectral", rev = FALSE, na_color = "#787878",
    na_omit = FALSE, alph = 1, include_var = TRUE)
}
\arguments{
  \item{var}{Numeric vector to partition. Alternatively, if
  "style" argument is a clustering algorithm, you can
  supply a matrix.}

  \item{n}{Number of colors / breaks}

  \item{style}{Breaks algorithm from "classIntervals" in
  the "classInt" package. These include: "fixed", "sd",
  "equal", "pretty", "quantile", "kmeans", "hclust",
  "bclust", "fisher", or "jenks"}

  \item{pal}{Palette from RColorBrewer. Alternatively a
  character vector of hexcodes representing your palette.
  If your variable is continuous, these colors should be
  ramped upwards or downwards.}

  \item{rev}{Logical; should the function reverse the
  ordering of the palette}

  \item{na_color}{Hexcode to assign NA values}

  \item{na_omit}{Logical; should the function remove NAs.
  'na_color' will be irrelevant if this is TRUE.}

  \item{alph}{Opacity level (0=transparent, 1=opaque)}

  \item{include_var}{Logical; should the function return
  the input variable(s)}
}
\value{
  A data.frame with the variable, break assignments, and
  color assignments
}
\description{
  This function takes an input numeric vector and
  partitions it into a set number of breaks. It then
  assigns a color to each break via RColorBrewer
}
\examples{
var <- rnorm(1000)
library("hiR")
var_cols <- color_assign(var)
par(family="HersheySans")
plot(var_cols$var,
     pch=20,
     col=var_cols$col,
     xlab="index",
     ylab="value",
     main="assign_colors example")
}

