\name{assign_colors}
\alias{assign_colors}
\title{Partition a numeric vector into a set of breaks and assign colors}
\usage{
  assign_colors(var, n = 9, style = "jenks", pal = "Blues",
    na_color = "#787878", na_omit = FALSE, alph = 1)
}
\arguments{
  \item{var}{Numeric vector to partition}

  \item{n}{Number of colors / breaks}

  \item{style}{Breaks algorithm from "classIntervals" in
  the "classInt" package. These include: "fixed", "sd",
  "equal", "pretty", "quantile", "kmeans", "hclust",
  "bclust", "fisher", or "jenks"}

  \item{palette}{Palette from RColorBrewer}

  \item{na_color}{Hex code to assign NA values}

  \item{na_omit}{Logical; should the function remove NAs.
  'na_color' will be irrelevant if this is TRUE.}

  \item{alph}{Opacity level (0=transparent, 1=opaque)}
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
var <- rnorm(100)
library("hiR")
var_cols <- assign_colors(var)
par(family="HersheySans")
plot(var_cols$var,
     pch=20,
     col=var_cols$col,
     xlab="index",
     ylab="value",
     main="assign_colors example")
}

