#' Partition a numeric vector into a set of breaks and assign colors
#'
#' This function takes an input numeric vector and partitions
#' it into a set number of breaks.
#' It then assigns a color to each break via RColorBrewer
#'
#' @param var Numeric vector to partition
#' @param n Number of colors / breaks
#' @param style Breaks algorithm from "classIntervals" in the "classInt" package. These include: "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
#' @param pal Palette from RColorBrewer
#' @param na_color Hex code to assign NA values
#' @param na_omit Logical; should the function remove NAs. 'na_color' will be irrelevant if this is TRUE.
#' @param alph Opacity level (0=transparent, 1=opaque)
#'
#' @return
#' A data.frame with the variable,
#' break assignments, and color assignments
#'
#' @export
#'
#' @examples
#' var <- rnorm(100)
#' library("hiR")
#' var_cols <- assign_colors(var)
#' par(family="HersheySans")
#' plot(var_cols$var,
#'      pch=20,
#'      col=var_cols$col,
#'      xlab="index",
#'      ylab="value",
#'      main="assign_colors example")

assign_colors <- function(var,
                         n = 9,
                         style = "jenks",
                         pal = "Reds", # Palettes from RColorBrewer
                         na_color ='#787878', # Color to give NA's
                         na_omit = FALSE, # Logical, argument above will be irrelevant if TRUE
                         alph=1 # Opacity (0-1)
                         ) {

    # load required libraries
    if(!require("classInt")){
        install.packages("classInt")
        library("classInt")
    }
    if(!require("scales")){
        install.packages("scales")
        library("scales")
    }
    if(!require("RColorBrewer")){
        install.packages("RColorBrewer")
        library("RColorBrewer")
    }
    if(!require("plyr")){
        install.packages("plyr")
        library("plyr")
    }

    # na_omit?
    if (na_omit) {
      var <- var[!is.na(var)]
    }

    # create colors
    cols <- brewer.pal(n, pal)

    # create breaks
    print("creating breaks...")
    cuts <- classIntervals(var, n, style=style)
    breaks <- cut(var, breaks=cuts$brks, labels=FALSE)

    # create function
    assignColor <- function(x) {
                     if(is.na(x)) {
                        assignment <- alpha(na_color, alph)
                     } else {
                        assignment <- alpha(cols[x], alph)
                     }
                   return(assignment)
                }
    print("assigning colors...")

    # assign colors to breaks
    assignments <- unlist(llply(breaks, assignColor, .progress="text"))
    return(data.frame(var, brk = breaks, col = assignments))
}