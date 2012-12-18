#' Partition a numeric vector into a set of breaks and assign colors
#'
#' This function takes an input numeric vector and partitions
#' it into a set number of breaks.
#' It then assigns a color to each break via RColorBrewer
#'
#' @param var Numeric vector to partition
#' @param n Number of colors / breaks
#' @param style Breaks algorithm from "classIntervals" in the "classInt" package. These include: "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
#' @param palette Palette from RColorBrewer
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
                         pal = "Blues", # Palettes from RColorBrewer
                         na_color ='#787878', # Color to give NA's
                         na_omit = FALSE, # Logical, argument above will be irrelevant if TRUE
                         alph=1 # Opacity (0-1)
                         ) {
    # na_omit?
    if (na_omit) {
      var <- var[!is.na(var)]
    }

    # create colors
    c <- data.frame(col = brewer.pal(n, pal), brk = 1:n)
    c_na <- data.frame(col = na_color, brk = NA)
    c <- rbind(c, c_na)

    # create breaks
    cuts <- classIntervals(var, n, style = "jenks")
    b <- data.frame(brk = cut(var, breaks = cuts$brks, labels = FALSE))

    # assign and return
    return(join(c, b, by="brk", type="right"))
}