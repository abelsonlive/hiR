#' Partition a numeric vector into a set of breaks and assign colors
#'
#' This function takes an input numeric vector and partitions
#' it into a set number of breaks.
#' It then assigns a color to each break via RColorBrewer
#'
#' @param var Numeric vector to partition
#' @param n Number of colors / breaks
#' @param style Breaks algorithm from "classIntervals" in the "classInt" package. These include: "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
#' @param pal Palette from RColorBrewer. Alternatively a character vector of hexcodes representing your palette.  If your variable is continuous, these colors should be ramped upwards or downwards.
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
#' var_cols <- color_assign(var)
#' par(family="HersheySans")
#' plot(var_cols$var,
#'      pch=20,
#'      col=var_cols$col,
#'      xlab="index",
#'      ylab="value",
#'      main="assign_colors example")

color_assign <- function(var,
                         n = 9,
                         style = "jenks",
                         pal = "Blues", # Palettes from RColorBrewer.
                         rev = FALSE,  # Logical; Should the function reverse ramp order palette
                         na_color = '#787878', # Color to give NA's
                         na_omit = FALSE, # Logical, argument above will be irrelevant if TRUE
                         alph = 0.5 # Opacity (0-1)
                         ) {
    # na_omit?
    if (na_omit) {
      var <- var[!is.na(var)]
    }

    # create colors
    if (length(pal)==1){
       cols <- brewer.pal(n, pal)
    } else if (length(pal) > 1){
        cols <- pal
        n <- length(pal)
    }

    # create order
    if (rev) {
        order <- n:1
    } else {
        order <- 1:n
    }

    # create color data.frame
    c <- data.frame(col = cols, brk = order, stringsAsFactors=FALSE)
    c_na <- data.frame(col = na_color, brk = NA, stringsAsFactors=FALSE)
    c <- rbind(c, c_na)
    c$col <- alpha(c$col, alph) # add alpha levels to color

    # cut variable into breaks
    cuts <- classIntervals(var, n, style = style)

    # check if breaks are unique.
    brks <- cuts$brks
    test <- duplicated(brks)
    if (any(test)) {
        # if they aren't add a small amount to the duplicated version
        brks[test] <- brks[test] + max(brks)/10000000
    }

    # assign variables into breaks
    breaks <- cut(var, breaks = brks, labels = FALSE)
    b <- data.frame(var = var, brk = breaks, stringsAsFactors=FALSE)

    # assign colors to breaks
    out <- join(c, b, by="brk", type="right")

    # return
    return(data.frame(out, stringsAsFactors=FALSE))
}

