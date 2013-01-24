#' Partition a numeric vector into a set of breaks and assign colors
#'
#' This function takes an input numeric vector and partitions
#' it into a set number of breaks.
#' It then assigns a color to each break via RColorBrewer
#'
#' @param var Numeric vector to partition. Alternatively, if "style" argument is a clustering algorithm, you can supply a matrix.
#' @param n Number of colors / breaks
#' @param style Breaks algorithm from "classIntervals" in the "classInt" package. These include: "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
#' @param pal Palette from RColorBrewer. Alternatively a character vector of hexcodes representing your palette.  If your variable is continuous, these colors should be ramped upwards or downwards.
#' @param rev Logical; should the function reverse the ordering of the palette
#' @param na_color Hexcode to assign NA values
#' @param na_omit Logical; should the function remove NAs. 'na_color' will be irrelevant if this is TRUE.
#' @param alph Opacity level (0=transparent, 1=opaque)
#' @param include_var Logical; should the function return the input variable(s)
#'
#' @return
#' A data.frame with the variable,
#' break assignments, and color assignments
#'
#' @export
#'
#' @examples
#' var <- rnorm(1000)
#' library("hiR")
#' var_cols <- color_assign(var)
#' par(family="HersheySans")
#' plot(var_cols$var,
#'      pch=20,
#'      col=var_cols$col,
#'      xlab="index",
#'      ylab="value",
#'      main="color_assign example")

color_assign <- function(var,
                         n = 9,
                         style = "jenks",
                         pal = "Spectral",
                         rev = FALSE,
                         na_color = '#787878',
                         na_omit = FALSE,
                         alph = 1,
                         include_var = TRUE
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

    # create order of colors
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
        brks[test] <- brks[test] + max(brks)/1E5
    }

    # assign variables into breaks
    breaks <- cut(var, breaks = brks, labels = FALSE)
    b <- data.frame(var = var, brk = breaks, stringsAsFactors=FALSE)

    # assign colors to breaks
    out <- join(c, b, by="brk", type="right")

    # remove input var if requested
    if (!include_var) {
        out$var <- NULL
    }

    # return
    return(data.frame(out, stringsAsFactors=FALSE))
}