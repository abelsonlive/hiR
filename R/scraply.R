#' Scrape urls with llply, handling errors
#'
#' IN DEVELOPMENT
#' This function is a shell for applying scraping
#' functions aross a vector of urls while handling errors
#'
#' @param ids A character vector of ids/urls to feed to a scraping function
#' @param scrape The scraping function to apply across the ids
#'
#' @export
#'
#' @return
#' a data.frame created by scrape

scraply <- function(ids, scrape=scrape) {
    #libraries
    if(!require("plyr")) {
        install.packages("plyr")
        library("plyr")
    }

    # apply function wrapper
    apply_fx <- function(x, fx) {
            FUN <- match.fun(fx)
            FUN(x)
    }
    # scrape urls, handling errors
    print("scraping pages...")
    ids <- as.character(ids)
    output <- llply(ids, function(id) {
                                    out <- try(apply_fx(id, scrape), TRUE)
                                    if (class(out)=='try-error') {
                                        warning(paste("error scraping", id))
                                        out <- NULL
                                    } else {
                                        return(out)
                                    }
                                 }, .progress="text")

    # remove null elements, combine in one data.frame
    output <- output[!sapply(output, is.null)]
    print("reducing output...")
    output <- ldply(output, function(x) {
                                data.frame(x, stringsAsFactors=FALSE)
                                }, .progress="text")
    return(output)
}
