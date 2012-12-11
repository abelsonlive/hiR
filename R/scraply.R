#' Scrape urls with llply, handling errors
#'
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

scraply <- function(ids, scrape=NULL) {
    if(!require("plyr")) {
        install.packages("plyr")
        library("plyr")
    }
    if(typeof(scrape)!="closure") {
        stop("scrape must be a function for scraping associated ids/urls")
    }

    print("scraping pages...")
    ids <- as.characater(ids)
    output <- llply(ids, function(id) {
                                    out <- try(scrape(id, TRUE))
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
