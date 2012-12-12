#' Scrape urls with llply, handling errors and iteratively dumping to csv
#'
#' IN DEVELOPMENT
#' This function is a shell for applying two functions:
#' The first, "gen_urls" creates a list of urls to scrape given a seed, i.e "A" in LETTERS
#' The second, "scrape" scrapes each of thes urls.
#' This function generates the urls, scrapes them, and dumps one csv to file per seed.
#'
#' @param ids A character vector of ids/urls to feed to a scraping function
#' @param scrape The scraping function to apply across the ids
#'
#' @export
#'
#' @return
#' a data.frame created by scrape

dumply <- function(the_seed, gen_urls = gen_urls, scrape = scrape, path_to_dir=NULL) {

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

    # generate urls
    the_urls <- apply_fx(the_seed, gen_urls)
    the_urls <- as.character(the_urls)

    # scrape urls, handling errors
    print(paste("scraping pages under", the_seed))

    # run the function
    output <- llply(the_urls, function(the_url) {
                                    out <- try(apply_fx(the_url, scrape), TRUE)
                                    if (class(out)=='try-error') {
                                        warning(paste("error scraping", the_url))
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
    # output genre chunk
    if(is.null(path_to_dir) {
      path_to_dir = ""
    } else {
      path_to_dir <- paste0(path_to_dir, "/")
    }
    fp <- paste0(path_to_dir, the_seed, ".csv")
    print(paste("writing", fp, "to file"))
    write.csv(output, fp, row.names=F)
}