#' Scrape imdb_ids titles, and image urls
#'
#' Scrape all imdb_ids under one or all genres
#' Paging is handled automagically
#' Errors are handled via scraply
#'
#' @param the_genre A genre from genrelist.txt
#'
#' @export
#'
#' @return
#' csv(s) of format <the_genre>.csv in your current directory.
#' These csvs have four attributes:
#' imdb_id, title, alt (alternative title), thumbnail (if applicable)
#'
#' @example
#' # not run:
#' # for use with ldply / genrelist.txt
#' # one genre:
#' # imdb_ids("film_noir")
#' # get all ids from all genres:
#' # xx <- readLines("data/genrelist.txt")
#' # library(plyr)
#' # ldply(xx, imdb_ids)
#'
imdb_ids <- function(the_genre) {
  if(!require('XML')){
    install.packages('XML')
    library('XML')
  }
  if(!require('RCurl')){
    install.packages('RCurl')
    library('RCurl')
  }
  if(!require('stringr')){
    install.packages('stringr')
    library('stringr')
  }
  if(!require('plyr')){
    install.packages('plyr')
    library('plyr')
  }

  # clean up intput variable
  the_genre <- str_trim(as.character(the_genre))
  print(paste("scraping", the_genre))

  # extract page count # generate sub urls
  root <- "http://www.imdb.com/search/title?genres="
  part1 <- "&sort=release_date_us,desc&start="
  url1 <- paste0(root, the_genre , part1, 1)
  page1 <- getURL(url1)
  tree1 <- htmlTreeParse(page1, useInternalNodes=T)
  raw_n_pages <- xmlValue(getNodeSet(tree1, '//*[@id="left"]')[[1]])
  raw_n_pages <- gsub("\n(1-[0-9]{1,2} of )?([0-9,]+)\ntitles\\.\n", "\\2", raw_n_pages)
  n_pages <- as.numeric(gsub(",|\\.", "", raw_n_pages))

  # generate sub urls
  if( n_pages < 50 || any(is.na(n_pages)) ) {
    n_pages <- 1
    sub_urls <- url1
  } else {
    sub_urls <- paste0(root, the_genre, part1, seq(1, n_pages, 50))
  }

  # the scraping function
  scrape_imdb_ids <- function(the_url){
    the_url <- as.character(the_url)
    page2 <- getURL(the_url)
    tree2 <- htmlTreeParse(page2, useInternalNodes=T)

    # get nodes for titles, ids, dates, img_urls
    imageNodes <- getNodeSet(tree2, '//*[@class="image"]/a')
    # extract node-level info
    extractNodeInfo <- function(imageNode) {
      imdb_id <- xmlGetAttr(imageNode, "href")
      child <- xmlChildren(imageNode)
      img <- child$img
      thumbnail <- xmlGetAttr(img, "src")
      title <- xmlGetAttr(img, "title")
      alt <- xmlGetAttr(img, "alt")
      return(data.frame(imdb_id,
                        title,
                        alt,
                        thumbnail,
                        stringsAsFactors=F))
    }
    output <- ldply(imageNodes, extractNodeInfo)
    return(output)
  }

  # scrape the sub urls
  msg <- paste0("scraping ", ceiling(n_pages/50), " sub urls for ", the_genre, "...")
  print(msg)
  # output <- scraply(sub_urls, scrape_imdb_ids)... employ eventually

  # run the function
  output <- llply(sub_urls, function(sub_url) {
                                  out <- try(scrape_imdb_ids(as.character(sub_url)), TRUE)
                                  if (class(out)=='try-error') {
                                      warning(paste("error scraping", sub_url))
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
  fp <- paste0(the_genre, ".csv")
  print(paste("writing", fp, "to file"))
  write.csv(output, fp, row.names=F)
  cat("\n")
}