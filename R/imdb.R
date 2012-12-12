#' @title Scrape imdb_ids titles, and image urls
#'
#' @description
#' Scrape all imdb_ids under one or all genres
#' Paging is handled automagically.
#' Errors and csv output are handled via dumply
#'
#' @param the_genre A genre from imbd_genres.txt
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
#' # for use with ldply / imbd_genres.txt
#' # one genre:
#' # imdb_ids("film_noir")
#' # get all ids from all genres:
#' # xx <- readLines("data/genrelist.txt")
#' # library(plyr)
#' # ldply(xx, imdb_ids)
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

  # extract page count
  root <- "http://www.imdb.com/search/title?genres="
  part1 <- "&sort=release_date_us,desc&start="
  url1 <- paste0(root, the_genre , part1, 1)
  page1 <- getURL(url1)
  tree1 <- htmlTreeParse(page1, useInternalNodes=T)
  raw_n_pages <- xmlValue(getNodeSet(tree1, '//*[@id="left"]')[[1]])
  raw_n_pages <- gsub("\n(1-[0-9]{1,2} of )?([0-9,]+)\ntitles\\.\n", "\\2", raw_n_pages)
  n_pages <- as.numeric(gsub(",|\\.", "", raw_n_pages))

  # generate sub urls
  if( n_pages < 51 || any(is.na(n_pages)) ) {
    n_pages <- 1
    sub_urls <- url1
  } else {
    sub_urls <- paste0(root, the_genre, part1, seq(1, n_pages, 50))
  }

  # the scraping function
  scrape_imdb_ids <- function(the_url){
    # get page
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
    # extract info from all nodes
    output <- ldply(imageNodes, extractNodeInfo)
    return(output)
  }

  # scrape all the sub urls for the genre
  msg <- paste0("scraping ", ceiling(n_pages/50), " sub urls for ", the_genre, "...")
  print(msg)
  # output <- scraply(sub_urls, scrape_imdb_ids)... implement eventually

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
#' Scrape cast info for a film given its imdb_id
#'
#' @param imdb_id An imdb id of format "tt[0-9]{7}"
#'
#' @example
#' #for use with scraply
#' #not run: scraply(imdb_ids, imdb_cast)
#'
#' @export
#'
#' @return
#' a data.frame created by the function
imdb_cast <- function(imdb_id) {
    if(!require("RCurl")) {
        install.packages('RCurl')
        library("RCurl")
    }
    if(!require("XML")) {
        install.packages("XML")
        library("XML")
    }
    if(!require("stringr")) {
        install.packages("stringr")
        library("stringr")
    }
    if(!require("plyr")) {
        install.packages("plyr")
        library("plyr")
    }

    imdb_id <- as.character(imdb_id)
    url <- paste0('http://www.imdb.com/title/', imdb_id, '/fullcredits#cast')
    page <- getURL(url) # RCurl
    tree <- htmlTreeParse(page, useInternalNodes=TRUE) #XML package

    # get nodes
    nodes <- getNodeSet(tree, '//*[@cellpadding="1"]')

    #extraction function
    extract_cast <- function(node) {
        # get children
        children <- xmlChildren(node)

        # header
        header <- children[1]$tr
        meta_node <- header[1]$td[1]$h5[1]$a
        meta_role <- xmlValue(meta_node)
        meta_role_id <- xmlGetAttr(meta_node, "href")

        # extract the rows
        n <- length(children)
        rows <- children[2:n]

        # parse
        #args
        out_df <- data.frame()
        row_arg <- TRUE
        i <- 2

        while(row_arg) {
            name_node <- children[i]$tr[1]$td[1]$a
            if(is.null(name_node)) {
                row_arg <- FALSE
            }
            else {
                name_id <- xmlGetAttr(name_node, "href")
                name  <- xmlValue(name_node)
                role_node <- children[i]$tr[3]$td[1]$a
                role_id <- xmlGetAttr(role_node, "href")
                role  <- xmlValue(role_node)
                the_df <- data.frame(name_id,
                                     name,
                                     role_id,
                                     role,
                                     meta_role,
                                     meta_role_id,
                                     stringsAsFactors=F)
                out_df <- rbind(out_df, the_df)
                i <- i + 1
            }
        }
        return(out_df)
    }

    # run
    output <- ldply(nodes, extract_cast)
    output$imdb_id <- imdb_id
    return(output)
}
#' @title Scrape links to external reviews about a movie
#'
#' @param imdb_id An imdb id of format "tt[0-9]{7}"
#'
#' @example
#' #for use with scraply
#' #not run: scraply(imdb_ids, imdb_info)
#'
#' @export
#'
#' @return
#' a data.frame created by the function
imdb_external_reviews <- function(imdb_id) {

    # libs
    if(!require("RCurl")) {
        install.packages('RCurl')
        library("RCurl")
    }
    if(!require("XML")) {
        install.packages("XML")
        library("XML")
    }
    if(!require("stringr")) {
        install.packages("stringr")
        library("stringr")
    }
    if(!require("plyr")) {
        install.packages("plyr")
        library("plyr")
    }

    # generate url
    the_url <- paste0("http://www.imdb.com/title/", imdb_id, "/", "externalreviews")

    # download page
    html <- getURL(the_url)
    tree <- htmlTreeParse(html, useInternalNodes=T)
    nodes <- getNodeSet(tree, '//*[@id="tn15content"]/ol/li/a')
    nodes <-unlist(nodes)
    the_articles <- laply(nodes, function(x) xmlGetAttr(x, "href") )
    output <- data.frame(article_link = the_articles, stringsAsFactors=F)
    output$imdb_id <- imdb_id
    return(output)
}

#' @title Scrape imdb keywords for a movie, handling errors
#'
#' @param imdb_id An imdb id of format "tt[0-9]{7}"
#'
#' @example
#' #for use with scraply
#' #not run: scraply(imdb_ids, imdb_keywords)
#'
#' @export
#'
#' @return
#' a data.frame created by the function
imdb_keywords <- function(imdb_id) {

    # libs
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
    # generate url
    imdb_id <- as.character(imbd_id)
    the_url <- paste0("http://www.imdb.com/title/", imdb_id, "/", "keywords")

    # download page
    html <- getURL(the_url)
    tree <- htmlTreeParse(html, useInternalNodes=T)
    # extract article links
    nodes <- unlist(getNodeSet(tree, '//*[@class="keyword"]/a'))
    the_keyword_ids <- laply(nodes, function(x) xmlGetAttr(x, "href") )
    the_keyword_text <- laply(nodes, xmlValue )

    # output
    output <- data.frame(key`_id = the_keyword_ids,
                         keyword_text = the_keyword_text,
                        stringsAsFactors=F)
    output$imdb_id <- imdb_id
    return(output)
}

#' @title get metacritic links for an imdb_id
#'
#' @param imdb_id An imdb id of format "tt[0-9]{7}"
#'
#' @example
#' #for use with scraply
#' #not run: scraply(imdb_ids, imdb_metacritic)
#'
#' @export
#'
#' @return
#' a data.frame created by the function
imdb_metacritic <- function(imdb_id) {

  # libraries
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
    # generate url
    imdb_id <- as.character(imdb_id)
    the_url <- paste0("http://www.imdb.com/title/", imdb_id, "/", "criticreviews")

    # download page
    html <- getURL(the_url)
    tree <- htmlTreeParse(html, useInternalNodes=T)
    meta_critic_url <- NA
    meta_critic_link_nodes <- getNodeSet(tree, '//*[@class="see-more"]/a')[[1]]
    if (length(meta_critic_link_node) > 1) {
        meta_critic_url <- xmlGetAttr(meta_critic_link_node, "href")
    }
    return(data.frame(imdb_id, meta_critic_url, stringsAsFactors=F))
}
#' @title Scrape the plot summary for an imdb_id, handling errors
#'
#' @param imdb_id An imdb id of format "tt[0-9]{7}"
#'
#' @example
#' #for use with scraply
#' #not run: scraply(imdb_ids, imdb_plot_summaryo)
#'
#' @export
#'
#' @return
#' a data.frame created by the function
imdb_plot_summary <- function(imdb_id) {
    # download page
    imdb_id <- as.character(imdb_id)
    the_url <- paste0("http://www.imdb.com/title/", imdb_id, "/plotsummary")
    html <- getURL(the_url)
    tree <- htmlTreeParse(html, useInternalNodes=T)

    # GET PLOT SUMMARY #
    plot <- NA
    plot_nodes <- getNodeSet(tree, '//*[@class="plotpar"]')
    if (length(plot_nodes) > 1) {
        plot_nodes <- laply(plot_nodes, xmlValue)
        plot <- gsub("\n", "", paste(plot_nodes, collapse=""))
    }
    return(data.frame(imdb_id, plot, stringsAsFactors=F))
}

