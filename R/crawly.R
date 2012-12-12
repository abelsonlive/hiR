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
crawly <- function(root,
                   url_fx = create_urls,
                   link_fx = extract_links,
                   page_fx = scrape_page,
                   dump_urls = TRUE,
                   dump_sub_urls = TRUE,
                   dump_pages = FALSE,
                   steps = 10,
                   name = "crawl",
                   dir=NULL
                   ) {
    # start
    start <- Sys.time()
    print( paste( "starting crawly", start ) )

    # initialize directory
    setwd(getwd())
    system(paste("mkdir", mame))
    setwd(name)

    #libraries
    if(!require("plyr") ) {
        install.packages("plyr")
        library("plyr")
    }

    # create function wrapper
    apply_fx <- function(x, fx) {
            FUN <- match.fun(fx)
            FUN(x)
    }

    # if implicitly requested, apply url generation function
    if(length(root)==1) {
        urls <- apply_fx(root, url_fx)
        if (dump_urls) {
            print("generating urls...")
            system("mkdir urls")
            write(urls, paste0("urls/", name, "_urls.txt"))
        }
    }

    # extract sub urls
    if (dump_sub_urls) {
        system("mkdir sub_urls")
        dir <- "sub_urls"
    }
    print("extracting links...")
    links <- scraply(urls,
                     fx = function(x) {apply_fx(x, link_fx)},
                     dump=dump_sub_urls, dir=dir, steps=1, name=name)
    write(links$log, paste0(dir, paste0(dir, "/", name, "_links_log.txt")))

    # scrape pages
    if (dump_pages) {
        system("mkdir pages")
        dir <- "pages"
    }
    print("scraping pages...")
    pages <- scraply(links$urls,
                     fx = function(x) {apply_fx(x, page_fx)},
                     dump=dump_pages, steps = steps, dir=dir, name=name)
    write(links$log, paste0(dir, "/", name, "_pages_log.txt"))

    #end

    # CALCULATE JOB LENGTH
    end <- Sys.time()
    job_length <- round(difftime(end, start, units="mins"), digits=2)
    msg1 <- paste("crawly finished at:", end)
    msg2 <- paste("job took:", job_length, "minutes")
    the_msg <- paste(msg1, cat("/n"),  msg2)
    end <- Sys.time()

    # depending on the type of function, end accordinlgy:
    if (steps == 1) {
        print(the_msg)
        return(pages)

    } else {
        print(the_msg)
        print(paste("there are", steps, "files now in", paste0(name, "/", dir)))
        stop("goodbye!")
    }
}