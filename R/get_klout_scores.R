#' Retrieve klout scores for a vector of twitter handles
#'
#' @param twitter_handles A charachter vector of twitter handles - with or without "@@"
#' @param api_key Your api key from http://klout.com/s/developers/
#' @param na_omit Logical; should the function remove handles that don't have klout scores
#'
#' @return A data.frame of twitter handles, klout ids, and klout scores
#'
#' @export
#'
#' @examples
#' # simply get a couple of klout scores
#' # you can use my apikey for now but it will eventually break
#' library("hiR")
#' get_klout_scores(twitter_handles = c("brianabelson", "hinstitute"), api_key="8yng356gnjg37cvn4esbtewy")
#' # see inst/docs/get_klout_scores_ex.R for a more detailed use case

get_klout_scores <- function(twitter_handles,
                             api_key,
                             na_omit=TRUE
                             ) {
    #libraries
    if(!require("rjson")){
        install.packages("rjson")
        library("rjson")
    }
    if(!require("RCurl")){
        install.packages("RCurl")
        library("RCurl")
    }
    if(!require("plyr")){
        install.packages("plyr")
        library("plyr")
    }
    if(!require("stringr")){
        install.packages("stringr")
        library("stringr")
    }
    # clean twitter hadles
    twitter_handles <- gsub("@", "", twitter_handles)

    # step one: get klout ids
    getID <- function(twitter_handle) {
            base <- 'http://api.klout.com/v2/identity.json/twitter?screenName='
            url <- paste0(base, twitter_handle, "&key=", api_key)
            out <- try(fromJSON(getURL(url))$id, TRUE)
            if(class(out)=='try-error'){
                id <- NA
            } else {
                id <- str_trim(out)
            }
            return(id)
        }
    print("Fetching Klout Ids...")
    ids <- laply(twitter_handles, getID, .progress='text')

    # announce errors
    exists <- !is.na(ids)
    twitter_handles_TRUE <- twitter_handles[exists]

    if(length(twitter_handles_TRUE) < length(twitter_handles)) {

        warning(paste("No Klout Scores for:",
                      paste(twitter_handles[is.na(ids)], collapse=" "))
                )

        # partition output by error status
        exists <- !is.na(ids)
        ids <- ids[exists]
        twitter_handles_TRUE <- twitter_handles[exists]
        twitter_handles_FALSE <- twitter_handles[!exists]
    }

    # step two:
    getScore <- function(id) {
            url <- paste0('http://api.klout.com/v2/user.json/', id,'/score', '?key=', api_key)
            out <- try(fromJSON(getURL(url))$score, TRUE)
            if(class(out)=='try-error'){
                score <- NA
            } else {
                score <- str_trim(out)
            }
            return(score)
        }
    print("Fetching Klout Scores...")
    cat("\n")
    score <- laply(ids, getScore, .progress='text')

    # prepare output, leave NAs
    output_TRUE <- data.frame(handle = twitter_handles_TRUE, id=ids, score=score, stringsAsFactors=F)
    if(na_omit){
        output <- output_TRUE
    } else {
        output_FALSE <- data.frame(handle = twitter_handles_FALSE, stringsAsFactors=F)
        output_FALSE$id <- NA
        output_FALSE$score <- NA
        output <- data.frame(rbind(output_TRUE, output_FALSE), stringsAsFactors=F)
    }

    output$score <- as.numeric(str_trim(output$score))
    return(output)
 }