#' Retrieve gender given a vector of first names \n Warning: only about 60% accurate with current dataset.
#'
#' @param names A character vector of names
#' @param full Logical; should the function try to extract the first name?
#'            WARNING: names like "sarah ann" will turn into "sarah"
#'
#' @export
#'
#' @return
#' about 60-70 percent of first names from twitter accurately classified
#'
#' @examples
#' names <- c("cindy", "sally", "bob", "joe")
#' library("hiR")
#' match_gender(names)

match_gender <- function(names,
                         full=FALSE
                         ) {
    # libraries
    if(!require("plyr")) {
        install.packages("plyr")
        library("plyr")
    }
    if(!require("stringr")) {
        install.packages("stringr")
        library("stringr")
    }
    # read in names database
    names_db <- read.csv("http://dl.dropbox.com/u/6535582/HI_Files/hiR/gender_match/names.csv", stringsAsFactors=F)

    # clean up input names
    names <- str_trim(as.character(names))

    # if requested, extract first names
    if(full){
        print(paste("extracting first names..."))
        extractFirstName <- function(name){
            first_name <- unlist(str_split(name, " "))[1]
            return(first_name)
        }
        names <- laply(names, extractFirstName, .progress="text")
    }

    # match first names
    match <- function(name) {
         test <- names_db[which(names_db$name==name),]
         if(nrow(test) > 0) {
            gender <- test$gender[1]
         } else {
            gender = NA
         }
    return(data.frame(name, gender, stringsAsFactors=F))
    }
    print(paste("matching names..."))
    output <- ldply(names, match, .progress="text")
    return(output)
}