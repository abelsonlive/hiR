#' Automatically add leading zeros to id columns
#'
#' This function quickly and painlessly adds leading zeros to id varibles
#'
#' @param id A vector of ids
#' @param n_digits The desired length of each id.
#'
#' @export
#'
#' @examples
#'
#' ids <- c("1", "12470192401" , "30479103", "42u1p9241", "532", "3153")
#' library("hiR")
#' leading_zeros(id = ids)
leading_zeros <- function(id = NULL, n_digits = NULL) {
    if(!require("plyr")){
        install.packages("plyr")
        library("plyr")
    }
    # clean up x and get chararchter length info
    df <- data.frame(id = gsub(" ", "", as.character(id)), stringsAsFactors=F)
    df$nchar <- laply(df$id, nchar)

    # determine value of n_digits
    if(is.null(n_digits)) {
        n_digits <- max(df$nchar)
    }

    # add zeros function
    add_zeros <- function(dd) {
        n <- as.numeric(unique(dd$nchar))
        the_zeros <- rep("0", n_digits-n)
        the_zeros <- paste(the_zeros, collapse="")
        dd$id <- paste0(the_zeros, dd$id)
        return(dd)
    }
    # run
    clean_df <- ddply(df, .(nchar), add_zeros, .progress="text")
    # return
    return(as.character(clean_df$id))
}
ids <- c("1", "12470192401" , "30479103", "42u1p9241", "532", "3153")
leading_zeros(id = ids)