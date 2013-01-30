#' Stack up a directory of csvs or list of data.frames
#'
#' This functions makes data stacks
#'
#' @param csvs a directory full of csv files or a list of data.frames
#' @pattern an optional regex to match file paths in the directory
#'
#' @return
#' A data.frame with inconsistent columns filled in
#'
#' @export
#'
csv_stack <- function(csvs = NULL, pattern=NULL) {
    if(is(csvs, "character")) {
        # get list of file paths to csvs
        files <- list.files(csvs)
        if(is.null(pattern)){
            pattern = "\\.csv"
        }
        files <- paste(csvs, files[grep(pattern, files)], sep="/")
        # read in csvs to a list
        cat("reading in csvs...", "\n")
        csvs <- llply(files, function(x) {read.csv(x, stringsAsFactors=F)}, .progress="text")
    }

    # order list by number of columns in each csv
    # this will ensure that rbind.fill works properly
    col_order <- unlist(lapply(csvs, ncol))
    csvs <- csvs[order(col_order)]

    cat("combining csvs...", "\n")
    # reduce data
    rbind.fill(csvs)
}


