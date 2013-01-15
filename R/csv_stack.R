#' Stack up a directory of csvs or list of data.frames
#'
#' This functions makes data stacks
#'
#' @param csvs a directory full of csv files or a list of data.frames
#'
#' @return
#' A data.frame with inconsistent columns filled in
#'
#' @export
#'
csv_stack <- function(csvs = NULL) {
    if(is(csvs, "character")) {
        # get list of file paths to csvs
        files <- list.files(csvs)
        files <- files[grep("\\.csv", files)]
        # read in csvs to a list
        csvs <- lapply(files, function(x) {read.csv(x, stringsAsFactors=F)})
    }

    # order list by number of columns in each csv
    # this will ensure that rbind.fill works properly
    col_order <- unlist(lapply(csvs, ncol))
    csvs <- csvs[order(col_order)]

    # reduce data
    rbind.fill(csvs)
}


