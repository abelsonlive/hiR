#' output .dat document term format for ctm algorithm
#'
#' @param corpus a corpus / character vector of text documents
#'
#' @export
#'
#' @examples
#' #gen_ctm_data(corpus, "ctm.dat")

gen_ctm_data <- function(corpus, file="ctm.dat") {
    corpus <- as.character(corpus)
    format_row <- function(doc) {
        words <- str_trim(unlist(strsplit(doc, " ")))
        words <- words[words!=""]
        t  <- table(words)
        features <- str_trim(names(t))
        counts <- as.numeric(t)
        data_lines <- paste(features, counts, sep=":")
        data_line <- paste(data_lines, collapse=" ")
        line <- paste(length(features), data_line)
        return(line)
     }
     data <- laply(corpus, format_row, .progress="text")
     cat("\n", "writing", file, "to file", "\n")
     write(data, file)
}