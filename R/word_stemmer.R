#' Stem each feature in a blob of text
#'
#' A vecotrizable wrapper for wordStem in the Rstem package
#'
#' @param document A blob of text
#'
#' @export
#'
#' @examples
#' documents <- c("running runner run", "jumping jump jumped")
#' library("tm")
#' corpus <- Corpus(VectorSource(documents))
#' library("hiR")
#' as.character(tm_map(corpus, word_stemmer))

word_stemmer <- function(document) {
    if(!require("Rstem")){
        install.packages("Rstem", repos="http://www.omegahat.org/R", type="source")
        library("Rstem")
    }
    words <- str_trim(unlist(strsplit(document, " ")))
    words <- words[words!=""]
    # stem words
    stemmed_words <- wordStem(words)
    # collapse back into one blob
    output <- paste(stemmed_words, collapse=" ")
    return(output)
}
