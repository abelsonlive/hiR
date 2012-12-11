#' Classify the sentiment of text documents.
#'
#' This function takes a character vector of documents as an input and returns probabilistic sentiment classification.
#' This function is a slight adjustment to "classify_polarity" in the "sentiment" package.
#' WARNING: This still needs to be tweaked to return meaningul classifications.  Use the pos/neg ratio as a better metric for now.
#'
#' @param text A character vector of text blobs.
#' @param algorithm A string indicating whether to use the naive bayes algorithm or a simple voter algorithm.
#' @param pstrong A numeric specifying the probability that a strongly subjective term appears in the given text.
#' @param pweak A numeric specifying the probability that a weakly subjective term appears in the given text.
#' @param prior A numeric specifying the prior probability to use for the naive Bayes classifier.
#' @param neutral_range # A numeric vector specifying the low and high value of pos/neg ratio to classify as "neutral."
#' @param verbose A logical specifying whether to print detailed output regarding the classification process.
#' @param ... Additional arguments to pass to create_matrix in the sentiment package
#'
#' @export
#'
#' @examples
#' documents <- c("I am very happy, excited, and optimistic.",
#'                "I am very scared, annoyed, and irritated.",
#'                "Iraq's political crisis entered its second week one step closer to the potential
#'                dissolution of the government, with a call for elections by a vital coalition partner
#'                and a suicide attack that extended the spate of violence that has followed the withdrawal
#'                of U.S. troops.",
#'                "With nightfall approaching, Los Angeles authorities are urging residents to keep their
#'                outdoor lights on as police and fire officials try to catch the person or people responsible
#'                for nearly 40 arson fires in the last three days.")
#' library("hiR")
#' classify_sentiment(documents,algorithm="bayes",verbose=TRUE)

classify_sentiment <- function (text,
                       algorithm = "bayes",
                       pstrong = 0.5,
                       pweak = 1,
                       prior = 1, # prior for the ratio
                       neutral_range = c(1, 1.5), # low and high value of neutral range... INCLUSIVE!
                       verbose = FALSE, #logical; should the function print how it's classifying each documenta
                       ... # other arguments to pass to create_matrix in the sentiment package
                       ) {

    if(!require('sentiment')) {
        install.packages("sentiment")
        library("sentiment")
    }

    matrix <- create_matrix(text, ...)
    lexicon <- read.csv(system.file("data/subjectivity.csv.gz", package = "sentiment"), header = FALSE)
    counts <- list(positive = length(which(lexicon[, 3] == "positive")),
                   negative = length(which(lexicon[, 3] == "negative")),
                   total = nrow(lexicon))

    documents <- c()
    for (i in 1:nrow(matrix)) {
        if (verbose)
            print(paste("DOCUMENT", i))
        scores <- list(positive = 0, negative = 0)
        doc <- matrix[i, ]
        words <- findFreqTerms(doc, lowfreq = 1)
        for (word in words) {
            index <- pmatch(word, lexicon[, 1], nomatch = 0)
            if (index > 0) {
                entry <- lexicon[index, ]
                polarity <- as.character(entry[[2]])
                category <- as.character(entry[[3]])
                count <- counts[[category]]
                score <- pweak
                if (polarity == "strongsubj")
                  score <- pstrong
                if (algorithm == "bayes")
                  score <- abs(log(score * prior/count))
                if (verbose) {
                  print(paste("WORD:", word, "CAT:", category,
                    "POL:", polarity, "SCORE:", score))
                }
                scores[[category]] <- scores[[category]] + score
            }
        }

        if (algorithm == "bayes") {
            for (key in names(scores)) {
                count <- counts[[key]]
                total <- counts[["total"]]
                score <- abs(log(count/total))
                scores[[key]] <- scores[[key]] + score
            }
        }
        else {
            for (key in names(scores)) {
                scores[[key]] <- scores[[key]] + 1e-06
            }
        }

        best_fit <- names(scores)[which.max(unlist(scores))]
        ratio <- as.integer(abs(scores$positive/scores$negative))
        if (ratio >= neutral_range[1] & ratio <= neutral_range[2])
            best_fit <- "neutral"
            documents <- rbind(documents, c(scores$positive, scores$negative, abs(scores$positive/scores$negative), best_fit))
        if (verbose) {
            print(paste("POS:", scores$positive, "NEG:", scores$negative, "RATIO:", abs(scores$positive/scores$negative)))
            cat("\n")
        }
    }
    colnames(documents) <- c("pos", "neg", "ratio", "best_fit")
    return(documents)
}