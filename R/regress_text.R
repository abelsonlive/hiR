#' Automate lasso/ridge text regression
#'
#' Adapted from: https://github.com/johnmyleswhite/TextRegression
#'
#' @param text, A charachter vector of text blobs to use as predictors.
#' @param y The outcome variable. Its class depends on the family of regression selected.
#' @param stop_words Logical; should the function remove stop words?
#' @param stem_words Logical; should the function stem words?
#' @param stop_words_to_add A character vector of additional stopwords
#' @param sparse Level of sparsity at which a given feature will not be considered (0-1)
#' @param family Regression type in glmnet
#' @param alpha Alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
#' @param n_splits Number of times to resample data
#' @param size How much of the data should be used during resampling for model fitting?
#'
#' @return
#' A list with a data.frame of terms and coefficients,
#' and optimal lamda and rmse metrics for model comparison
#'
#' @export
#'
#' @examples
#' # from https://github.com/johnmyleswhite/TextRegression
#'
#' text <- c('saying text is good',
#'           'saying text once and saying text twice is better',
#'           'saying text text text is best',
#'           'saying text once is still ok',
#'           'not saying it at all is bad',
#'           'because text is a good thing',
#'           'we all like text',
#'           'even though sometimes it is missing')
#'
#' y <- c(1, 2, 3, 1, 0, 1, 1, 0)
#'
#' library("hiR")
#' res <- regress_text(text, y)
#'
#' print(res[[1]])

regress_text <- function(text, # charachter vector of text blobs
                         y, # outcome variable
                         stop_words = TRUE, # logical; should the function stem words?
                         stem_words = TRUE, # logical; should the function stem words?
                         stop_words_to_add = NULL, # a character vector of additional stopwords
                         sparse = 0.99, # level of sparsity at which a given feature will not be considered
                         family = 'gaussian', # family argument for glmnet
                         alpha = 0.1, # alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
                         n_splits = 10, # number of times to resample data
                         size = 0.8 # how much of the data should be used during resampling for model fitting?
                         ) {
# LIBRARIES
    if(!require("glmnet")) {
        install.packages("glmnet")
        library("glmnet")
    }
    if(!require("tm")) {
        install.packages("tm")
        library("tm")
    }
    if(!require("Rstem")) {
        install.packages("Rstem", repos="http://www.omegahat.org/R", type="source")
        library("Rstem")
    }


  print("cleaning text / removing stopwords...")
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, tolower)
  if(stop_words) {
    corpus <- tm_map(corpus, removeWords, c(stopwords('english'), stop_words_to_add))
  }
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)

  if (stem_words) {
    print("stemming words...")
    # generate stemming function
    wordStemmer <- function(x) {

        require("Rstem")
        removeNonASCII <- function(x) {
            iconv(x, "latin1", "ASCII", sub="")
        }
        x <- laply(x, removeNonASCII)
        words <- unlist(strsplit(x, " "))

        #ensure all empty words and words with more than 50 characters are removed
        nchars <- laply(words, nchar)
        clean_words <- words[which(nchars > 1 & nchars < 50)]

        # stem words
        stemmed_words <- wordStem(clean_words)

        # collapse back into one blob
        output <- paste(stemmed_words, collapse=" ")
        return(output)
    }
    # run stemming function
    corpus <- tm_map(corpus, wordStemmer)
  }

  # create and clean document term matrix
  print("prepping data...")
  corpus <- Corpus(VectorSource(corpus))
  dtm <- DocumentTermMatrix(corpus)
  dtm <- removeSparseTerms(dtm, sparse)

  # dtm to matrix function
  # HOW CAN I VECTORIZE THIS FUNCTION!
  dtm.to.Matrix <- function(dtm) {
    m <- Matrix(0, nrow = dtm$nrow, ncol = dtm$ncol, sparse = TRUE)

    for (index in 1:length(dtm$i)) {
      m[dtm$i[index], dtm$j[index]] <- dtm$v[index]
    }
    return(m)
  }

  # fit model
  x <- dtm.to.Matrix(dtm)
  y <- as.vector(y)

  print("fitting initial model...")
  regularized.fit <- glmnet(x, y, family = family, alpha = alpha)

  lambdas <- regularized.fit$lambda

  # Calculate number of splits based on time required to perform original model fit.
  # Or based on data set size?
  print("resampling...")
  performance <- data.frame()

  for (i in 1:n_splits) {
    indices <- sample(1:nrow(x), round(size * nrow(x)))

    training.x <- x[indices, ]
    training.y <- y[indices]
    test.x <- x[-indices, ]
    test.y <- y[-indices]

    for (lambda in lambdas) {
      resampling.fit <- glmnet(training.x, training.y, family=family, alpha = alpha)
      predicted.y <- as.numeric(predict(resampling.fit, newx = test.x, s = lambda))
      rmse <- sqrt(mean((predicted.y - test.y) ^ 2))
      performance <- rbind(performance, data.frame(Split = i, Lambda = lambda, RMSE = rmse))
    }
    print(paste("iteration", i, "of", n_splits, "..."))
  }

  print("preparing output...")
  mean.rmse <- ddply(performance,
                     'Lambda',
                     function (df) {
                       with(df, data.frame(RMSE = mean(RMSE)))
                     })

  optimal.lambda <- with(mean.rmse, max(Lambda[which(RMSE == min(RMSE))]))
  optimal.rmse <- with(subset(mean.rmse, Lambda == optimal.lambda), RMSE)
  coefficients <- as.numeric(coef(regularized.fit, s = optimal.lambda)[, 1])
  terms <- data.frame(coef = coefficients, term = c('(Intercept)', colnames(dtm)))
  terms <- terms[order(terms$coef, decreasing=TRUE),]
  list(terms=terms, lambda=optimal.lambda, rmse=optimal.rmse)
}