\name{regress_text}
\alias{regress_text}
\title{Automate lasso/ridge text regression}
\usage{
  regress_text(text, y, stop_words = TRUE,
    stem_words = TRUE, stop_words_to_add = NULL,
    sparse = 0.995, family = "gaussian", alpha = 0.1,
    n_splits = 10, size = 0.8)
}
\arguments{
  \item{text,}{A charachter vector of text blobs to use as
  predictors.}

  \item{y}{The outcome variable. Its class depends on the
  family of regression selected.}

  \item{stop_words}{Logical; should the function remove
  stop words?}

  \item{stem_words}{Logical; should the function stem
  words?}

  \item{stop_words_to_add}{A character vector of additional
  stopwords}

  \item{sparse}{Level of sparsity at which a given feature
  will not be considered (0-1)}

  \item{family}{Regression type in glmnet}

  \item{alpha}{Alpha=1 is the lasso penalty, and alpha=0
  the ridge penalty.}

  \item{n_splits}{Number of times to resample data}

  \item{size}{How much of the data should be used during
  resampling for model fitting?}
}
\value{
  A list with a data.frame of terms and coefficients, and
  optimal lamda and rmse metrics for model comparison
}
\description{
  Adapted from:
  https://github.com/johnmyleswhite/TextRegression
}
\examples{
# from https://github.com/johnmyleswhite/TextRegression

text <- c('saying text is good',
          'saying text once and saying text twice is better',
          'saying text text text is best',
          'saying text once is still ok',
          'not saying it at all is bad',
          'because text is a good thing',
          'we all like text',
          'even though sometimes it is missing')

y <- c(1, 2, 3, 1, 0, 1, 1, 0)

library("hiR")
res <- regress_text(text, y)

print(res[[1]])
}

