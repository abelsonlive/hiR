\name{classify_sentiment}
\alias{classify_sentiment}
\title{Classify the sentiment of text documents}
\usage{
  classify_sentiment(text, algorithm = "bayes",
    pstrong = 1, pweak = 0.75, prior = 1,
    neutral_range = c(1, 1.5), verbose = FALSE, ...)
}
\arguments{
  \item{text}{A character vector of text blobs.}

  \item{algorithm}{A string indicating whether to use the
  naive bayes algorithm or a simple voter algorithm.}

  \item{pstrong}{A numeric specifying the probability that
  a strongly subjective term appears in the given text.}

  \item{pweak}{A numeric specifying the probability that a
  weakly subjective term appears in the given text.}

  \item{prior}{A numeric specifying the prior probability
  to use for the naive Bayes classifier.}

  \item{neutral_range}{# A numeric vector specifying the
  low and high value of pos/neg ratio to classify as
  "neutral."}

  \item{verbose}{A logical specifying whether to print
  detailed output regarding the classification process.}

  \item{...}{Additional arguments to pass to create_matrix
  in the sentiment package}
}
\description{
  This function takes a character vector of documents as an
  input and returns probabilistic sentiment classification.
  This function is a slight adjustment to
  "classify_polarity" in the "sentiment" package. WARNING:
  This still needs to be tweaked to return meaningul
  classifications. Use the pos/neg ratio as a better metric
  for now.
}
\examples{
documents <- c("I am very happy, excited, and optimistic.",
               "I am very scared, annoyed, and irritated.",
               "Iraq's political crisis entered its second
               week one step closer to the potential
               dissolution of the government, with a call
               for elections by a vital coalition partner
               and a suicide attack that extended the spate
               of violence that has followed the withdrawal
               of U.S. troops.",
               "With nightfall approaching, Los Angeles
               authorities are urging residents to keep their
               outdoor lights on as police and fire officials
               try to catch the person or people responsible
               for nearly 40 arson fires in the last three days.")
library("hiR")
classify_sentiment(documents,algorithm="bayes",verbose=TRUE)
}

