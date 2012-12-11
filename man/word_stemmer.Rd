\name{word_stemmer}
\alias{word_stemmer}
\title{Stem each feature in a blob of text}
\usage{
  word_stemmer(document)
}
\arguments{
  \item{document}{A blob of text}
}
\description{
  Stem each feature in a blob of text
}
\examples{
documents <- c("running runner run", "jumping jump jumped")
library(tm)
corpus <- Corpus(VectorSource(documents))
library(hidatalabs)
tm_map(corpus, word_stemmer)
}

