\name{lda}
\alias{lda}
\title{An easy-to-use and comprehensive implementation of topic modeling in R}
\usage{
  lda(text, ids = NULL, lower_case = TRUE,
    remove_stop_words = TRUE, stop_words_to_add = NULL,
    remove_numbers = TRUE, remove_punctuation = TRUE,
    remove_non_ascii = TRUE, stem_words = FALSE,
    char_range = c(3, 50), min_word_count = 5,
    n_topics = 10, n_topic_words = 20, n_iter = 1000,
    burnin = 100, alpha = 0.1, eta = 0.1,
    n_assignments = 2, doc_stats = TRUE)
}
\arguments{
  \item{text}{A character vector of text documents}

  \item{ids}{A vector of ids (to allow joining results to
  other variables). default is 1:length(text)}

  \item{lower_case}{Logical; should the function make the
  text lower case?}

  \item{remove_stop_words}{Logical; should the function
  remove stop words? NOTE: this will also make the text
  lower case}

  \item{stop_words_to_add}{A character vector of stopwords
  to add}

  \item{remove_numbers}{Logical; should the function remove
  numbers?}

  \item{remove_punctuation}{Logical; should the function
  remove punctuation?}

  \item{remove_non_ascii}{Logical; should the function
  remove non-ASCII characters?}

  \item{stem_words}{Logical; should the function stem the
  words?}

  \item{char_range}{A numeric vector of length two with low
  and high value of characters per word (inclusive!) - e.g:
  c(3,50)}

  \item{min_word_count}{The number of times a word/feature
  must occur in a text to be considered}

  \item{n_topics}{The number of topics to fit}

  \item{n_topic_words}{The number of top topic words to
  return}

  \item{n_iter}{The number of iterations}

  \item{burnin}{The number of initial iterations to ignore.
  the function adds burnin to n_iter}

  \item{alpha}{The scalar value of the dirichlet
  hyperparameter for topic proportions}

  \item{eta}{The scalar value of the dirichlet
  hyperparamater for topic multinomials}

  \item{n_assignments}{The number of assignments to return
  (returned as topic_a, topic_b etc.)}
}
\value{
  A list of: $topic_words: A table of the top n words per
  topic, n = n_topic_words $document_stats: A data.frame of
  stats about topics in each document $topic_words: A table
  of top topic words in each document
}
\description{
  lda is a wrapper for lda.collapsed.gibbs.sampler in the
  "lda" package. It fits topic models using latent
  dirichlet allocation, It provides arguments for cleaning
  the input text and tuning the parameters of the model. it
  also returns alot of useful information about the
  topics/documents in a format that you can easily join
  back to your original data this allows you to easily
  model outcomes based on the distribution of topics within
  a collection of texts
}

