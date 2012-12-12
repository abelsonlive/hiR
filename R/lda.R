#' An easy-to-use and comprehensive implementation of topic modeling in R
#'
#' lda is a wrapper for lda.collapsed.gibbs.sampler in the "lda" package.
#' It fits topic models using latent dirichlet allocation,
#' It provides arguments for cleaning the input text and tuning the parameters of the model.
#' it also returns alot of useful information about the topics/documents in a format that you can easily join back to your original data
#' this allows you to easily model outcomes based on the distribution of topics within a collection of texts
#'
#' @param text A character vector of text documents
#' @param ids A vector of ids (to allow joining results to other variables). default is 1:length(text)
#' @param lower_case Logical; should the function make the text lower case?
#' @param remove_stop_words Logical; should the function remove stop words? NOTE: this will also make the text lower case
#' @param stop_words_to_add A character vector of stopwords to add
#' @param remove_numbers Logical; should the function remove numbers?
#' @param remove_punctuation Logical; should the function remove punctuation?
#' @param remove_non_ascii Logical; should the function remove non-ASCII characters?
#' @param stem_words Logical; should the function stem the words?
#' @param char_range A numeric vector of length two with low and high value of characters per word (inclusive!) - e.g: c(3,50)
#' @param min_word_count The number of times a word/feature must occur in a text to be considered
#' @param n_topics The number of topics to fit
#' @param n_topic_words The number of top topic words to return
#' @param n_iter The number of iterations
#' @param burnin The number of initial iterations to ignore. the function adds burnin to n_iter
#' @param alpha The scalar value of the dirichlet hyperparameter for topic proportions
#' @param eta The scalar value of the dirichlet hyperparamater for topic multinomials
#' @param n_assignments The number of assignments to return (returned as topic_a, topic_b etc.)
#'
#' @return
#' A list of:
#' $topic_words: A table of the top n words per topic, n = n_topic_words
#' $document_stats: A data.frame of stats about topics in each document
#' $topic_words: A table of top topic words in each document
#'
#' @export
#'

lda <- function(

    # DATA #
    text,
    ids = NULL,

    # CLEANING #
    lower_case = TRUE,
    remove_stop_words = TRUE,
    stop_words_to_add = NULL,
    remove_numbers = TRUE,
    remove_punctuation = TRUE,
    remove_non_ascii = TRUE,
    stem_words = FALSE,
    char_range = c(2,50),
    min_word_count = 5,

    # MODEL PARAMETERS #
    n_topics = 10,
    n_topic_words = 20,
    n_iter = 1000,
    burnin = 100,
    alpha = 0.1,
    eta = 0.1,

    # OUTPUT #
    n_assignments = 2

    ) {

# LIBRARIES
    if(!require("tm")) {
        install.packages("tm")
        library("tm")
    }
    if(!require("lda")) {
        install.packages("lda")
        library("lda")
    }
    if(!require("plyr")) {
        install.packages("plyr")
        library("plyr")
    }
    if(!require("stringr")) {
        install.packages("stringr")
        library("stringr")
    }
    if(!require("Rstem")) {
        install.packages("Rstem", repos="http://www.omegahat.org/R", type="source")
        library("Rstem")
    }

# start time (for calculating the time it takes for function to run)
    start <- Sys.time()

# gen id var if NULL
    if(is.null(ids)) {
        ids <- 1:length(text)
    }

# META VARIABLES - RAW TEXT
    # total number of characters/ features / unique features
    docStats <- function(x) {
        # length of document
        len <- nchar(x)

        # split words
        words <- str_trim(unlist(strsplit(x, " ")))
        words <- words[words!=""]

        # calculate average word length
        nchars <- laply(words, nchar)
        len_word <- mean(nchars)

        # count features
        n_feat <- length(words)
        n_unq_feat <- length(unique(words))

        # return stats
        return(data.frame(len, len_word, n_feat, n_unq_feat))
    }
    features_raw <- ldply(text, docStats)
    names(features_raw) <- paste0(names(features_raw),"_raw")

# CLEAN THE INPUT TEXT #
    # convert text to corpus
    corpus <- Corpus(VectorSource(text))

    # standardize case
    if (lower_case) {
        corpus <- tm_map(corpus, tolower)
    }

    # remove stopwords / numbers / punctuation / whitespace
    if (remove_stop_words) {
        corpus <- tm_map(corpus, tolower)
        print("removing stop words...")
        stop_words <- c(stopwords('english'), stop_words_to_add)
        corpus <- tm_map(corpus, removeWords, stop_words)
    }

    # remove numbers / punctuation / strip whitespace
    print("cleaning text...")
    if (remove_numbers) {
        corpus <- tm_map(corpus, removeNumbers)
    }
    if (remove_punctuation) {
        removePunct <- function(x) {
            gsub("[[:punct:]]", " ", x)
        }
        corpus <- tm_map(corpus, removePunct)
    }

    # remove non-ASCII characters
    if (remove_non_ascii) {
        removeNonASCII <- function(x) {
            iconv(x, "latin1", "ASCII", sub="")
        }
        corpus <- tm_map(corpus, removeNonASCII)
    }
    corpus <- tm_map(corpus, stripWhitespace)

    # filter out "words" that have more than 255
    # these will break the stemming function
    charFilter <- function(x) {
        words <- str_trim(unlist(strsplit(x, " ")))
        nchars <- laply(words, nchar)
        clean_words <- words[which(nchars <= 255)]
        output <- paste(clean_words, collapse=" ")
        return(output)
    }
    corpus <- tm_map(corpus, charFilter)
    corpus <- tm_map(corpus, stripWhitespace)

    # stem words
    if(stem_words) {
        print("stemming words...")
        # generate stemming function
        wordStemmer <- function(x) {
            words <- str_trim(unlist(strsplit(x, " ")))
            words <- words[words!=""]
            # stem words
            stemmed_words <- wordStem(words)
            # collapse back into one blob
            output <- paste(stemmed_words, collapse=" ")
            return(output)
        }
        # run stemming function
        corpus <- tm_map(corpus, wordStemmer)
    }

    # filter out words that fall outside of desired char_range
    charFilter2 <- function(x) {
        words <- str_trim(unlist(strsplit(x, " ")))
        nchars <- laply(words, nchar)
        arg <- which(nchars >= char_range[1] & nchars <= char_range[2])
        clean_words <- words[arg]
        output <- str_trim(paste(clean_words, collapse=" "))
        return(output)
    }
    corpus <- tm_map(corpus, charFilter2)

    # strip white space again for good measure
    corpus <- tm_map(corpus, stripWhitespace)

    # convert corpus back to character vector for lexicalizing
    text <- as.character(corpus)

# META VARIABLES - CLEAN TEXT
    # total number of characters / features / unique features
    features_clean <- ldply(text, docStats)
    names(features_clean) <- paste0(names(features_clean),"_clean")

# CREATE / FILTER LEXICON
    # lexicalize text
    print("lexicalizing text...")
    corpus <- lexicalize(text, sep=" ", count=1)

    # only keep words that appear at least twice.
    N <- min_word_count
    keep <- corpus$vocab[word.counts(corpus$documents, corpus$vocab) >= N]

    # re-lexicalize, using this subsetted vocabulary
    documents <- lexicalize(text, lower=TRUE, vocab=keep)

# FIT TOPICS
    # gibbs sampling
    # K is the number of topics
    print("fitting topics...")
    K <- n_topics
    n_iter <- n_iter + burnin
    result <- lda.collapsed.gibbs.sampler(documents, K, keep, n_iter, alpha, eta)

# PREPARE OUTPUT
    print("preparing output...")

    # top words by document
    predictions <- t(predictive.distribution(result$document_sums, result$topics, 0.1, 0.1))
    document_words <- data.frame(top.topic.words(predictions,
                                                 n_topic_words,
                                                 by.score = TRUE))
    names(document_words) <- ids

    # top words by topic
    topic_words <- data.frame(top.topic.words(result$topics,
                                              num.words = n_topic_words,
                                              by.score = TRUE))
    names(topic_words) <- paste0("topic_", 1:K)

    # topics by documents stats
    raw <- as.data.frame(t(result$document_sums))
    names(raw) <- 1:K
    n_docs  <- nrow(raw)
    topics <- data.frame(id = ids, matrix(0, nrow = n_docs, ncol=2*K))
    names(topics) <- c("id", paste0("n_topic_", 1:K), paste0("p_topic_", 1:K))

    # add assignment variables dynamically
    topic_ass_vars <- paste0("topic_", letters[1:n_assignments])
    topics[,topic_ass_vars] <- 0

    # assign primary and secondary topic(s), get distribution topics by document
    for(doc in 1:n_docs) {
        assignments <- as.numeric(names(sort(raw[doc,1:K], decreasing=TRUE)))
        topics[doc, topic_ass_vars] <- assignments[1:n_assignments]
        topics[doc, grep("n_topic_[0-9]+", names(topics))] <- raw[doc,]
        topics[doc, grep("p_topic_[0-9]+", names(topics))] <- raw[doc,] / sum(raw[doc,])
    }

    # add meta variables
    document_stats <- data.frame(topics, features_raw, features_clean)

# CALCULATE JOB LENGTH
    end <- Sys.time()
    job_length <- round(difftime(end, start, units="mins"), digits=2)
    print(paste("lda finished at:", end))
    print(paste("job took:", job_length, "minutes"))

# RETURN OUTPUT
    return(list(topic_words = topic_words,
                document_stats = document_stats,
                document_words = document_words))
}