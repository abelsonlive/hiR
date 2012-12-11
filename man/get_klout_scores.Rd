\name{get_klout_scores}
\alias{get_klout_scores}
\title{Retrieve klout scores for a vector of twitter handles}
\usage{
  get_klout_scores(twitter_handles, api_key,
    na_omit = TRUE)
}
\arguments{
  \item{twitter_handles}{A charachter vector of twitter
  handles - with or without "@"}

  \item{api_key}{Your api key from
  http://klout.com/s/developers/}

  \item{na_omit}{Logical; should the function remove
  handles that don't have klout scores}
}
\value{
  A data.frame of twitter handles, klout ids, and klout
  scores
}
\description{
  Retrieve klout scores for a vector of twitter handles
}
\examples{
# simply get a scouple of klout scores
# you can use my apikey for now but it will eventually break
library("hiR")
get_klout_scores(twitter_handles = c("brianabelson", "mhkeller"), api_key="8yng356gnjg37cvn4esbtewy")
# see ex/get_klout_scores_ex.R for a more detailed use case
}

