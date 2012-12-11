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
  A list data.frame of twitter handles, klout ids, and
  klout scores
}
\description{
  Retrieve klout scores for a vector of twitter handles
}
\examples{
# EXAMPLE ONE:
# simply get a scouple of klout scores
# you can use my apikey for now but it will eventually break
library("hiR")
get_klout_scores(twitter_handles = c("brianabelson", "mhkeller"), api_key="8yng356gnjg37cvn4esbtewy")

# EXAMPLE TWO:
library("hiR")
# Now we're going to scrape data from twittercounter.com to
#compare klout scores of twitter users with:
#the highest number of followers
#the highest number of friends
#the highest number of tweets

#STEP ONE: generate data.frame of urls and types
subpages <- seq(0, 80, 20)
follower_base <- 'http://twittercounter.com/pages/100'
follower_urls <- paste0(follower_base, "/", subpages)
follower_df <- data.frame(url = follower_urls,
                          list = rep("followers",  length(follower_urls)),
                          subpage = subpages,
                          stringsAsFactors = F
                          )

friend_base <- 'http://twittercounter.com/pages/friends'
friend_urls <- paste0(friend_base, "/", subpages)
friend_df <- data.frame(url = friend_urls,
                        list = rep("friends", length(friend_urls)),
                        subpage = subpages,
                        stringsAsFactors = F)

tweet_base <- 'http://twittercounter.com/pages/tweets'
tweet_urls <- paste0(tweet_base, "/", subpages)
tweet_df <- data.frame(url = tweet_urls,
                       list = rep("tweets", length(friend_urls)),
                       subpage = subpages,
                       stringsAsFactors = F)

df <- rbind(follower_df, friend_df, tweet_df)

#STEP TWO: Scrape data
#create scraping function
getHandles <- function(df) {
   #download page
    library("RCurl")
    url <- as.character(df$url)
    page <- getURL(url)
    library("XML")
    tree <- htmlTreeParse(page, useInternalNodes=T)

   #get handles
    handle_nodes <- getNodeSet(tree, '//*[
}

