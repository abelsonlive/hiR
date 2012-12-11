\name{scraply}
\alias{scraply}
\title{Scrape urls with llply, handling errors
This function is a shell for applying scraping
functions aross a vector of urls while handling errors}
\usage{
  scraply(ids, scrape = NULL)
}
\arguments{
  \item{ids}{character vector of ids/urls to feed to a
  scraping function}

  \item{scraping}{function}
}
\value{
  a data.frame created by scrape
}
\description{
  Scrape urls with llply, handling errors This function is
  a shell for applying scraping functions aross a vector of
  urls while handling errors
}

