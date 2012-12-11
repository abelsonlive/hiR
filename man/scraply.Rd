\name{scraply}
\alias{scraply}
\title{Scrape urls with llply, handling errors}
\usage{
  scraply(ids, scrape = scrape)
}
\arguments{
  \item{ids}{A character vector of ids/urls to feed to a
  scraping function}

  \item{scrape}{The scraping function to apply across the
  ids}
}
\value{
  a data.frame created by scrape
}
\description{
  This function is a shell for applying scraping functions
  aross a vector of urls while handling errors
}

