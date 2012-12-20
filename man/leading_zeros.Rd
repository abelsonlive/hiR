\name{leading_zeros}
\alias{leading_zeros}
\title{Automatically add leading zeros to id columns}
\usage{
  leading_zeros(ids = NULL, n_digits = NULL)
}
\arguments{
  \item{ids}{A vector of ids to add zeros to.}

  \item{n_digits}{The desired length of each id. if NULL,
  this will be the maximum number of characters of a single
  id.}
}
\description{
  This function quickly and painlessly adds leading zeros
  to id varibles
}
\examples{
ids <- c("1", "12470192401" , "30479103", "42u1p9241", "532", "3153")
library("hiR")
leading_zeros(ids)
}

