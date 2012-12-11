\name{match_gender}
\alias{match_gender}
\title{Retrieve gender given a vector of first names}
\usage{
  match_gender(names, full = FALSE)
}
\arguments{
  \item{names}{A character vecotr of names}

  \item{full}{Logical; should the function try to extract
  the first name?  WARNING: names like "sarah ann" will
  turn into "sarah"}
}
\description{
  Retrieve gender given a vector of first names
}
\examples{
names <- c("cindy", "sally", "bob", "joe")
library("hiR")
match_gender(names)
}

