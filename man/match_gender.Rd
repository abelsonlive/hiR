\name{match_gender}
\alias{match_gender}
\title{Retrieve gender given a vector of first names \n Warning: only about 60% accurate with current dataset.}
\usage{
  match_gender(names, full = FALSE)
}
\arguments{
  \item{names}{A character vector of names}

  \item{full}{Logical; should the function try to extract
  the first name?  WARNING: names like "sarah ann" will
  turn into "sarah"}
}
\value{
  about 60-70 percent of firstnames from twitter accurately
  classified
}
\description{
  Retrieve gender given a vector of first names \n Warning:
  only about 60% accurate with current dataset.
}
\examples{
names <- c("cindy", "sally", "bob", "joe")
library("hiR")
match_gender(names)
}

