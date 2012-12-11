\name{match_gender}
\alias{match_gender}
\title{Retrieve gender given a vector of first names}
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
  About 60-70 percent of first names from twitter
  accurately classified by gender
}
\description{
  only about 60% accurate with current names dataset.
}
\examples{
names <- c("cindy", "sally", "bob", "joe")
library("hiR")
match_gender(names)
}

