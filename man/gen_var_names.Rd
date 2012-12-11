\name{gen_var_names}
\alias{gen_var_names}
\title{Automatically generate variable names for count subsets.}
\usage{
  gen_var_names(roots, vars, delim = "_")
}
\arguments{
  \item{roots}{A set of names that serve as the root
  variable}

  \item{vars}{A set of names that represent the subsets of
  each root variable}

  \item{delim}{Character to separate roots and vars by.
  Defeaults to "_"}
}
\description{
  Say you were building a dataset and wanted to
  automatically generate variable names by some pattern.
  For instance, you might want to do this with population
  counts within 100 census tracts by race IE:
}
\details{
  tracts <- paste("c", rep(1:100), sep="") race -
  c("black", "white", "hispanic")

  In this case you would want to generate 300 unique
  variable names This function will generate these variable
  names automatically when provided with: 1. the "roots" -
  in the example above, the unique census tracts 2. the
  "vars" - in the example above, the unique races
}
\examples{
tracts <- paste("ct", rep(1:100), sep="")
race <- c("black", "white", "hispanic")
library(hidatalabs)
gen_var_names(roots=tracts, vars=race)
}

