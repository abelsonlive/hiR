\name{pretty_coefs}
\alias{pretty_coefs}
\title{Create Pretty Coefficient Plots}
\usage{
  pretty_coefs(m, title = "", pval = 0.05, null = 0)
}
\arguments{
  \item{m}{A model from glm, lm, etc.}

  \item{title}{The title for the plot}

  \item{pval}{the p-value at which to color significant
  coefficients blue}

  \item{null}{the value at which a coefficient is null (0
  or 1, usually)}
}
\value{
  A pretty coefficient plot
}
\description{
  Create Pretty Coefficient Plots
}
\examples{
y = rnorm(100)
x1 = rnorm(100)
x2 = rnorm(100)
x3 = rnorm(100)
x4 = rnorm(100)
m <- glm(y ~ x1 + x2 + x3 + x4)
pretty_coefs(m)
}

