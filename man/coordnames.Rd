\name{coordnames}
\alias{coordnames}
\alias{flob}
\title{Coordinate names for relativity}
\description{

Trivial function to set coordinate names to \dQuote{\code{t}},
\dQuote{\code{x}}, \dQuote{\code{y}}, \dQuote{\code{z}}.
}
\usage{
coordnames(...)
flob(x)
}
\arguments{
  \item{\dots}{Further arguments, currently ignored}
  \item{x}{A matrix}
}
\author{Robin K. S. Hankin}
\note{If anyone can think of a better name than \code{flob()} let me know}
\examples{

coordnames()

flob(diag(3))
flob(matrix(1,4,4))

## You can change the names if you wish:
coordnames <- function(x){letters[1:4]}
flob(outer(1:4,1:4))

}
