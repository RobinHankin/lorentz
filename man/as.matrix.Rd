\name{as.matrix.3vel}
\alias{as.matrix.3vel}
\alias{as.matrix.4vel}
\title{Coerce 3-vectors and 4-vectors to a matrix}
\description{
Coerce 3-vectors and 4-vectors to a matrix.  A convenience wrapper for
\code{unclass()}
}
\usage{
\method{as.matrix}{3vel}(x, ...)
\method{as.matrix}{4vel}(x, ...)
}
\arguments{
  \item{x}{Object of class \code{3vel} or \code{4vel}}
  \item{...}{Further arguments (currently ignored)}
}
\author{Robin K. S. Hankin}
\examples{
as.matrix(r3vel(5))
as.matrix(r4vel(5))
}
