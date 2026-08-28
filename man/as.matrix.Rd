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
\note{

 Coercing to a matrix simply removes the \proglang{S3} class \code{3vel}
 or \code{4vel}.  To convert a three-velocity or a four-velocity to a
 \eqn{4\times 4}{4*4} matrix, use \code{boost()}
 }
\seealso{\code{\link{boost}}}  
\author{Robin K. S. Hankin}
\examples{
as.matrix(r3vel(5))
as.matrix(r4vel(5))
}
