\name{Extract.3vel}
\alias{Extract.3vel}
\alias{extract.3vel}
\alias{[.vel}
\alias{[.vec}
\alias{[<-.vel}
\alias{[.3vel}
\alias{[<-.3vel}
\alias{[.4vel}
\alias{[<-.4vel}
\title{Extract or replace parts of three-velocity}
\description{
  Extract or replace subsets of three-velocities
}
\usage{
\method{[}{vec}(x, i, j, drop=TRUE, ...)
\method{[}{3vel}(x, i, j) <-  value
\method{[}{4vel}(x, i, j) <-  value
}
\arguments{
    \item{x}{A three-vector}
    \item{i,j}{elements to extract or replace}
    \item{value}{replacement value}
    \item{drop}{Boolean, with default \code{TRUE} meaning to drop values}
    \item{...}{Further arguments, currently ignored}
}
\details{
These methods (should) work as expected: an object of class
\code{3vel} is a three-column matrix with rows corresponding to
three-velocities; a single argument is interpreted as a row number.
Salient use-cases are \code{u[1:5] <- u[1]} and \code{u[1] <- 0}.

To extract a single component, pass a second index: \code{u[,1]}
returns the x- component of the three-velocity.

Extraction functions take a \code{drop} argument, except for \code{x[i]}
which returns a \code{vec} object.

Currently, \code{u[]} returns \code{u} but I am not sure this is
desirable.  Maybe it should return \code{unclass(u)} or perhaps
\code{c(unclass(u))}.

Use idiom \code{u[] <- x} to replace entries of \code{u} elementwise.

}
\examples{

u <- r3vel(10)
u[1:4]
u[5:6] <- 0

u[7:8] <- u[1]

u[,1] <- 0.1

}
