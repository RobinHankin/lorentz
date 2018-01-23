\name{Extract.3vel}
\alias{[.3vel}
\alias{[<-.3vel}
\title{Extract or replace parts of three-velocity}
\description{
  Extract or replace subsets of three-velocities
}
\arguments{
  \item{x}{A three-vector}
  \item{index}{elements to extract or replace}
  \item{value}{replacement value}
}
\details{
These methods (should) work as expected: a three-vector is a
three-column matrix with rows corresponding to three-velocities.

The replacement method uses the special value \dQuote{0} which is
coerced to \eqn{\mathbf{0}}{0}.
}
\examples{


u <- r3vel(10)
u[1:4]
u[5:6] <- 0

}
