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

The salient use-case is \code{u[1:5] <- u[1]} in which the elements of
\code{u} three velocities.

}
\examples{


u <- r3vel(10)
u[1:4]
u[5:6] <- 0

u[7:8] <- u[1]

u[,1] <- 0.1


}
