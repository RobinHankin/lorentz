\name{cosines}
\alias{cosines}
\alias{cosine}
\alias{dcosines}
\alias{direction.cosines}
\title{Direction cosines}
\description{
Given a vector of three-velocities, returns their direction cosines
}
\usage{
cosines(u, drop = TRUE)
}
\arguments{
  \item{u}{A vector of three-velocities}
  \item{drop}{Boolean, with default \code{TRUE} meaning to coerce return
    value from a one-row matrix to a vector, and \code{FALSE} meaning
    to consistently return a matrix}
}
\author{Robin K. S. Hankin}
\examples{

cosines(r3vel(7))


cosines(r3vel(1),drop=TRUE)
cosines(r3vel(1),drop=FALSE)
}
