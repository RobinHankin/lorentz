\name{threevel}
\alias{3vel}
\alias{as.3vel}
\alias{is.3vel}
\alias{length.vec}
\alias{names.vec}
\alias{names<-.vec}
\alias{threevel}
\alias{3velocity}
\alias{3-velocity}
\alias{threevelocity}
\alias{three-velocity}
\title{Three velocities}
\description{
Create and test for three-velocities, \code{3vel} objects.
}
\usage{
`3vel`(n)
threevel(n)
as.3vel(x)
is.3vel(x)
\method{length}{vec}(x)
\method{names}{vec}(x)
\method{names}{vec}(x) <- value
}
\arguments{
  \item{n}{In function \code{3vel()}, number of three velocities to
    create}
  \item{x,value}{Vectors of three-velocities}
}
\note{

  Class \code{vel} is a virtual class containing classes \code{3vel} and
  \code{4vel}. 

  Function \code{threevel()} is a convenience wrapper for \code{3vel()}.
  }
\author{Robin K. S. Hankin}
\examples{

U <- r4vel(7)
as.4vel(as.3vel(U)) # equal to U, to numerical precision

x <- as.3vel(1:3/4)
u <- as.3vel(matrix(runif(30)/10,ncol=3))

names(u) <- letters[1:10]

x+u
u+x  # not equal



}
