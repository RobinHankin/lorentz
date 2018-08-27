\name{4vel}
\alias{4vel}
\alias{as.4vel}
\alias{is.consistent.4vel}
\alias{fourvelocity}
\alias{four-velocity}
\alias{4velocity}
\alias{4-velocity}
\alias{as.4vel}
\alias{is.4vel}
\alias{to3}
\alias{inner4}
\alias{inner product}
\title{Four velocities}
\description{
Create and test for four-velocities.
}
\usage{
as.4vel(u)
is.consistent.4vel(U,TOL=1e-10)
inner4(U)
to3(U)
}
\arguments{
  \item{u}{A three-velocity}
  \item{U}{A four-velocity}
  \item{TOL}{Small positive value used for tolerance}
}
\note{
  Function \code{as.4vel()} takes a three-velocity and returns a
  four-velocity.

  Function \code{inner4()} returns \eqn{U^iU_i=\eta_{ij}U^iU^j}{U^i.U_i}

  Function \code{to3()} is a low-level helper function used when
  \code{as.3vel()} is given a four-velocity.

  Function \code{is.consistent.4vel()} checks for a vector of
  four-velocities being consistent in the sense that
  \eqn{U^iU_i=-1}{U.U=-1}.
  
  There is no class \dQuote{\code{4vel}}; the emphasis is on
  three-velocities.
}
\author{Robin K. S. Hankin}
\examples{

a <- r3vel(10)
as.4vel(a)     # a four-velocity

as.3vel(as.4vel(a))-a   # should be small

inner4(as.4vel(a))   # should be -1

stopifnot(is.consistent.4vel(as.4vel(a)))


}