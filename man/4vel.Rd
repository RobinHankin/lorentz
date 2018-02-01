\name{4vel}
\alias{4vel}
\alias{as.4vel}
\alias{is.consistent.4vel}
\alias{to3}
\alias{to4}
\title{Four velocities}
\description{
Various R idiom to create and test for four-velocities.
}
\usage{
to3(U)
to4(u)
is.consistent.4vel(U,TOL=1e-10)
}
\arguments{
  \item{u}{A three-velocity}
  \item{U}{A four-velocity}
  \item{TOL}{Small positive value used for tolerance}
}
\note{
  Function \code{to3()} takes a four-velocity and returns a
  three-velocity; function \code{to4()} takes a three-velocity and
  returns a four-velocity.
  
  There is no class \dQuote{\code{4vel}}; the emphasis is on
  three-velocities.
}
\author{Robin K. S. Hankin}
\examples{

a <- r3vel(10)
to4(a)

to3(to4(a)) - a   # should be small


}