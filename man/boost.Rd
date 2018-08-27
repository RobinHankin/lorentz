\name{boost}
\alias{boost}
\alias{rot}
\alias{thomas}
\alias{Thomas}
\alias{Thomas rotation}
\alias{wigner}
\alias{Wigner}
\alias{Wigner rotation}
\alias{precession}
\alias{boostfun}
\title{Lorentz boosts and Thomas rotation}
\description{Thomas rotation for Lorenz addition of three velocities}
\usage{
boost(u)
rot(u,v,space=TRUE)
}
\arguments{
  \item{u,v}{Three-velocities, coerced to  class \code{3vel}}
  \item{space}{Boolean, with default \code{TRUE} meaning to return just
    the spatial component of the rotation matrix and \code{FALSE}
    meaning to return the full \eqn{4\times 4}{4x4} matrix transformation}
}
\details{

}
\references{
\author{Robin K. S. Hankin}
\note{
Function \code{boost()} uses \code{crossprod()} for efficiency reasons
but is algebraically equivalent to

\code{boost(-u-v) \%*\% boost(u) \%*\% boost(v)}. 
}

\examples{
boost(as.3vel(c(0.4,-0.2,0.1)))


u <- as.3vel(c(0.3,0.4,0))
v <- as.3vel(c(0.5,-0.3,0))
w <- as.3vel(c(0.7,-0.1,0))

Lu <- boost(u)
Lv <- boost(v)
Lw <- boost(w)


boost(u) - solve(boost(-u))  # should be zero

Lu \%*\% Lv
boost(u+v)  # not the same!
boost(v+u)  # also not the same!

Lu \%*\% Lv \%*\% Lw  # associative, no brackets needed
boost(u+(v+w))  # not the same!
boost((u+v)+w)  # also not the same!

u <- as.3vel(c(0.5,0,0))
v <- as.3vel(c(0,0.5,0))

u+v
boost(u) \%*\% boost(v)   # not a pure boost (not symmetrical)

rot(u,v)
rot(v,u)    # transpose (=inverse) of rot(u,v)




}
