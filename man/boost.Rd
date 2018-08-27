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
\title{Lorentz transformations}
\description{Lorenz transformations: boosts and rotations}
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
\value{
  Function \code{Lorentz()} returns a symmetric \eqn{4\times 4}{4*4}
  matrix; function \code{rot()} returns an orthogonal matrix
}
\details{
  Arguments \code{u,v} are coerced to three-velocities.

  A rotation-free Lorentz transformation is known as a \dfn{boost}.
  Function \code{boost(u)} returns \eqn{4\times 4}{4x4} matrix giving
  the Lorentz transform of an arbitrary three-velocity \code{u}.  Note
  that boosts can be successively applied with regular matrix
  multiplication.  Boost matrices are symmetrical, but do not commute in
  general.
  
  Composing two successive boosts does not return a pure boost matrix.  It
  is possible to decompose two successive pure boosts into a pure boost and
  a rotation.  This rotation may be expressed in terms of an orthogonal
  matrix, which is returned by function \code{rot()}.
  
  Neither function is vectorized.

}
\references{
  \itemize{
    \item
    Ungar 2006. \dQuote{Thomas precession: a kinematic
      effect...}. \emph{European Journal of Physics}, 27:L17-L20,
    \doi{doi:10.1088/0143-0807/27/3/L02}
    \item
    Sbitneva 2001. \dQuote{Nonassociative geomery of special relativity}.
    \emph{International Journal of Theoretical Physics, volume 40, number 1,
      pages 359--362}
  \item Wikipedia contributors 2018.  \dQuote{Wigner rotation},
  \emph{{Wikipedia}, The Free Encyclopedia}.
  \url{https://en.wikipedia.org/w/index.php?title=Wigner_rotation&oldid=838661305}.  Online; accessed 23 August 2018
}
}
\author{Robin K. S. Hankin}
\note{
Function \code{rot()} uses \code{crossprod()} for efficiency reasons
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
