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
\alias{decompose}
\alias{pureboost}
\alias{orthog}
\alias{is.consistent.boost}
\title{Lorentz transformations}
\description{Lorenz transformations: boosts and rotations}
\usage{
boost(u)
rot(u,v,space=TRUE)
is.consistent.boost(L, give=FALSE, TOL=1e-10)
pureboost(L)
orthog(L)
}
\arguments{
  \item{u,v}{Three-velocities, coerced to  class \code{3vel}}
  \item{L}{Lorentz transform expressed as a \eqn{4\times 4}{4x4} matrix}
  \item{TOL}{Numerical tolerance}
  \item{give}{Boolean with \code{TRUE} meaning to return the transformed
    metric tensor (which should be the flat-space \code{eta()}; qv) and
    default \code{FALSE} meaning to return whether the matrix is a
    consistent boost or not}
  \item{space}{Boolean, with default \code{TRUE} meaning to return just
    the spatial component of the rotation matrix and \code{FALSE}
    meaning to return the full \eqn{4\times 4}{4x4} matrix transformation}
}
\value{
  Function \code{boost()} returns a symmetric \eqn{4\times 4}{4*4}
  matrix; function \code{rot()} returns an orthogonal matrix.
}
\details{
  Arguments \code{u,v} are coerced to three-velocities.

  A rotation-free Lorentz transformation is known as a \dfn{boost}
  (sometimes a \dfn{pure boost}), and when expressed in matrix form is
  symmetric.  Function \code{boost(u)} returns a \eqn{4\times 4}{4x4}
  matrix giving the Lorentz transform of an arbitrary three-velocity
  \code{u}.  Pure boost matrices are symmetrical, but do not commute in
  general.

  Boosts can be successively applied with regular matrix multiplication.
  However, composing two successive pure boosts does not return a pure
  boost matrix: the product is not symmetric in general.  The resulting
  matrix represents a \dfn{Lorentz transform}.

  It is possible to decompose a Lorentz transform \eqn{L} into a pure
  boost and a spatial rotation.  Thus \eqn{L=OP} where \eqn{O} is an
  orthogonal matrix and \eqn{P} a pure boost matrix; these are returned by
  functions \code{orthog()} and \code{pureboost()} respectively.  If the 
  speed of light is not equal to 1, the functions still work but can be
  confusing.
  
  The composition of two pure Lorentz boosts is not necessarily pure.  If
  we have two successive boosts corresponding to \eqn{u} and \eqn{v}, then
  the composed boost may be decomposed into a pure boost of \code{boost(u+v)}
  and a rotation of \code{rot(u,v)}.
  
}
\references{
  \itemize{
    \item
    Ungar 2006. \dQuote{Thomas precession: a kinematic
      effect\ldots}. \emph{European Journal of Physics}, 27:L17-L20
    \item
    Sbitneva 2001. \dQuote{Nonassociative geometry of special relativity}.
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

u <- r3vel(1)
v <- r3vel(1)
w <- r3vel(1)

boost(u) - solve(boost(-u))  # should be zero

boost(u) \%*\% boost(v)   # not a pure boost (not symmetrical)
boost(u+v)  # not the same!
boost(v+u)  # also not the same!

u+v  # returns a three-velocity


boost(u) \%*\% boost(v) \%*\% boost(w)  # associative, no brackets needed
boost(u+(v+w))  # not the same!
boost((u+v)+w)  # also not the same!


rot(u,v)
rot(v,u)    # transpose (=inverse) of rot(u,v)


rot(u,v,FALSE) \%*\% boost(v) \%*\% boost(u)
boost(u+v)     # should be the same.


orthog(boost(u) \%*\% boost(v)) - rot(u,v,FALSE)  # should be small
pureboost(boost(v) \%*\% boost(u)) - boost(u+v)   # should be small


## Define a random-ish Lorentz transform
L <- boost(r3vel(1)) \%*\% boost(r3vel(1)) \%*\% boost(r3vel(1))

## check it:


\dontrun{   # needs emulator package
quad.form(eta(),L)  # should be eta()
}

## More concisely:
is.consistent.boost(L)     # should be TRUE

## Decompose L into a rotation and a pure boost:
U <- orthog(L)
P <- pureboost(L)

L - U \%*\% P              # should be small (L = UP)
crossprod(U)               # should be identity (U is orthogonal)
P - t(P)                   # should be small (P is symmetric)

## First row of P should be a consistent 4-velocity:
is.consistent.4vel(as.4vel(P[,1]))



}
