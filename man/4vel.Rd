\name{4vel}
\alias{4vel}
\alias{fourvel}
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
is.consistent.4vel(U, give=FALSE, TOL=1e-10)
inner4(U,V=U)
to3(U)
}
\arguments{
  \item{u}{A vector of three-velocities}
  \item{U,V}{A vector of four-velocities}
  \item{give}{In function \code{is.consistent.4vel()}, Boolean with
    \code{TRUE} meaning to return \eqn{U\cdot U+c^2}{U.U+c^2}, which is
    zero for a four-velocity, and default \code{FALSE} meaning to return
    whether the four-velocity is consistent to numerical precision}
  \item{TOL}{Small positive value used for tolerance}
}
\details{
  Function \code{as.4vel()} takes a three-velocity and returns a
  four-velocity.

  Given a four-vector \eqn{V}, function \code{inner4()} returns the
  Lorentz invariant \eqn{V^iV_i=\eta_{ij}V^iV^j}{V^i.V_i}.  This
  quantity is unchanged under Lorentz transforms.  Note that function
  \code{inner()} works for any four-vector, not just four-velocities.
  It will work for (eg) a four-displacement, a four-momentum vector or a
  four-frequency.  In electromagnetism, we could have a four-current or
  a four-potential.  If \eqn{U} is a four-velocity, then
  \eqn{U^iU_i=-1}; if \eqn{U} is a 4-displacement, then \eqn{U^iU_i} is
  the squared interval.  If \eqn{P} is the four-momentum of a photon
  then \eqn{P^iP_i=0}.

  Function \code{to3()} is a low-level helper function used when
  \code{as.3vel()} is given a four-velocity.

  Function \code{is.consistent.4vel()} checks for four-velocities being
  consistent in the sense that \eqn{U^iU_i=-1}{U.U=-1}.  Giving this
  function a vector as in \code{is.consistent.4vel(1:5)} will return an
  error.
  
  There is a class \dQuote{\code{4vel}}; but the emphasis is on
  three-velocities.  Compare the functions documented here with
  \code{boost()}, which returns a \eqn{4\times 4}{4*4} transformation
  matrix (which also includes rotation information).

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{boost}}}
\examples{

a <- r3vel(10)
as.4vel(a)     # a four-velocity

as.3vel(as.4vel(a))-a   # should be small

inner4(as.4vel(a))   # should be -1

stopifnot(all(is.consistent.4vel(as.4vel(a))))


## check Lorentz invariance of dot product:
U <- as.4vel(r3vel(10))
V <- as.4vel(r3vel(10))
B <- boost(as.3vel(1:3/10))
frame1dotprod <- inner4(U \%*\% B, V \%*\% B)
frame2dotprod <- inner4(U \%*\% B, V \%*\% B)

max(abs(frame1dotprod-frame2dotprod))  # should be small

}
