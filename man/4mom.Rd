\name{4mom}
\alias{Ops.4mom}
\alias{4mom}
\alias{fourmom}
\alias{as.4mom}
\alias{sum.4mom}
\alias{fourmomentum}
\alias{four-momentum}
\alias{4momentum}
\alias{4-momentum}
\alias{vel_to_4mom}
\alias{p_to_4mom}
\alias{as.4mom}
\alias{is.4mom}
\alias{fourmom_mult}
\alias{fourmom_add}
\title{Four momentum}
\description{Create and test for four-momentum}
\usage{
\method{Ops}{4mom}(e1, e2)
\method{sum}{4mom}(..., na.rm=FALSE)
vel_to_4mom(U,m=1)
p_to_4mom(p,E=1)
as.4mom(x)
is.4mom(x)
fourmom_mult(P,n)
fourmom_add(e1,e2)
}
\arguments{
  \item{x,P,e1,e2}{Four-momentum}
  \item{p}{Three-momentum}
  \item{E}{Scalar; energy}
  \item{U}{Object coerced to four-velocity}
  \item{m}{Scalar; rest mass}
  \item{n}{Multiplying factor}
  \item{...,na.rm}{Arguments sent to \code{sum()}}
}
\details{

Four-momentum is a relativistic generalization of three-momentum, with
the object's energy as the first element.  It can be defined as
\eqn{mU}, where \eqn{m} is the rest mass and \eqn{U}, the four-velocity
(use \code{vel_to_4mom()}), or \eqn{(E/c,p_x,p_y,p_z)}{(E/c,px,py,pz)}
where \eqn{E} is the energy and \eqn{(p_x,p_y,p_z)}{(px,py,pz)} the
three-momentum (\code{p_to_4mom()}).

The function \code{Ops.4mom()} passes unary and binary arithmetic
operators \dQuote{\code{+}}, \dQuote{\code{-}} and \dQuote{\code{*}} to
the appropriate specialist function.

The package is designed so that natural R idiom may be used for
physically meaningful operations such as combining momenta of different
objects, using the conservation of four-momentum.

For the four-momentum of a photon, use \code{as.photon()}.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{boost}},\code{\link{as.photon}}}
\examples{


o <- r4vel(5)

vel_to_4mom(o)         # five objects, random velocities, all unit mass
vel_to_4mom(o,   1:5)  # five objects with identical 4vel, masses 1..5
vel_to_4mom(o[1],1:5)  # five objects with random velocities, masses 1..5


p3 <- r3vel(5)
p_to_4mom(p3,E=1)
p_to_4mom(p3,E=10)   # slower
p_to_4mom(p3,E=100)  # even slower

P <- vel_to_4mom(as.3vel(c(0.8,0,0)) + r3vel(7,0.01))

reflect(P)
reflect(P,c(1,1,1))

sum(P)

}
