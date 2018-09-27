\name{lorentz-package}
\alias{lorentz-package}
\alias{lorentz}
\alias{Lorentz}
\alias{gyrogroup}
\docType{package}
\title{
\packageTitle{lorentz}
}
\description{
\packageDescription{lorentz}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{lorentz}
\packageIndices{lorentz}

}
\author{
\packageAuthor{lorentz}

Maintainer: \packageMaintainer{lorentz}
}
\references{
Ungar 2006. \dQuote{Thomas precession: a kinematic
  effect...}. \emph{European Journal of Physics}, 27:L17-L20,
}
\keyword{ package }
\examples{


## Create some random three-velocities:

u <- r3vel(10)
v <- r3vel(10)
w <- r3vel(10)

u+v
v+u  ## Velocity addition is not commutative

u+(v+w)   
(u+v)+w   ## nor associative

}
