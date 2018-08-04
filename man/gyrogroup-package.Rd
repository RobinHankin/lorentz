\name{gyrogroup-package}
\alias{gyrogroup-package}
\alias{gyrogroup}
\docType{package}
\title{
\packageTitle{gyrogroup}
}
\description{
\packageDescription{gyrogroup}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{gyrogroup}
\packageIndices{gyrogroup}

}
\author{
\packageAuthor{gyrogroup}

Maintainer: \packageMaintainer{gyrogroup}
}
\references{
Ungar 2006. \dQuote{Thomas precession: a kinematic
  effect...}. \emph{European Journal of Physics}, 27:L17-L20,
\doi{doi:10.1088/0143-0807/27/3/L02}
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
