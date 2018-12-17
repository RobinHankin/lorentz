\name{reflect}
\alias{reflect}
\alias{mirror}
\alias{reflection}
\title{Mirrors}
\description{Plane mirrors in special relativity}
\usage{
reflect(P,m,ref=1)
}
\arguments{
  \item{P}{Vector of four-momenta}
  \item{m}{Orientation of mirror, expressed as a three-vector}
  \item{ref}{Coefficient of reflectivity of the mirror}
}
\value{
  Returns the four-momentum after reflection
}
\author{Robin K. S. Hankin}
\note{
It is easy to reflect photons or other four-momenta of other objects
from moving mirrors; see examples.
}

\examples{
## We will reflect some photons from an oblique mirror moving at half
## the speed of light:


## A is a bunch of photons all moving roughly along the x-axis:
A <- as.photon(as.3vel(cbind(0.9,runif(10)/1000,runif(10)/1000)))

## m is a mirror oriented perpendicular to c(1,1,1):
m <- c(1,1,1)

## B is a boost representing the mirror's movement along the x-axis at speed c/2:
B <- boost(as.3vel(c(0.5,0,0)))


## first transform to the mirror's rest frame:
A \%*\% t(B)  

## Then reflect the photons in the mirror:
reflect(A \%*\% t(B),m)


## Now transform back to the original rest frame (NB: active transform):
reflect(A \%*\% t(B),m) \%*\% solve(t(B))
}
