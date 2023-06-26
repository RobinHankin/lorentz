\name{reflect}
\alias{reflect}
\alias{mirror}
\alias{mirrors}
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
  Takes a four-momentum and returns the four-momentum after reflection.
  Will handle objects or photons.
  
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{photon}}}
\note{

  All four-momenta are measured in the rest frame of the mirror, but it
  is easy to reflect from moving mirrors; see examples.

  However, note that the \code{ref} argument is designed to work with
  photons only, where it is conceptually the percentage of photons
  reflected and not absorbed by the mirror.  If \code{ref} is less than
  unity, odd results are given for four momenta of nonzero restmass
  objects.
}

\examples{
## We will reflect some photons from an oblique mirror moving at half
## the speed of light.

## First create 'A', a bunch of photons all moving roughly along the x-axis:
A <- as.photon(as.3vel(cbind(0.9,runif(10)/1000,runif(10)/1000)))

## Now create 'm', a mirror oriented perpendicular to c(1,1,1):
m <- c(1,1,1)

## Reflect the photons in the mirror:
reflect(A,m)

## Reflect the photons in a series of mirrors:
A |> reflect(m) |> reflect(1:3) |> reflect(3:1) 


## To reflect from a moving mirror we need to transform to a frame in
## which the mirror is at rest, then transform back to the original
## frame.  First create B, a boost representing the mirror's movement
## along the x-axis at speed c/2:

B <- boost(as.3vel(c(0.5,0,0)))


## Transform to the mirror's rest frame:
A \%*\% t(B)    

## NB: in the above, take a transpose because the *rows* of A are 4-vectors.

## Then reflect the photons in the mirror:
reflect(A \%*\% t(B),m)


## Now transform back to the original rest frame (NB: active transform):
A |> tcrossprod(B) |> reflect(m) |> tcrossprod(solve(B))


}
