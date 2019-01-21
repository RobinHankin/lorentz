\name{photon}
\alias{photon}
\alias{as.photon}
\alias{light}
\alias{nullvec}
\alias{nullvector}
\alias{null vector}
\alias{is.consistent.nullvec}
\title{Photons}
\description{Various functionality to deal with the 4-momentum of a photon}
\usage{
is.consistent.nullvec(N,TOL=1e-10)
as.photon(x,E=1)
}
\arguments{
  \item{N}{Four-momentum to be tested for nullness}
  \item{TOL}{tolerance}
  \item{x}{Vector of three-velocities}
  \item{E}{Energy, a scalar}
}
\details{
  Returns the four-momentum of a photon.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{4mom}},\code{\link{reflect}}}
\examples{

## A bunch of photons all approximately parallel to the x-axis:
as.photon(as.3vel(cbind(0.9,runif(10)/1000,runif(10)/1000)))


## mirror ball:
disco <- matrix(rnorm(30),10,3) \%>\% sweep(1, sqrt(rowSums(.^2)),`/`)
p <- as.photon(c(1,0,0))
reflect(p,disco)

table(reflect(p,disco)[,2]>0) # should be TRUE with probability sqrt(0.5)

## relativistic disco; mirror ball moves at 0.5c:

B <- boost(as.3vel(c(0.5,0,0)))
p \%>\% tcrossprod(B) \%>\% reflect(disco) \%>\% tcrossprod(solve(B))

}

