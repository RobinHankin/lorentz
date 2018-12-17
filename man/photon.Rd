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
\author{Robin K. S. Hankin}
\seealso{\code{\link{4mom}},\code{\link{reflect}}}
\examples{

as.photon(as.3vel(cbind(0.9,runif(10)/1000,runif(10)/1000)))

}
