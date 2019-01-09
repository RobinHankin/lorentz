\name{4mom}
\alias{4mom}
\alias{fourmom}
\alias{as.4mom}
\alias{fourmomentum}
\alias{four-momentum}
\alias{4momentum}
\alias{4-momentum}
\alias{as.4mom}
\alias{is.4mom}
\title{Four momentum}
\description{Create and test for four-momentum}
\usage{
as.4mom(x,E)
is.4mom(x)
}
\arguments{
  \item{x}{Object coerced to four-momentum}
  \item{E}{Scalar, energy}
}
\details{

Four-momentum is a relativistic generalization of three-momentum, with
the object's energy as the first element.

For the four-momentum of a photon, use \code{as.photon()}.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{boost}},\code{\link{as.photon}}}
\examples{

as.4mom(r3vel(5),E=3)

P <- as.4mom(r3vel(5),E=3)
P
reflect(P)
reflect(P,c(1,1,1))

}
