\name{c.3vel}
\alias{c.3vel}
\alias{c.3cel}
\alias{c.4vel}
\title{Combine vectors of three-velocities and four-velocities into a single vector}
\description{
Combines its arguments recursively to form a vector of three velocities
or four velocities
}
\usage{
\method{c}{3vel}(...)
\method{c}{3cel}(...)
\method{c}{4vel}(...)
}
\arguments{
  \item{\dots}{Vectors of three-velocities}
}
\details{

  Returns a vector of three-velocities or four-velocities.  These are
  stored as three- or four- column matrices; each row is a velocity.

 Names are inherited from the behaviour of \code{cbind()}, not
  \code{c()}.

}
\note{

  This function is used extensively in
  \code{inst/distributive_search.R}.

  For \dQuote{c} as in celerity or speed of light, see \code{sol()}.
}
  
\author{Robin K. S. Hankin}
\seealso{\code{\link{sol}}}
\examples{

c(r3vel(3),r3vel(6,0.99))


}
\keyword{array}
