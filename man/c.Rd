\name{c.3vel}
\alias{c.3vel}
\title{Combine vectors of three-velocities into a single vector}
\description{
Combines its arguments recursively to form a vector of three velocities
}
\usage{
\method{c}{3vel}(...)
}
\arguments{
  \item{\dots}{Vectors of three-velocities}
}
\details{

  Returns a vector of three-velocities.  Names are inherited from the
  behaviour of \code{cbind()}, not \code{c()}.
}
\note{
  This function is used extensively in
  \code{inst/distributive_search.R}.
}

  
\author{Robin K. S. Hankin}
\examples{

c(r3vel(3),r3vel(6,0.99))


}
\keyword{array}