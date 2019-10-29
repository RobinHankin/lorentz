\name{r3vel}
\alias{r3vel}
\alias{r4vel}
\alias{rboost}
\title{Random relativstic velocities}
\description{
Generates random three-velocities or four-velocities, optionally specifiying a magnitude
}
\usage{
r3vel(n, r = NA)
r4vel(...)
rboost(r = NA)
}
\arguments{
  \item{n}{Number of three- or four- velocities to generate}
  \item{r}{Absolute value of the three-velocities, with default
    \code{NA} meaning to sample uniformly from the unit ball}
  \item{...}{Arguments passed to \code{r3vel()}}
}
\details{
  Function \code{r3vel()} returns a random three-velocity.  Function
  \code{r4vel()} is a convenience wrapper for \code{as.4vel(r3vel())}.

  Function \code{rboost()} returns a random \eqn{4\times 4}{4*4} Lorentz
  boost matrix, drawn from the connected component.  If given
  \code{r=0}, then a transform corresponding to a random rotation will
  be returned.
}
\value{
Returns a vector of three- or four- velocities.
}
\author{Robin K. S. Hankin}
\note{

  If the speed of light is infinite, these functions require a specified
  argument for \code{r}.

  It is not entirely trivial to sample \emph{uniformly} from the unit ball
  or unit sphere, but it is not hard either.
}
\examples{

a <- r3vel(10000)
b <- r3vel(1000,0.8)
u <- as.3vel(c(0,0,0.9))

pairs(unclass(u+a),asp=1)
pairs(unclass(a+u),asp=1)

is.consistent.boost(rboost())

sol(299792458)    # switch to SI units
sound <- 343      # speed of sound in metres per second
r3vel(100,343)    # random 3-velocities with speed = 343 m/s

sol(1)   # return to default c=1

}
