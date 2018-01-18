\name{r3vel}
\alias{r3vel}
\title{Random three-velocities}
\description{
Generates random three-velocities, optionally specifiying a magnitude
}
\usage{
r3vel(n, r = NA)
}
\arguments{
  \item{n}{Number of three-velocities to generate}
  \item{r}{Absolute value of the three-velocities, with default
    \code{NA} meaning to sample uniformly from the unit ball}
}
\details{
It is not entirely trivial to sample \emph{uniformly} from the unit ball
or unit sphere, but it is not hard either.
}
\value{
Returns a vector of three velocities.
}
\author{Robin K. S. Hankin}
\examples{

a <- r3vel(10000)
b <- r3vel(1000,0.8)
u <- as.3vel(c(0,0,0.9))

pairs(a,asp=1)
pairs(b,asp=1)

pairs(u+a,asp=1)
pairs(a+u,asp=1)

}