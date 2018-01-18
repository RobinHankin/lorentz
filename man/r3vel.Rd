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
u <- r3vel(10)
v <- r3vel(10)

u+v


u + as.3vel(c(0.6,0.6,0.1))

}