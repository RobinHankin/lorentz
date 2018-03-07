\name{comm_fail}
\alias{comm_fail}
\alias{comm_fail1}
\alias{comm_fail2}
\alias{ass_fail}
\alias{ass_fail}
\title{
Functions show failure of commutativity and associativty using visual plots
}
\description{
Relativistic addition of three-velocities is neither commutative nor
associative, and the functions documented here show this visually.
}
\usage{
comm_fail1(u, v)
comm_fail2(u, v)
ass_fail(u, v, w, bold)
}
\arguments{
  \item{u,v,w}{Three velocities.  Arguments \code{u} and \code{w} are
    single-element three velocities, argument \code{v} is a vector.  See
    the examples}
  \item{bold}{Integer specifying which vector element to be drawn in
    bold}
}
\value{
These functions are called for their side-effect of plotting a diagram.
}
\note{The vignette \code{commutative_associative} gives more details and
  interpretation of the diagrams}
\author{
Robin K. S. Hankin
}
\examples{

jj <- as.3vel(c(0.4,-0.2,0))
kk <- as.3vel(c(-0.4,0.6,0))
kk1 <- as.3vel(c(-0.4,0.3,0))

u <- as.3vel(c(0.5,0,0))
v <- jj + kk*seq(from=0,to=3,len=28)
w <- as.3vel(c(0.8,-0.4,0))

comm_fail1(u=u, v=v)
comm_fail2(u=u, v=v)

ass_fail(
    u=u, 
    v = jj + kk1 * seq(from=0,to=3,len=28),
    w = w, bold=23
    )

}
