\name{comm_fail}
\alias{comm_fail}
\alias{comm_fail1}
\alias{comm_fail2}
\alias{ass_fail}
\alias{ass_fail}
\alias{my_seg}
\title{
Failure of commutativity and associativty using visual plots
}
\description{
Relativistic addition of three-velocities is neither commutative nor
associative, and the functions documented here show this visually.
}
\usage{
comm_fail1(u, v, bold=5, r=1)
comm_fail2(u, v, bold=5, r=1)
ass_fail(u, v, w, bold=5,r=1)
my_seg(u,start=as.3vel(0), bold=5, ...)
}
\arguments{
  \item{u,v,w,start}{Three velocities.  Arguments \code{u} and \code{w} are
    single-element three velocities, argument \code{v} is a vector.  See
    the examples}
  \item{bold}{Integer specifying which vector element to be drawn in
    bold}
  \item{r}{Radius of dotted green circle, defaulting to 1 (corresponding
    to \eqn{c=1}).  Use \code{NA} to suppress plotting of circle}
  \item{...}{Further arguments, passed to \code{arrows()}}
  }
\value{
These functions are called for their side-effect of plotting a diagram.
}
\note{The vignette \code{lorentz} gives more details and
  interpretation of the diagrams.

  Function \code{my_seg()} is an internal helper function.
}
\author{Robin K. S. Hankin}
\examples{

u <- as.3vel(c(0.4,0,0))
v <- seq(as.3vel(c(0.4,-0.2,0)), as.3vel(c(-0.3,0.9,0)),len=20)
w <- as.3vel(c(0.8,-0.4,0))

comm_fail1(u=u, v=v)
comm_fail2(u=u, v=v)
  ass_fail(u=u, v=v, w=w, bold=10)

}
