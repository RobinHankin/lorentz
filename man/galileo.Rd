\name{galileo}
\alias{galileo}
\alias{galilean}
\alias{Galileo}
\alias{Galilean}
\alias{classical}
\alias{newton}
\alias{Newton}
\alias{newtonian}
\alias{Newtonian}
\title{Classical mechanics; Newtonian approximation; infinite speed of light}
\description{
The Lorentz transforms reduce to their classical limit, the Galilean
transforms, if speeds are low compared with \eqn{c}.  Package idiom for
working in a classical framework is to use an infinite speed of light:
\code{sol(Inf)}.  Here I show examples of this.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{boost}}}
\examples{
sol(Inf)
boost(as.3vel(1:3))
as.3vel(1:3) + as.3vel(c(-1,4,5))     # classical velocity addition
rot(as.3vel(1:3),as.3vel(c(-4,5,2)))  # identity matrix


B <- boost(as.3vel(1:3))
orthog(B) \%*\% pureboost(B)  # should be B

sol(1)
}
