\name{seq.3vel}
\alias{seq.3vel}
\title{seq method for three velocities}
\description{
  Simplified version of \code{seq()} for three-velocities.
}
\usage{
\method{seq}{3vel}(from, to, len, ...)
}
\arguments{
  \item{from,to}{Start and end of sequence}
  \item{len}{Length of vector returned}
  \item{...}{Further arguments (currently ignored)}
}
\details{

  \code{seq(a,b,n)} returns \code{a + t*(-b+a)} where \code{t} is
  numeric vector \code{seq(from=0,to=1,len=n)}.

  This definition is one of several plausible alternatives, but has the
  nice property that the first and last elements are exactly equal to
  the \code{a} and \code{b}  respectively.

  }
\author{Robin K. S. Hankin}
\examples{

a <- as.3vel(c(4,5,6)/9)
b <- as.3vel(c(-5,6,8)/14)
x <- seq(a,b,len=9)

x[1]-a # should be zero
x[9]-b # should be zero


jj <- a + seq(0,1,len=9)*(b-a)

jj-x   # decidedly non-zero


}

