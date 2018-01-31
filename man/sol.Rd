\name{sol}
\alias{sol}
\alias{lightspeed}
\alias{celerity}
\title{Speed of light}
\description{
Getting and setting the speed of light}
\usage{
sol(c)
}
\arguments{
  \item{c}{Scalar, speed of light.  If missing, return the speed of light}
}
\details{
  In the context of an R package, the symbol \dQuote{c} presents
  particular problems.  In the \pkg{gyrogroup} package, the speed of
  light is denoted \dQuote{sol}, for  \sQuote{speed of light}.

  The speed of light is a global variable, governed by
  \code{options("c")}.  If \code{NULL}, define \code{c=1}.

  
}
\author{Robin K. S. Hankin}
\examples{

sol()

r3vel(5)

sol(299792458)
r3vel(5)

sol(1)


}