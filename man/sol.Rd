\name{sol}
\alias{sol}
\alias{eta}
\alias{minkowski}
\alias{lightspeed}
\alias{celerity}
\title{Speed of light and Minkowski metric}
\description{Getting and setting the speed of light}
\usage{
sol(c)
eta()
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

  Function \code{eta()} returns the Minkowski flat-space metric

  \deqn{\left(\begin{array}{cccc}
    -1&0&0&0\\
    0&1&0&0\\
    0&0&1&0\\
    0&0&0&1
    \end{array}
    \right)
  }{
    omitted}
  
}
\author{Robin K. S. Hankin}
\examples{

sol()

r3vel(5)

sol(299792458)
r3vel(5)
eta()

sol(1)


}