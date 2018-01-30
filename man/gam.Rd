\name{gam}
\alias{gam}
\alias{gam.3vel}
\alias{gam.default}
\alias{gamm1}
\alias{gamm1.3vel}
\alias{gamm1.default}
\title{Gamma correction}
\description{
Lorentz gamma correction term in special relativity}
\usage{
gam(u)
gamm1(u)
\method{gam}{3vel}(u)
\method{gamm1}{3vel}(u)
}
\arguments{
  \item{u}{Speed, either a vector of speeds or a vector of threevelocities}
}
\details{
  Function \code{gam(u)} returns the Lorentz factor
  \deqn{\frac{1}{\sqrt{1-\mathbf{u}\cdot\mathbf{u}/c^2}}}{see PDF}

  Function \code{gamm1(u)} returns the Lorentz factor minus 1, useful
  for slow speeds when larger accuracy is needed (much like
  \code{expm1()}).
}
\author{Robin K. S. Hankin}
\examples{

gam(seq(from=0,by=0.1,len=10))
gam(r3vel(6,0.7))

gam(0.000001)-1
gamm1(0.000001)   # algebraically identical but much more accurate



options("c" = 299792458)

gamm1(343)  # speed of sound in SI

options("c" = NULL)   # return to default c=1


}