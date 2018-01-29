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
gam(u,SOL = getOption("c"))
gamm1(u,SOL = getOption("c"))
\method{gam}{3vel}(u,SOL = getOption("c"))
\method{gamm1}{3vel}(u,SOL = getOption("c"))
}
\arguments{
  \item{u}{Speed, either a vector of speeds or a vector of threevelocities}
  \item{SOL}{Speed of light, defaulting to 1}
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

gam(0.8)

gam(0.0004)
gamm1(0.0004)

gam(r3vel(6,0.7))

}