\name{gam}
\alias{speed}
\alias{speed.3vel}
\alias{speed.4vel}
\alias{speedsquared}
\alias{gam}
\alias{gam.3vel}
\alias{gam.4vel}
\alias{gam.default}
\alias{gamm1}
\alias{gamm1.3vel}
\alias{gamm1.4vel}
\alias{gamm1.default}
\alias{gam_ur}
\title{Gamma correction}
\description{
Lorentz gamma correction term in special relativity}
\usage{
\method{speed}{3vel}(u)
\method{speed}{4vel}(u)
speedsquared(u)
gam(u)
gamm1(u)
\method{gam}{3vel}(u)
\method{gam}{4vel}(u)
\method{gamm1}{3vel}(u)
\method{gamm1}{4vel}(u)
gam_ur(d)
}
\arguments{
  \item{u}{Speed: either a vector of speeds or a vector of
    three-velocities or four-velocities}
  \item{d}{In function \code{gam_ur()}, deficit of speed; speed of light
      minus speed of object}
}
\details{

  Function \code{speed(u)} returns the speed of a \code{3vel} object or
  \code{4vel} object.
  
  Function \code{gam(u)} returns the Lorentz factor
  \deqn{\frac{1}{\sqrt{1-\mathbf{u}\cdot\mathbf{u}/c^2}}}{see PDF}

  Function \code{gamm1(u)} returns the Lorentz factor minus 1, useful
  for slow speeds when larger accuracy is needed (much like
  \code{expm1()}); to see the R idiom, type \dQuote{\code{gamm1.3vel}}
  at the commandline.  Function \code{gamm1()} is intended to work with
  \code{3vel} objects or speeds.  The function will take a 4-velocity,
  but this is not recommended as accuracy is lost (all it does is return
  the time component of the 4-velocity minus 1).

  Function \code{gam_ur()} is used for the ultrarelativistic case where
  speeds are very close to the speed of light (the function is named for
  \dQuote{gamma, ultrarelativistic}).
    

  
  Function \code{speedsquared(u)} returns the square of the speed of a
  \code{3vel} object.  Use this to avoid taking a needless square root.
}
\author{Robin K. S. Hankin}
\examples{

gam(seq(from=0,by=0.1,len=10))
gam(r3vel(6,0.7))


x <- as.3vel(c(0.1,0.4,0.5))
speed(x)

gam(speed(x))  # works, but slow and inaccurate
gam(x)         # recommended: avoids needless coercion



## Some work in SI units and terrestrial speeds.  Use gamm1() for this.
sol(299792458)
sound <- 343 # speed of sound in SI
gam(sound)
gam(sound)-1  
gamm1(sound)   # gamm1() gives much higher precision

snail <- as.3vel(c(0.00275,0,0)) # even the world's fastest snail...
gamm1(snail)                     # ...has only a small relativistic correction


## for the ultrarelativistic case of speeds very close to the speed of
##   light, use gam_ur():

sol(1)           # revert to relativistic units
omgp <- 4.9e-24  # speed deficit of the Oh-My-God particle
gam(1-omgp)      # numeric overflow
gam_ur(omgp)     # large but finite

}
