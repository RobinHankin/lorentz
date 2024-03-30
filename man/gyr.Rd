\name{gyr}
\alias{gyr}
\alias{gyr.a}
\alias{gyrfun}
\alias{gyrfun}
\title{Gyr function}
\description{
Relativistic addition of three velocities}
\usage{
gyr(u, v, x)
gyr.a(u, v, x)
gyrfun(u, v)
}
\arguments{
  \item{u,v,x}{Three-velocities, objects of class \code{3vel}}
}
\details{

  Function  \code{gyr(u,v,x)} returns the three-vector
  \eqn{\mathrm{gyr}[u,v]x}{gyr[u,v]x}.

  Function \code{gyrfun(u,v)} returns a function that returns a
  three-vector; see examples.

  The speed of light (1 by default) is not used directly by these
  functions; set it with \code{sol()}.

  
}
\references{
  \itemize{
    \item
Ungar 2006. \dQuote{Thomas precession: a kinematic
  effect of the algebra of Einstein's velocity addition law.  Comments
  on \sQuote{Deriving relativistic momentum and energy: {I}.
    {T}hree-dimensional case}}. {European Journal of Physics},
  27:L17-L20.
\item
Sbitneva 2001. \dQuote{Nonassociative geometry of special relativity}.
{International Journal of Theoretical Physics, volume 40, number 1,
  pages 359--362}}
}
\author{Robin K. S. Hankin}
\note{
  Function \code{gyr()} is slightly faster than \code{gyr.a()}, which is
  included for pedagogical reasons.

  Function \code{gyr()} is simply

  \code{add3(neg3(add3(u,v)),add3(u,add3(v,x)))}


  while function \code{gyr.a()} uses the slower but more transparent
  idiom

  \code{ -(u+v) + (u+(v+x)) }
}

\examples{


u <- r3vel(10)
v <- r3vel(10)
w <- r3vel(10)

x <- as.3vel(c(0.4,0.1,-0.5))
y <- as.3vel(c(0.1,0.2,-0.7))
z <- as.3vel(c(0.2,0.3,-0.1))


gyr(u,v,x)  # gyr[u,v]x

f <- gyrfun(u,v)
g <- gyrfun(v,u)

f(x)
f(r3vel(10))

f(g(x)) - x              # zero, by eqn 9
g(f(x)) - x              # zero, by eqn 9
(x+y) - f(y+x)           # zero by eqn 10
(u+(v+w)) - ((u+v)+f(w)) # zero by eqn 11


# Following taken from Sbitneva 2001:

rbind(x+(y+(x+z))  ,   (x+(y+x))+z)   # left Bol property
rbind((x+y)+(x+y)  ,   x+(y+(y+x)))   # left Bruck property


sol(299792458)   # speed of light in SI
as.3vel(c(1000,3000,1000)) + as.3vel(c(1000,3000,1000))
## should be close to Galilean result

sol(1)   # revert to default c=1

}
