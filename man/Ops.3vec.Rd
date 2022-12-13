\name{Ops.3vel}
\alias{Ops.3vel}
\alias{Ops.4vel}
\alias{Ops.gyr}
\alias{Ops}
\alias{massage3}
\alias{neg3}
\alias{prod3}
\alias{add3}
\alias{dot3}
\alias{equal3}
\title{Arithmetic Ops Group Methods for 3vel objects}
\description{Arithmetic operations for three-velocities}
\usage{
\method{Ops}{3vel}(e1, e2)
\method{Ops}{4vel}(e1, e2)
massage3(u,v)
neg3(u)
prod3(u,v=u)
add3(u,v)
dot3(v,r)
}
\arguments{
  \item{e1,e2,u,v}{Objects of class \dQuote{\code{3vel}},
    three-velocities}
  \item{r}{Scalar value for circle-dot multiplication}
}

\details{
  The function \code{Ops.3vel()} passes unary and binary arithmetic
  operators \dQuote{\code{+}}, \dQuote{\code{-}} and \dQuote{\code{*}}
  to the appropriate specialist function.

  The most interesting operators are \dQuote{\code{+}} and
  \dQuote{\code{*}}, which are passed to \code{add3()} and \code{dot3()}
  respectively.  These are defined, following Ungar, as:


  \deqn{
    \mathbf{u}+\mathbf{v} = \frac{1}{1+\mathbf{u}\cdot\mathbf{b}/c^2}
    \left\{
    \mathbf{u} +
    \frac{1}{\gamma_\mathbf{u}}\mathbf{v} +
    \frac{1}{c^2}\frac{\gamma_\mathbf{u}}{1+\gamma_\mathbf{u}}
    \left(\mathbf{u}\cdot\mathbf{v}\right)\mathbf{u}
    \right\}
  }{
    see PDF
  }

  and

  \deqn{
    r\odot\mathbf{v} =
    c\tanh\left(
    r\tanh^{-1}\frac{\left|\left|\mathbf{v}\right|\right|}{c}
    \right)\frac{\mathbf{v}}{\left|\left|\mathbf{v}\right|\right|}
  }{
    see PDF
  } 

  where \eqn{\mathbf{u}}{u} and \eqn{\mathbf{v}}{v} are three-vectors
  and \eqn{r} a scalar.  Function \code{dot3()} has special dispensation
  for zero velocity and does not treat \code{NA} entries entirely
  consistently.

  Arithmetic operations, executed via \code{Ops.4vel()}, are not defined
  on four-velocities.
  
  The package is designed so that natural \R idiom may be used for three
  velocity addition, see the examples section.
}
\value{
  Returns an object of class \code{3vel}, except for \code{prod3()}
  which returns a numeric vector.
}

\examples{
u <- as.3vel(c(-0.7, 0.1,-0.1))
v <- as.3vel(c( 0.1, 0.2, 0.3))
w <- as.3vel(c( 0.5, 0.2,-0.3))

x <- r3vel(10)   # random three velocities
y <- r3vel(10)   # random three velocities


u+v   # add3(u,v)
u-v   # add3(u,neg3(v))

-v    # neg3(v)

gyr(u,v,w)

## package is vectorized:


u+x
x+y

f <- gyrfun(u,v)
g <- gyrfun(v,u)

f(g(x)) - x    # should be zero by eqn10
g(f(x)) - x


(u+v) - f(v+u)                     # zero by eqn 10
(u+(v+w)) - ((u+v)+f(w))           # zero by eqn 11
((u+v)+w) - (u+(v+g(w)))           # zero by eqn 11


## NB, R idiom is unambiguous.  But always always ALWAYS use brackets.

## Ice report in lat 42.n to 41.25n Long 49w to long 50.30w saw much
## heavy pack ice and great number large icebergs also field
## ice.  Weather good clear

## -u+v == (-u) + v == neg3(u) + v == add3(neg3(u),v)

## u+v+w == (u+v)+w == add3(add3(u,v),w)

}
