\name{celerity}
\alias{celerity}
\alias{threecel}
\alias{rapidity}
\alias{celerity.3vel}
\alias{celerity.4vel}
\alias{celerity.default}
\alias{celerity_ur}
\alias{rapidity.3vel}
\alias{rapidity.4vel}
\alias{rapidity.default}
\alias{rapidity_ur}
\alias{as.3cel}
\alias{cel_to_vel}
\alias{vel_to_cel}
\alias{is.3cel}
\title{Celerity and rapidity}
\description{The celerity and rapidity of an object (experimental functionality)}
\usage{
\method{celerity}{3vel}(u)
\method{celerity}{4vel}(u)
celerity_ur(d)
\method{rapidity}{3vel}(u)
\method{rapidity}{4vel}(u)
rapidity_ur(d)
as.3cel(x)
cel_to_vel(x)
vel_to_cel(x)
}
\arguments{
  \item{u,x}{Speed: either a vector of speeds or a vector of
    three-velocities or four-velocities}
  \item{d}{In functions \code{celerity_ur()} and \code{rapidity_ur()},
      deficit of speed; speed of light minus speed of object}
}
\details{

  The \dfn{celerity} corresponding to speed \eqn{u} is defined as
  \eqn{u\gamma}{ug} and the \dfn{rapidity} is
  \eqn{c\cdot\mathrm{atanh}(u/c)}{omitted}.

  Functions \code{celerity_ur()} and \code{rapidity_ur()} are used for
  the ultrarelativistic case where speeds are very close to the speed of
  light.  Its argument \code{d} is the deficit, that is, \eqn{d=c-v}
  where \eqn{v} is the speed of the transformation.  Algebraically,
  \code{celerity_ur(c-v) == celerity(v)}, but if \eqn{d=1-v/c} is small
  the result of \code{celerity_ur()} is more accurate than that of
  \code{celerity()}.

  Things get a bit sticky for celerity and rapidity if \eqn{c\neq
  1}{c!=1}.  The guiding principle in the package is to give the
  celerity and rapidity the same units as \eqn{c}, so if \eqn{u\ll
  c}{u<<c} we have that all three of \code{celerity(u)},
  \code{rapidity(u)} and \code{u} are approximately equal.  Note
  carefully that, in contrast, \eqn{\gamma}{g} is dimensionless.  Also
  observe that \code{d} in functions \code{celerity_ur()} and
  \code{rapidity_ur()} has the same units as \eqn{c}.
  
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{gam}}}
\examples{

u <- 0.1  # c=1
c(u,celerity(u),rapidity(u))

omgp <- 4.9e-24  # speed deficit of the Oh-My-God particle
c(celerity_ur(omgp),rapidity_ur(omgp))


sol(299792458)                 # use SI units
u <- 3e7  # ~0.1c
c(u,celerity(u),rapidity(u))


snail <- 0.00275
c(snail,celerity(snail),rapidity(snail))


omgp <- omgp*sol() 
c(celerity_ur(omgp),rapidity_ur(omgp))


sol(1)

}

