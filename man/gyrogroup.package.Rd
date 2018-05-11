\name{gyrogroup-package}
\alias{gyrogroup}
\alias{gyro}
\alias{gyrogroup-package}
\docType{package}
\title{The gyrogroup package}
\description{Three-velocity composition in special relativity}
\details{

Package \code{gyrogroup} uses the \code{3vel} class of objects to
represent three-velocities.  Various methods are provided to compose and
manipulate them.


}
\author{
  Robin K. S. Hankin

  Maintainer: <hankin.robin@gmail.com>
}
\seealso{\code{\code{\link{gyr}},\code{\link{Ops.3vec}}}
\keyword{math}
\examples{

x <- r3vel(10)   # random three velocities
y <- r3vel(10)   # random three velocities
z <- r3vel(10)   # random three velocities


x+y
4*(x+y)
4*x + 4*y


(x+(y+z)) - ((x+y)+z)  # not equal in general

}
