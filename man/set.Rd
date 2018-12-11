\name{transform}
\alias{transform}
\alias{transform_dd}
\alias{transform_ud}
\alias{transform_uu}
\alias{perfectfluid}
\alias{dust}
\alias{stress-energy}
\alias{stress-energy-tensor}
\alias{energy-momentum}
\alias{energy-momentum-tensor}
\title{The energy-momentum tensor}
\description{Various functionality to deal with the stress-energy tensor
  in special relativity}
\usage{
perfectfluid(rho,p,u=0)
dust(rho,u=0)
transform_dd(TT, B)
transform_ud(TT, B)
transform_uu(TT, B)
}

\arguments{
  \item{TT}{A second-rank tensor with indices either
  downstairs-downstairs, downstairs-upstairs, or upstairs-upstairs}
\item{B}{A boost matrix}
\item{rho,p,u}{Density,pressure, and four-velocity of the dust}
}
\details{

  Function \code{perfectfluid()} returns the stress-energy tensor, with
  two upstairs indices, for a perfect fluid with the conditions
  specified.  No checking for physical reasonableness (eg the weak
  energy condition) is performed: caveat emptor!

  Function \code{dust()} is a (trivial) function that returns the
  stress-energy tensor for a zero-pressure perfect fluid, that is,
  dust.  It is here for discoverability reasons.

  Functions \code{transform_dd()} et seq transform a second-rank tensor
  using the Lorentz transform.  The letters \dQuote{u} or \dQuote{d}
  denote the indices of the tensor being upstairs (contravariant) or
  downstairs (covariant).  The stress-energy tensor is usually written
  with two upstairs indices, so use \code{transform_uu()} to transform
  it.

}
\author{Robin K. S. Hankin}
\examples{

perfectfluid(10,1)

u <- as.3vel(c(0.4,0.4,0.2))

## In the following, LHS is stationary dust and RHS is dust moving at
## velocity 'u', but transformed to a frame also moving at velocity 'u':

LHS <- dust(1)
RHS <- transform_uu(dust(1,u),boost(u))
max(abs(LHS-RHS))  # should be small


## In the following, negative sign needed because active/passive
## difference:

LHS <- dust(1,u)
RHS <- transform_uu(dust(1),boost(-u))
max(abs(LHS-RHS))  # should be small



sol(299792458)
perfectfluid(1.225,101325) # air at STP


LHS <- transform_uu(perfectfluid(1.225,101325),boost(as.3vel(c(1000,0,0))))
RHS <- perfectfluid(1.225,101325) 
LHS-RHS

sol(1)
}
