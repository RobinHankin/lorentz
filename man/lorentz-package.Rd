\name{lorentz-package}
\alias{lorentz-package}
\alias{lorentz}
\alias{Lorentz}
\alias{gyrogroup}
\docType{package}
\title{
\packageTitle{lorentz}
}
\description{
\packageDescription{lorentz}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{lorentz}
\packageIndices{lorentz}

}
\author{
\packageAuthor{lorentz}

Maintainer: \packageMaintainer{lorentz}
}
\references{
  \itemize{
\item Ungar 2006. \dQuote{Thomas precession: a kinematic
  effect...}. \emph{European Journal of Physics}, 27:L17-L20.
\item
\url{https://www.youtube.com/watch?v=9Y9CxiukURw&index=68&list=PL9_n3Tqzq9iWtgD8POJFdnVUCZ_zw6OiB}
}
}
\keyword{ package }
\examples{

u <- as.3vel(c(0.3,0.6,-0.1))  # u is a three-velocity
gam(u)                         # relativistic gamma term for u
U <- as.4vel(u)                # U is a four-velocity
B1 <- boost(u)                 # B1 is the Lorentz transform matrix for u
B1 \%*\% c(1,0,0,0)              # Lorentz transform of zero 4-velocity (=-u)

B2 <- boost(as.3vel(c(-0.1,0.8,0.3)))  
B3 <- boost(as.3vel(c(-0.1,0.1,0.9)))  # more boosts

Bi <- B1 \%*\% B2  # Bi is the boost for successive Lorentz transforms


pureboost(Bi)      # Decompose Bi into a pure boost...
orthog(Bi)         # and an orthogonal matrix

Bj <- B2 \%*\% B1    # B1 and B2 do not commute...

(B1 \%*\% B2) \%*\% B3 
B1 \%*\% (B2 \%*\% B3)    # ...but composition *is* associative



## Three velocities and the gyrogroup

## Create some random three-velocities:

u <- r3vel(10)
v <- r3vel(10)
w <- r3vel(10)

u+v
v+u        # Three-velocity addition is not commutative...

u+(v+w)   # ... nor associative
(u+v)+w 

}
