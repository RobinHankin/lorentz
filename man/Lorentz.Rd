\name{Lorentz}
\alias{Lorentz}
\title{Lorentz transforms}
\description{
Returns the Lorentz boost matrix as a function of three-velocity.
}
\usage{
Lorentz(v)
}
\arguments{
  \item{v}{A single three-velocity}
}
\value{
Returns a \eqn{4\times 4}{4*4} matrix.
}
\author{Robin K. S. Hankin}
\examples{
Lorentz(as.3vel(c(0.4,-0.2,0.1)))


u <- as.3vel(c(0.3,0.4,0))
v <- as.3vel(c(0.5,-0.3,0))
w <- as.3vel(c(0.7,-0.1,0))

Lu <- Lorentz(u)
Lv <- Lorentz(v)
Lw <- Lorentz(w)


Lorentz(u) - solve(Lorentz(-u))  # should be zero

Lu %*% Lv
Lorentz(u+v)  # not the same!
Lorentz(v+u)  # also not the same!

Lu %*% Lv %*% Lw  # associative, no brackets needed
Lorentz(u+(v+w))  # not the same!
Lorentz((u+v)+w)  # also not the same!




}
