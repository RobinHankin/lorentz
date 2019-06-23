\name{sol}
\alias{sol}
\alias{eta}
\alias{minkowski}
\alias{lightspeed}
\alias{celerity}
\alias{ptm}
\title{Speed of light and Minkowski metric}
\description{Getting and setting the speed of light}
\usage{
sol(c)
eta(downstairs=TRUE)
ptm(to_natural=TRUE, change_time=TRUE)
}
\arguments{
  \item{c}{Scalar, speed of light.  If missing, return the speed of
    light}
  \item{downstairs}{Boolean, with default \code{TRUE} meaning to return
    the covariant metric tensor \eqn{g_{ij}}{g_ij} with two downstairs
      indices, and \code{FALSE} meaning to return the contravariant
      version \eqn{g^{ij}}{g^ij} with two upstairs indices}
    \item{to_natural,change_time}{Boolean, specifying the
      nature of the passive transform matrix}
}
\details{

  In the context of an R package, the symbol \dQuote{c} presents
  particular problems.  In the \pkg{lorentz} package, the speed of light
  is denoted \dQuote{sol}, for \sQuote{speed of light}.  You can set the
  speed of light with \code{sol(x)}, and query it with \code{sol()}; see
  the examples.  An infinite speed of light is sometimes useful for
  Galilean transforms.

  The speed of light is a global variable, governed by
  \code{options("c")}.  If \code{NULL}, define \code{c=1}.  Setting
  \code{showSOL} to \code{TRUE} makes \code{sol()} change the prompt to
  display the speed of light which might be useful.

  Function \code{eta()} returns the Minkowski flat-space metric

  \deqn{\left(\begin{array}{cccc}
    -c^2& 0 & 0 & 0\\
      0 & 1 & 0 & 0\\
      0 & 0 & 1 & 0\\
      0 & 0 & 0 & 1
    \end{array}
    \right)
  }{
    omitted}

Note that the top-left element of \code{eta()} is \eqn{-c^2}, not
\eqn{-1}.

Function \code{ptm()} returns a passive transformation matrix that
converts displacement vectors to natural units (\code{to_natural=TRUE})
or from natural units (\code{to_natural=FALSE}).  Argument
\code{change_time} specifies whether to change the unit of time (if
\code{TRUE}) or the unit of length (if \code{FALSE}).

}
\author{Robin K. S. Hankin}
\note{
  Typing \dQuote{\code{sol(299792458)}} is a lot easier than typing
  \dQuote{\code{options("c"=299792458)}}, which is why the package uses
  the idiom that it does.
}
\examples{

sol()                          # returns current speed of light
sol(299792458)                 # use SI units
sol()                          # speed of light now SI value

eta()                          # note [t,t] term
u <- as.3vel(c(100,200,300))   # fast terrestrial speed, but not relativistic
boost(u)                       # boost matrix practically Galilean
is.consistent.boost(boost(u))  # should be TRUE
sol(1)                         # revert to relativisitic units

}
