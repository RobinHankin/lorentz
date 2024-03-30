\name{sol}
\alias{sol}
\alias{eta}
\alias{minkowski}
\alias{lightspeed}
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

  In the context of an \R package, the symbol \dQuote{c} presents
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
  \deqn{\mathrm{diag}\left(-c^2,1,1,1\right).}{diag(-c^2,1,1,1).}

  Note that the top-left element of \code{eta()} is \eqn{-c^2}, not
  \eqn{-1}.

  Function \code{ptm()} returns a passive transformation matrix that
  converts displacement vectors to natural units
  (\code{to_natural=TRUE}) or from natural units
  (\code{to_natural=FALSE}).  Argument \code{change_time} specifies
  whether to change the unit of time (if \code{TRUE}) or the unit of
  length (if \code{FALSE}).

}
\author{Robin K. S. Hankin}
\note{
  Typing \dQuote{\code{sol(299792458)}} is a lot easier than typing
  \dQuote{\code{options("c"=299792458)}}, which is why the package uses
  the idiom that it does.

  In a R-devel discussion about options for printing, Martin Maechler
  makes the following observation: \dQuote{Good programming style
  for functions according to my book is to have them depend only on
  their arguments, and if a global option really (really? think twice!)
  should influence behavior, there should be arguments of the function
  which have a default determined by the global option}

  I think he is right in general, but offer the observation that the
  speed of light depends on the units chosen, and typically one fixes
  one's units once and for all, and does not subsequently change them.
  This would indicate (to me at least) that a global option would be
  appropriate.  Further, there \emph{is} a default, \eqn{c=1}, which is
  returned by \code{sol()} if the option is unset.  This is not just a
  \dQuote{default}, though: it is used in the overwhelming majority of
  cases.  Indeed, pedagogically speaking, one learning objective from
  the package is that units in which \eqn{c\neq 1} are difficult,
  awkward, and unnatural.  In the package \proglang{R} code, the
  \emph{only} place the speed of light option is accessed is via
  \code{sol()}.  Similar arguments are presented in the \pkg{clifford}
  package at \code{signature.Rd}.

  
}
\examples{

sol()                          # returns current speed of light
sol(299792458)                 # use SI units
sol()                          # speed of light now SI value

eta()                          # note [t,t] term
u <- as.3vel(c(100,200,300))   # fast terrestrial speed, but not relativistic
boost(u)                       # boost matrix practically Galilean
is.consistent.boost(boost(u))  # should be TRUE
sol(1)                         # revert to relativistic units

}
