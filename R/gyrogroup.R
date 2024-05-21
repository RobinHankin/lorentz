#' Speed of light and Minkowski metric
#' 
#' Getting and setting the speed of light
#' 
#' 
#' In the context of an package, the symbol \dQuote{c} presents particular
#' problems.  In the \pkg{lorentz} package, the speed of light is denoted
#' \dQuote{sol}, for \sQuote{speed of light}.  You can set the speed of light
#' with \code{sol(x)}, and query it with \code{sol()}; see the examples.  An
#' infinite speed of light is sometimes useful for Galilean transforms.
#' 
#' The speed of light is a global variable, governed by \code{options("c")}.
#' If \code{NULL}, define \code{c=1}.  Setting \code{showSOL} to \code{TRUE}
#' makes \code{sol()} change the prompt to display the speed of light which
#' might be useful.
#' 
#' Function \code{eta()} returns the Minkowski flat-space metric
#' \deqn{\mathrm{diag}\left(-c^2,1,1,1\right).}{diag(-c^2,1,1,1).}
#' 
#' Note that the top-left element of \code{eta()} is \eqn{-c^2}, not \eqn{-1}.
#' 
#' Function \code{ptm()} returns a passive transformation matrix that converts
#' displacement vectors to natural units (\code{to_natural=TRUE}) or from
#' natural units (\code{to_natural=FALSE}).  Argument \code{change_time}
#' specifies whether to change the unit of time (if \code{TRUE}) or the unit of
#' length (if \code{FALSE}).
#' 
#' @aliases sol eta minkowski lightspeed ptm
#' @param c Scalar, speed of light.  If missing, return the speed of light
#' @param downstairs Boolean, with default \code{TRUE} meaning to return the
#' covariant metric tensor \eqn{g_{ij}}{g_ij} with two downstairs indices, and
#' \code{FALSE} meaning to return the contravariant version \eqn{g^{ij}}{g^ij}
#' with two upstairs indices
#' @param to_natural,change_time Boolean, specifying the nature of the passive
#' transform matrix
#' @note Typing \dQuote{\code{sol(299792458)}} is a lot easier than typing
#' \dQuote{\code{options("c"=299792458)}}, which is why the package uses the
#' idiom that it does.
#' 
#' In a R-devel discussion about options for printing, Martin Maechler makes
#' the following observation: \dQuote{Good programming style for functions
#' according to my book is to have them depend only on their arguments, and if
#' a global option really (really? think twice!)  should influence behavior,
#' there should be arguments of the function which have a default determined by
#' the global option}
#' 
#' I think he is right in general, but offer the observation that the speed of
#' light depends on the units chosen, and typically one fixes one's units once
#' and for all, and does not subsequently change them.  This would indicate (to
#' me at least) that a global option would be appropriate.  Further, there is a
#' default, \eqn{c=1}, which is returned by \code{sol()} if the option is
#' unset.  This is not just a \dQuote{default}, though: it is used in the
#' overwhelming majority of cases.  Indeed, pedagogically speaking, one
#' learning objective from the package is that units in which \eqn{c\neq 1} are
#' difficult, awkward, and unnatural.  In the package
#' c("\\ifelse{latex}{\\out{\\textsf{#1}}}{#1}",
#' "R")\ifelse{latex\out{\textsf{R}}R} code, the only place the speed of light
#' option is accessed is via \code{sol()}.  Similar arguments are presented in
#' the \pkg{clifford} package at \code{signature.Rd}.
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' sol()                          # returns current speed of light
#' sol(299792458)                 # use SI units
#' sol()                          # speed of light now SI value
#' 
#' eta()                          # note [t,t] term
#' u <- as.3vel(c(100,200,300))   # fast terrestrial speed, but not relativistic
#' boost(u)                       # boost matrix practically Galilean
#' is.consistent.boost(boost(u))  # should be TRUE
#' sol(1)                         # revert to relativistic units
#' 
#' 
#' @export sol
`sol` <- function(c){
  if(missing(c)){  # return SOL
    jj <- getOption("c")
    if(!is.null(jj)){
      return(jj)
    } else {
      return(1)
    }
  } else { # set SOL
    options("c" = c)
    if(isTRUE(getOption("showSOL"))){
        options("prompt" = paste(c, "> "))
    }
    return(c)
  }
}



#' Coordinate names for relativity
#' 
#' 
#' Trivial function to set coordinate names to \dQuote{\code{t}},
#' \dQuote{\code{x}}, \dQuote{\code{y}}, \dQuote{\code{z}}.
#' 
#' Function \code{coordnames()} simply returns the character string
#' \code{c("t","x","y","z")}.  It may be overwritten.  Function \code{flob()}
#' sets the row and columnnames of a \eqn{4\times }{4*4}\eqn{ 4}{4*4} matrix to
#' \code{coordnames()}.
#' 
#' @aliases coordnames flob
#' @param \dots Further arguments, currently ignored
#' @param x A matrix
#' @note If anyone can think of a better name than \code{flob()} let me know.
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' coordnames()
#' 
#' flob(diag(3))
#' flob(matrix(1,4,4))
#' 
#' ## You can change the names if you wish:
#' coordnames <- function(x){letters[1:4]}
#' flob(outer(1:4,1:4))
#' 
#' 
#' @export coordnames
`coordnames` <- function(...){c("t","x","y","z")}

`flob` <- function(x){
  if(all(dim(x)==c(4,4))){
    jj <- coordnames()
  } else if(all(dim(x)==c(3,3))){
    jj <- coordnames()[-1]
  } else {
    stop("matrix must be either 3x3 or 4x4")
  }
  rownames(x) <- jj
  colnames(x) <- jj
  return(x)
}

`eta` <- function(downstairs=TRUE){
  if(downstairs){
    return(diag(c(-sol()^2,1,1,1)))
    } else {
      return(diag(c(-1/sol()^2,1,1,1)))
    }
}

`as.3vel` <- function(x){
  if(is.3cel(x)){return(cel_to_vel(x))}    
  x <- unclass(x)
  if(length(x)==1){
    if(x==0){
      x <- c(0,0,0)
    } else {
      stop("not defined")
    }
  }

  if(is.vector(x)){x <- t(x)}
  if(ncol(x) == 4){   # assumed to be a 4-velocity
    out <- to3(as.4vel(x))
  } else if(ncol(x)==3){
    if(all(rowSums(x^2)<sol()^2)){
      out <- x
    } else {
      stop("speed > c")
    }
  } else {
    stop("should have 3 or 4 columns")
  }
  class(out) <- c("3vel","vec") # this is the only place where the class is set
  return(out)
}


`as.3cel` <- function(x){
  if(is.3vel(x)){return(vel_to_cel(x))}
  if(is.vector(x)){x <- t(x)}
  if(ncol(x) == 4){   # assumed to be a 4-velocity
    out <- x[,-1]  # lose the first column
  } else if(ncol(x)==3){
    out <- x
  } else {
    stop("should have 3 columns")
  }
  class(out) <- c("3cel","vec") # this is the only place where the class is set
  return(out)
}

`3vel` <- function(n){ as.3vel(matrix(0,n,3))  }
`4vel` <- function(n){ as.4vel(matrix(0,n,3))  }
`3cel` <- function(n){ as.3cel(matrix(0,n,3))  }



#' Three velocities
#' 
#' Create and test for three-velocities, \code{3vel} objects.
#' 
#' 
#' @aliases 3vel as.3vel is.3vel length.vec names.vec names<-.vec threevel
#' 3velocity 3-velocity threevelocity three-velocity
#' @param n In function \code{3vel()}, number of three velocities to create
#' @param x,value Vectors of three-velocities
#' @note
#' 
#' Class \code{vel} is a virtual class containing classes \code{3vel} and
#' \code{4vel}.
#' 
#' Function \code{threevel()} is a convenience wrapper for \code{3vel()}.
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' U <- r4vel(7)
#' as.4vel(as.3vel(U)) # equal to U, to numerical precision
#' 
#' x <- as.3vel(1:3/4)
#' u <- as.3vel(matrix(runif(30)/10,ncol=3))
#' 
#' names(u) <- letters[1:10]
#' 
#' x+u
#' u+x  # not equal
#' 
#' 
#' 
#' 
#' @export threevel
`threevel` <- `3vel`


#' Four velocities
#' 
#' Create and test for four-velocities.
#' 
#' Function \code{as.4vel()} takes a three-velocity and returns a
#' four-velocity.
#' 
#' Given a four-vector \eqn{V}, function \code{inner4()} returns the Lorentz
#' invariant \eqn{V^iV_i=\eta_{ij}V^iV^j}{V^i.V_i}.  This quantity is unchanged
#' under Lorentz transforms.  Note that function \code{inner4()} works for any
#' four-vector, not just four-velocities.  It will work for (eg) a
#' four-displacement, a four-momentum vector or a four-frequency.  In
#' electromagnetism, we could have a four-current or a four-potential.  If
#' \eqn{U} is a four-velocity, then \eqn{U^iU_i=-c^2}; if \eqn{U} is a
#' 4-displacement, then \eqn{U^iU_i} is the squared interval.  If \eqn{P} is
#' the four-momentum of a photon then \eqn{P^iP_i=0}.
#' 
#' Function \code{to3()} is a low-level helper function used when
#' \code{as.3vel()} is given a four-velocity.
#' 
#' Function \code{is.consistent.4vel()} checks for four-velocities being
#' consistent in the sense that \eqn{U^iU_i=-c^2}{U.U=-c^2}.  Giving this
#' function a vector, for example, \code{is.consistent.4vel(1:5)}, will return
#' an error.
#' 
#' Compare the functions documented here with \code{boost()}, which returns a
#' \eqn{4\times 4}{4*4} transformation matrix (which also includes rotation
#' information).
#' 
#' @aliases 4vel fourvel as.4vel is.consistent.4vel fourvelocity four-velocity
#' 4velocity 4-velocity as.4vel is.4vel to3 inner4 inner product
#' @param u A vector of three-velocities
#' @param U,V A vector of four-velocities
#' @param give In function \code{is.consistent.4vel()}, Boolean with
#' \code{TRUE} meaning to return \eqn{U\cdot U+c^2}{U.U+c^2}, which is zero for
#' a four-velocity, and default \code{FALSE} meaning to return whether the
#' four-velocity is consistent to numerical precision
#' @param TOL Small positive value used for tolerance
#' @author Robin K. S. Hankin
#' @seealso \code{\link{boost}}
#' @examples
#' 
#' 
#' a <- r3vel(10)
#' as.4vel(a)     # a four-velocity
#' 
#' as.3vel(as.4vel(a))-a   # zero to numerical precision
#' 
#' inner4(as.4vel(a))   #  -1 to numerical precision
#' 
#' stopifnot(all(is.consistent.4vel(as.4vel(a))))
#' 
#' 
#' ## check Lorentz invariance of dot product:
#' U <- as.4vel(r3vel(10))
#' V <- as.4vel(r3vel(10))
#' B <- boost(as.3vel(1:3/10))
#' 
#' frame1dotprod <- inner4(U, V)
#' frame2dotprod <- inner4(U %*% B, V %*% B)
#' max(abs(frame1dotprod-frame2dotprod))  # zero to numerical precision
#' 
#' 
#' @export fourvel
`fourvel` <- `4vel`
`threecel` <- `3cel`

`is.3vel` <- function(x){inherits(x,"3vel")}
`is.3cel` <- function(x){inherits(x,"3cel")}
`is.4vel` <- function(x){inherits(x,"4vel")}



#' Combine vectors of three-velocities and four-velocities into a single vector
#' 
#' Combines its arguments recursively to form a vector of three velocities or
#' four velocities
#' 
#' 
#' Returns a vector of three-velocities or four-velocities.  These are stored
#' as three- or four- column matrices; each row is a velocity.
#' 
#' Names are inherited from the behaviour of \code{cbind()}, not \code{c()}.
#' 
#' @aliases c.3vel c.3cel c.4vel
#' @param \dots Vectors of three-velocities
#' @note
#' 
#' This function is used extensively in \code{inst/distributive_search.R}.
#' 
#' For \dQuote{c} as in celerity or speed of light, see \code{sol()}.
#' @author Robin K. S. Hankin
#' @seealso \code{\link{sol}}
#' @keywords array
#' @examples
#' 
#' 
#' c(r3vel(3),r3vel(6,0.99))
#' 
#' 
#' 
#' @export c.3vel
`c.3vel` <- function(...){ as.3vel(do.call("rbind",list(...))) }
`c.3cel` <- function(...){ as.3cel(do.call("rbind",list(...))) }
`c.4vel` <- function(...){ as.4vel(do.call("rbind",list(...))) }



#' Print methods for three-velocities and four-velocities
#' 
#' Print methods for three-velocities
#' 
#' 
#' @aliases print.3vel print.3cel print.4vel print.4mom
#' @param x Vector of three-velocities
#' @param \dots Further arguments, currently ignored
#' @return Returns a vector of three-velocities
#' @author Robin K. S. Hankin
#' @examples
#' 
#' r3vel(10)
#' 
#' @export print.3vel
`print.3vel` <- function(x, ...){
  cat(paste("A vector of three-velocities (speed of light = ",capture.output(cat(sol())),")\n",sep=""))
  x <- unclass(x)
  if(is.null(colnames(x)) & ncol(x)==3){
    colnames(x) <- coordnames()[-1]
  }
  return(invisible(print(x)))
}

`print.3cel` <- function(x, ...){
  cat(paste("A vector of three-celerities (speed of light = ",capture.output(cat(sol())),")\n",sep=""))
  x <- unclass(x)
  if(is.null(colnames(x)) & ncol(x)==3){
    colnames(x) <- coordnames()[-1]
  }
  return(invisible(print(x)))
}

`print.4vel` <- function(x, ...){
  cat(paste("A vector of four-velocities (speed of light = ",capture.output(cat(sol())),")\n",sep=""))
  x <- rbind(unclass(x))
  if(is.null(colnames(x)) & ncol(x)==4){ 
    colnames(x) <- coordnames()
  }
  return(invisible(print(x)))
}

`length.vec` <- function(x){nrow(x)}
`names.vec` <- function(x){rownames(x)}

`names<-.vec` <- function(x,value){
  rownames(x) <- value
  return(x)
}



#' Random relativstic velocities
#' 
#' Generates random three-velocities or four-velocities, optionally specifiying
#' a magnitude
#' 
#' Function \code{r3vel()} returns a random three-velocity.  Function
#' \code{r4vel()} is a convenience wrapper for \code{as.4vel(r3vel())}.
#' 
#' Function \code{rboost()} returns a random \eqn{4\times 4}{4*4} Lorentz boost
#' matrix, drawn from the connected component.  If given \code{r=0}, then a
#' transform corresponding to a random rotation will be returned.
#' 
#' @aliases r3vel r4vel rboost
#' @param n Number of three- or four- velocities to generate
#' @param r Absolute value of the three-velocities, with default \code{NA}
#' meaning to sample uniformly from the unit ball
#' @param ... Arguments passed to \code{r3vel()}
#' @return Returns a vector of three- or four- velocities.
#' @note
#' 
#' If the speed of light is infinite, these functions require a specified
#' argument for \code{r}.
#' 
#' It is not entirely trivial to sample uniformly from the unit ball or unit
#' sphere, but it is not hard either.
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' r3vel()
#' 
#' a <- r3vel(10000)
#' b <- r3vel(1000,0.8)
#' u <- as.3vel(c(0,0,0.9))
#' 
#' pairs(unclass(u+a),asp=1)
#' pairs(unclass(a+u),asp=1)
#' 
#' is.consistent.boost(rboost())
#' 
#' sol(299792458)    # switch to SI units
#' sound <- 343      # speed of sound in metres per second
#' r3vel(100,343)    # random 3-velocities with speed = 343 m/s
#' 
#' sol(1)   # return to default c=1
#' 
#' 
#' @export r3vel
`r3vel` <- function(n=7,r=NA){
  z <- runif(n,-1,1)
  phi <- runif(n,0,2*pi)
  u <- sqrt(1-z^2)
  out <- cbind(x=u*sin(phi),y=u*cos(phi),z=z)  # Cartesian coords on unit sphere
  if(is.na(r)){
    out <- out*runif(n)^(1/3)*sol()
  }  else {
    out <- out*r
  }
  return(as.3vel(out))
}

r4vel <- function(...){as.4vel(r3vel(...))}

`rboost` <- function(r=NA){
  O <- svd(matrix(rnorm(9),3,3))$v
  if(det(O)<0){O <- -O} # det(O) either 1 or -1
  crossprod(adiag(1,O),boost(r3vel(1,r=r)))
}

`massage3` <- function(u,v){
  lu <- length(u)
  lv <- length(v)
  if( (lu >= lv) & (!is.null(names(u)))){
    names.out <- names(u)
  } else {
    names.out <- names(v)
  }

  jj <- rbind(seq(length.out=lu),seq(length.out=lv))
  if(length(names.out) != ncol(jj)){names.out <- NULL}
  return(list(
      u = (unclass(u))[jj[1,],,drop=FALSE],
      v = (unclass(v))[jj[2,],,drop=FALSE],
      names=names.out))
}


`speed` <- function(u){UseMethod("speed",u)}

`speed.3vel` <- function(u){sqrt(rowSums(unclass(u)^2))}
`speed.4vel` <- function(u){speed(as.3vel(u))}

`speedsquared` <- function(u){rowSums(unclass(u)^2)}



#' Gamma correction
#' 
#' Lorentz gamma correction term in special relativity
#' 
#' 
#' Function \code{speed(u)} returns the speed of a \code{3vel} object or
#' \code{4vel} object.
#' 
#' Function \code{gam(u)} returns the Lorentz factor
#' \deqn{\frac{1}{\sqrt{1-\mathbf{u}\cdot\mathbf{u}/c^2}}}{see PDF}
#' 
#' Function \code{gamm1(u)} returns the Lorentz factor minus 1, useful for slow
#' speeds when larger accuracy is needed (much like \code{expm1()}); to see the
#' idiom, type \dQuote{\code{gamm1.3vel}} at the commandline.  Function
#' \code{gamm1()} is intended to work with \code{3vel} objects or speeds.  The
#' function will take a 4-velocity, but this is not recommended as accuracy is
#' lost (all it does is return the time component of the 4-velocity minus 1).
#' 
#' Function \code{gam_ur()} is used for the ultrarelativistic case where speeds
#' are very close to the speed of light (the function is named for
#' \dQuote{gamma, ultrarelativistic}).  Its argument \code{d} is the deficit,
#' that is, \eqn{c-v} where \eqn{v} is the speed of the transformation.
#' Algebraically, \code{gam_ur(c-v) == gam(v)}, but if \code{d} is small
#' compared to \code{c} the result is more accurate.
#' 
#' Function \code{speedsquared(u)} returns the square of the speed of a
#' \code{3vel} object.  Use this to avoid taking a needless square root.
#' 
#' @aliases speed speed.3vel speed.4vel speedsquared gam gam.3vel gam.3cel
#' gam.4vel gam.default gamm1 gamm1.3vel gamm1.4vel gamm1.default gam_ur
#' @param u Speed: either a vector of speeds or a vector of three-velocities or
#' four-velocities
#' @param d In function \code{gam_ur()}, deficit of speed; speed of light minus
#' speed of object
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' gam(seq(from=0,by=0.1,len=10))
#' gam(r3vel(6,0.7))
#' 
#' 
#' x <- as.3vel(c(0.1,0.4,0.5))
#' speed(x)
#' 
#' gam(speed(x))  # works, but slow and inaccurate
#' gam(x)         # recommended: avoids needless coercion
#' 
#' 
#' 
#' ## Use SI units and deal with terrestrial speeds.  Use gamm1() for this.
#' sol(299792458)
#' sound <- 343 # speed of sound in SI
#' gam(sound)
#' gam(sound)-1  
#' gamm1(sound)   # gamm1() gives much higher precision
#' 
#' snail <- as.3vel(c(0.00275,0,0)) # even the world's fastest snail...
#' gamm1(snail)                     # ...has only a small relativistic correction
#' 
#' 
#' ## For the ultrarelativistic case of speeds very close to the speed of
#' ## light, use gam_ur():
#' 
#' sol(1)           # revert to relativistic units
#' 
#' gam(0.99) - gam_ur(0.01) # zero to numerical accuracy
#' 
#' omgp <- 4.9e-24  # speed deficit of the Oh-My-God particle
#' gam(1-omgp)      # numeric overflow
#' gam_ur(omgp)     # large but finite
#' 
#' 
#' @export gam
`gam` <- function(u){
  UseMethod("gam",u)
}

`gam.3vel` <- function(u){
  1/sqrt(1-rowSums(unclass(u)^2)/sol()^2)  #inline code avoids taking unnecessary sqrt()
}

`gam.4vel` <- function(u){
  u[,1]
}

`gam.3cel` <- function(u){
 sqrt(1+rowSums(unclass(u)^2))   
}

`gam.default` <- function(u){
  1/sqrt(1-u^2/sol()^2)
}

`gamm1` <- function(u){   # gamm1() named in analogy to expm1()
  UseMethod("gamm1",u)
}

`gamm1.3vel` <- function(u){
  jj <- log1p(-rowSums(unclass(u)^2/sol()^2))/2
  return(-expm1(jj)/exp(jj))
}

`gamm1.4vel` <- function(u){ # should not be here
  u[,1]-1
}

`gamm1.default` <- function(u){  # 'u' is a speed
  jj <- log1p(-u^2/sol()^2)/2
  return(-expm1(jj)/exp(jj))
}

`gam_ur` <- function(d){  # d=1-speed, d<<c
  d <- d/sol()
  return(1/sqrt(2*d-d^2))
}

`rapidity` <- function(u){ UseMethod("rapidity",u) }

`rapidity.default` <- function(u){  # 'u' is a speed
    sol()*atanh(u/sol())
}

`rapidity.3vel` <- function(u){
    rapidity(speed(u))
}

`rapidity.4vel` <- function(u){
    g <- u[,1] # gamma
    log(2*g*(g+sqrt(g^2-1))-1)/2
}

`rapidity_ur` <- function(d){# d=1-speed, d<<c
    d <- d/sol()
    sol()*log((2-d)/d)/2
}



#' Celerity and rapidity
#' 
#' The celerity and rapidity of an object (experimental functionality)
#' 
#' 
#' The \dfn{celerity} corresponding to speed \eqn{u} is defined as
#' \eqn{u\gamma}{ug} and the \dfn{rapidity} is
#' \eqn{c\cdot\mathrm{atanh}(u/c)}{omitted}.
#' 
#' Functions \code{celerity_ur()} and \code{rapidity_ur()} are used for the
#' ultrarelativistic case where speeds are very close to the speed of light.
#' Its argument \code{d} is the deficit, that is, \eqn{d=c-v} where \eqn{v} is
#' the speed of the transformation.  Algebraically, \code{celerity_ur(c-v) ==
#' celerity(v)}, but if \eqn{d=1-v/c} is small the result of
#' \code{celerity_ur()} is more accurate than that of \code{celerity()}.
#' 
#' Things get a bit sticky for celerity and rapidity if \eqn{c\neq }{c!=1}\eqn{
#' 1}{c!=1}.  The guiding principle in the package is to give the celerity and
#' rapidity the same units as \eqn{c}, so if \eqn{u\ll }{u<<c}\eqn{ c}{u<<c} we
#' have that all three of \code{celerity(u)}, \code{rapidity(u)} and \code{u}
#' are approximately equal.  Note carefully that, in contrast, \eqn{\gamma}{g}
#' is dimensionless.  Also observe that \code{d} in functions
#' \code{celerity_ur()} and \code{rapidity_ur()} has the same units as \eqn{c}.
#' 
#' @aliases celerity threecel rapidity celerity.3vel celerity.4vel
#' celerity.default celerity_ur rapidity.3vel rapidity.4vel rapidity.default
#' rapidity_ur as.3cel cel_to_vel vel_to_cel is.3cel
#' @param u,x Speed: either a vector of speeds or a vector of three-velocities
#' or four-velocities
#' @param d In functions \code{celerity_ur()} and \code{rapidity_ur()}, deficit
#' of speed; speed of light minus speed of object
#' @author Robin K. S. Hankin
#' @seealso \code{\link{gam}}
#' @examples
#' 
#' 
#' u <- 0.1  # c=1
#' c(u,celerity(u),rapidity(u))
#' 
#' omgp <- 4.9e-24  # speed deficit of the Oh-My-God particle
#' c(celerity_ur(omgp),rapidity_ur(omgp))
#' 
#' 
#' sol(299792458)                 # use SI units
#' u <- 3e7  # ~0.1c
#' c(u,celerity(u),rapidity(u))
#' 
#' 
#' snail <- 0.00275
#' c(snail,celerity(snail),rapidity(snail))
#' 
#' 
#' omgp <- omgp*sol() 
#' c(celerity_ur(omgp),rapidity_ur(omgp))
#' 
#' 
#' sol(1)
#' 
#' 
#' @export celerity
`celerity` <- function(u){ UseMethod("celerity",u) }

`celerity.default` <- function(u){  # 'u' is a speed
    u*gam(u)
}

`celerity.3vel` <- function(u){
    celerity(speed(u))
}

`celerity.4vel` <- function(u){
    g <- u[,1] # gamma
    sqrt(g^2-1)
}

`celerity_ur` <- function(d){# d=1-speed, d<<c
    d <- d/sol()
    sol()*(1-d)/sqrt(d*(2-d))
}

`add3` <- function(u,v){  # eq 2
  jj <- massage3(u,v)
  u <- jj[[1]]
  v <- jj[[2]]
  gu <- gam.3vel(u)
  uv <- rowSums(u*v)/sol()^2  # u.v/c^2
  out <- u + sweep(v,1,gu,"/") + sweep(u,1,uv*gu/(1+gu),"*")
  out <- sweep(out,1,1+uv,"/")
  rownames(out) <- jj$names
  return(as.3vel(out))
}

`neg3` <- function(u){as.3vel(-unclass(u))}

`dot3` <- function(v,r){
  if(is.infinite(sol())){ return(as.3vel(unclass(v)*r)) }
    jj <- cbind(seq_along(v),seq_along(r))
    v <- v[jj[,1]]
    r <- r[jj[,2]]


    vc <- sqrt(prod3(v))/sol()
    out <- sweep(unclass(v),1,tanh(r*atanh(vc))/vc,"*")
    out[vc==0,] <- 0
    return(as.3vel(out))
}

`prod3` <- function(u,v=u){
  jj <- massage3(u,v)
  out <- rowSums(jj[[1]]*jj[[2]])
  names(out) <- jj$names
  return(out)
}

`[.vec` <- function(x,i,j,drop=TRUE,...){
    a <- class(x)
    x <- unclass(x)
    if(missing(i) & !missing(j)){ # x[,j]
      return(x[,j,drop=drop])
    } else if(!missing(i) & !missing(j)){  # x[i,j]
      return(x[i,j,drop=drop])  # NB: unclassed
    } else if(missing(i) & missing(j)){  # x[]
      out <- x
    } else if(!missing(i) & missing(j)){  # meat of function: idiom x[i]; x[i,]
      out <- x[i,,drop=FALSE]  # NB: overrides method default
      if(ncol(out)==3){return(as.3vel(out))}
    } else {
      stop("this cannot happen")
    }
    class(out) <- a
    return(out)
}

`[<-.3vel` <- function(x,i,j,value){
    x <- unclass(x)
    value <- unclass(value)
    if(missing(i) & missing(j)){  # x[] <- value
      stop("'x[] <- value' meaningless for 3-velocities")
    } else if(missing(i) & !missing(j)){ # x[,j] <- value
        x[,j] <- value
        return(as.3vel(x))  # NB checks for speed>c
    } else if(!missing(i) & !missing(j)){  # x[i,j] <- value
        x[i,j] <- value
        return(x)  # NB no class, just a matrix
    } else if(!missing(i) & missing(j)){  # x[i,];  x[i] <- value
        if(length(value)==1){
          if(value==0){
          x[i,] <- 0
          return(as.3vel(x))
          } else {
            stop("3vel scalar replacement method only defined for special value 0")
          }
        } else {  # length(value) > 0
          jj <- t(x)
          jj[,i] <- t(value)
          x <- t(jj)
          return(as.3vel(x))
        }
    } else {
      stop("3vel replacement error: this cannot happen")
    }
}
  
`[<-.4vel` <- function(x,i,j,value){
    x <- unclass(x)
    if(missing(i) & missing(j)){  # x[] <- value
        stop("'x[] <- value' meaningless for 4-velocities")
    } else if(missing(i) & !missing(j)){ # x[,j]
        stop("'x[,j] <- value' meaningless for 4-velocities")
    } else if(!missing(i) & !missing(j)){  # x[i,j]
        x[i,j] <- value
        return(x)
    } else if(!missing(i) & missing(j)){  # x[i,] == x[i]
        if(is.4vel(value)){
            jj <- t(x)
            jj[,i] <- t(value)
            x <- t(jj)
            return(as.4vel(x))
        } else if(length(value)==1){
            if(value==0){
                x[i,1] <- 1
                x[i,-1] <- 0
                return(as.4vel(x))
            } else {
                stop("4vel scalar replacement method only defined for special value 0")
            }
        } else {  # length(value) > 0
            stop("replacement value not acceptable")
        }
    } else {
        stop("4vel replacement error: this cannot happen")
    }
}
  
`equal3` <- function(u,v){
  jj <- massage3(u,v)
  u <- jj[[1]]
  v <- jj[[2]]
  rowSums(unclass(u)!=unclass(v))==0
  }

`Ops.4vel` <- function(e1,e2){
  if(nargs() == 1){
    stop("unary operator '", .Generic, "' is not implemented for 4vel objects")
  } else {
    stop("operator '", .Generic, "' is not implemented for 4vel objects (four-velocities do not constitute a vector space).")
  }
}



#' Arithmetic Ops Group Methods for 3vel objects
#' 
#' Arithmetic operations for three-velocities
#' 
#' The function \code{Ops.3vel()} passes unary and binary arithmetic operators
#' \dQuote{\code{+}}, \dQuote{\code{-}} and \dQuote{\code{*}} to the
#' appropriate specialist function.
#' 
#' The most interesting operators are \dQuote{\code{+}} and \dQuote{\code{*}},
#' which are passed to \code{add3()} and \code{dot3()} respectively.  These are
#' defined, following Ungar, as:
#' 
#' \deqn{ }{ see PDF }\deqn{ \mathbf{u}+\mathbf{v} =
#' \frac{1}{1+\mathbf{u}\cdot\mathbf{b}/c^2} }{ see PDF }\deqn{ \left\{ }{ see
#' PDF }\deqn{ \mathbf{u} + }{ see PDF }\deqn{
#' \frac{1}{\gamma_\mathbf{u}}\mathbf{v} + }{ see PDF }\deqn{
#' \frac{1}{c^2}\frac{\gamma_\mathbf{u}}{1+\gamma_\mathbf{u}} }{ see PDF
#' }\deqn{ \left(\mathbf{u}\cdot\mathbf{v}\right)\mathbf{u} }{ see PDF }\deqn{
#' \right\} }{ see PDF }\deqn{ }{ see PDF }
#' 
#' and
#' 
#' \deqn{ }{ see PDF }\deqn{ r\odot\mathbf{v} = }{ see PDF }\deqn{ c\tanh\left(
#' }{ see PDF }\deqn{ r\tanh^{-1}\frac{\left|\left|\mathbf{v}\right|\right|}{c}
#' }{ see PDF }\deqn{
#' \right)\frac{\mathbf{v}}{\left|\left|\mathbf{v}\right|\right|} }{ see PDF
#' }\deqn{ }{ see PDF }
#' 
#' where \eqn{\mathbf{u}}{u} and \eqn{\mathbf{v}}{v} are three-vectors and
#' \eqn{r} a scalar.  Function \code{dot3()} has special dispensation for zero
#' velocity and does not treat \code{NA} entries entirely consistently.
#' 
#' Arithmetic operations, executed via \code{Ops.4vel()}, are not defined on
#' four-velocities.
#' 
#' The package is designed so that natural idiom may be used for three velocity
#' addition, see the examples section.
#' 
#' @aliases Ops.3vel Ops.4vel Ops.gyr Ops massage3 neg3 prod3 add3 dot3 equal3
#' @param e1,e2,u,v Objects of class \dQuote{\code{3vel}}, three-velocities
#' @param r Scalar value for circle-dot multiplication
#' @return Returns an object of class \code{3vel}, except for \code{prod3()}
#' which returns a numeric vector.
#' @examples
#' 
#' u <- as.3vel(c(-0.7, 0.1,-0.1))
#' v <- as.3vel(c( 0.1, 0.2, 0.3))
#' w <- as.3vel(c( 0.5, 0.2,-0.3))
#' 
#' x <- r3vel(10)   # random three velocities
#' y <- r3vel(10)   # random three velocities
#' 
#' 
#' u+v   # add3(u,v)
#' u-v   # add3(u,neg3(v))
#' 
#' -v    # neg3(v)
#' 
#' gyr(u,v,w)
#' 
#' ## package is vectorized:
#' 
#' 
#' u+x
#' x+y
#' 
#' f <- gyrfun(u,v)
#' g <- gyrfun(v,u)
#' 
#' f(g(x)) - x    # should be zero by eqn10
#' g(f(x)) - x
#' 
#' 
#' (u+v) - f(v+u)                     # zero by eqn 10
#' (u+(v+w)) - ((u+v)+f(w))           # zero by eqn 11
#' ((u+v)+w) - (u+(v+g(w)))           # zero by eqn 11
#' 
#' 
#' ## NB, R idiom is unambiguous.  But always always ALWAYS use brackets.
#' 
#' ## Ice report in lat 42.n to 41.25n Long 49w to long 50.30w saw much
#' ## heavy pack ice and great number large icebergs also field
#' ## ice.  Weather good clear
#' 
#' ## -u+v == (-u) + v == neg3(u) + v == add3(neg3(u),v)
#' 
#' ## u+v+w == (u+v)+w == add3(add3(u,v),w)
#' 
#' 
#' @export Ops.3vel
`Ops.3vel` <- function(e1,e2){
  f <- function(...){stop("odd---neither argument has class 3vel?")}
  unary <- nargs() == 1
  lclass <- nchar(.Method[1]) > 0
  rclass <- !unary && (nchar(.Method[2]) > 0)
  
  if(unary){
    if (.Generic == "+") {
      return(e1)
    } else if (.Generic == "-") {
      return(neg3(e1))
    } else {
      stop("unary operator '", .Generic, "' is not implemented for 3vel objects")
    }
  }
  if (!is.element(.Generic, c("+", "-",  "==", "!=", "*","/")))
    stop("operator '", .Generic, "' is not implemented for 3vel objects")
  
  if (.Generic == "*"){
    if(lclass & !rclass){
      return(dot3(e1,e2))
    }  else if (!lclass & rclass){
      return(dot3(e2,e1))
    } else {
      stop(" '*' requires a three-vector and a scalar")
    }
  }

  if (.Generic == "/"){
    if(lclass & !rclass){  # e.g. u/2
      return(dot3(e1,1/e2))
    } else {
      stop(" '/' requires a three-vector and a scalar")
    }
  }

  stopifnot(lclass & rclass)
    
  if (.Generic == "+") { 
    return(add3(e1, e2)) 
  } else if (.Generic == "-") { 
    return(add3(e1, neg3(e2)))
  } else if (.Generic == "==") {
    return(equal3(e1,e2))
  } else if (.Generic == "!=") {
    return(!equal3(e1,e2))
  } else {
    stop("should not reach here")
  }
}

## Equation numbers refer to Ungar 2006




#' Gyr function
#' 
#' Relativistic addition of three velocities
#' 
#' 
#' Function \code{gyr(u,v,x)} returns the three-vector
#' \eqn{\mathrm{gyr}[u,v]x}{gyr[u,v]x}.
#' 
#' Function \code{gyrfun(u,v)} returns a function that returns a three-vector;
#' see examples.
#' 
#' The speed of light (1 by default) is not used directly by these functions;
#' set it with \code{sol()}.
#' 
#' @aliases gyr gyr.a gyrfun gyrfun
#' @param u,v,x Three-velocities, objects of class \code{3vel}
#' @note Function \code{gyr()} is slightly faster than \code{gyr.a()}, which is
#' included for pedagogical reasons.
#' 
#' Function \code{gyr()} is simply
#' 
#' \code{add3(neg3(add3(u,v)),add3(u,add3(v,x)))}
#' 
#' while function \code{gyr.a()} uses the slower but more transparent idiom
#' 
#' \code{ -(u+v) + (u+(v+x)) }
#' @author Robin K. S. Hankin
#' @references \itemize{ \item Ungar 2006. \dQuote{Thomas precession: a
#' kinematic effect of the algebra of Einstein's velocity addition law.
#' Comments on \sQuote{Deriving relativistic momentum and energy: II.
#' Three-dimensional case}}. European Journal of Physics, 27:L17-L20. \item
#' Sbitneva 2001. \dQuote{Nonassociative geometry of special relativity}.
#' International Journal of Theoretical Physics, volume 40, number 1, pages
#' 359--362}
#' @examples
#' 
#' 
#' 
#' u <- r3vel(10)
#' v <- r3vel(10)
#' w <- r3vel(10)
#' 
#' x <- as.3vel(c(0.4,0.1,-0.5))
#' y <- as.3vel(c(0.1,0.2,-0.7))
#' z <- as.3vel(c(0.2,0.3,-0.1))
#' 
#' 
#' gyr(u,v,x)  # gyr[u,v]x
#' 
#' f <- gyrfun(u,v)
#' g <- gyrfun(v,u)
#' 
#' f(x)
#' f(r3vel(10))
#' 
#' f(g(x)) - x              # zero, by eqn 9
#' g(f(x)) - x              # zero, by eqn 9
#' (x+y) - f(y+x)           # zero by eqn 10
#' (u+(v+w)) - ((u+v)+f(w)) # zero by eqn 11
#' 
#' 
#' # Following taken from Sbitneva 2001:
#' 
#' rbind(x+(y+(x+z))  ,   (x+(y+x))+z)   # left Bol property
#' rbind((x+y)+(x+y)  ,   x+(y+(y+x)))   # left Bruck property
#' 
#' 
#' sol(299792458)   # speed of light in SI
#' as.3vel(c(1000,3000,1000)) + as.3vel(c(1000,3000,1000))
#' ## should be close to Galilean result
#' 
#' sol(1)   # revert to default c=1
#' 
#' 
#' @export gyr
`gyr` <- function(u,v,x){  # eq 6
  add3(neg3(add3(u,v)),add3(u,add3(v,x)))
}

`gyr.a` <- function(u,v,x){  # eq 6
  -(u+v) + (u+(v+x))
}

`gyrfun` <- function(u,v){
  return(function(x){gyr(u,v,x)})
}

`as.4vel` <- function(u){  # takes a 3vel, returns a 4vel
  u <- unclass(u)
  if(length(u)==1){
    if(u==0){
      u <- c(0,0,0)
    } else {
      stop("not defined")
    }
  }

  if(is.vector(u)){u <- t(u)}
  
  if(ncol(u)==3){
    u <- as.3vel(u) # checks for speed>c
    out <- cbind(t=1,u)*gam(u) # convert to a four-vector
    } else if(ncol(u)==4) { # assumes a 4-vector
      if(all(is.consistent.4vel(u))){
        out <- u
      } else {
        stop("inconsistent 4-velocity")
      }
    } else {
      stop("not recognised")
    }
  
  colnames(out) <- coordnames()
  class(out) <- c("4vel","vec")  # this is the only place class 4vel is assigned
  return(out)
}

`to3` <- function(U){  # takes a 4velocity, returns a 3vel
    stopifnot(is.4vel(U))
    return(as.3vel(sweep(U[, -1, drop = FALSE],1,U[,1],"/")))
}

`inner4` <- function(U,V=U){
  quad3.tdiag(eta(),unclass(U),unclass(V))
}

`is.consistent.4vel` <- function(U,give=FALSE, TOL=1e-10){

    out <- (inner4(U) + sol()^2)/sol()^2
    if(give){
        return(out)
    } else {
        return(abs(out)<TOL*sol()^2)
    }
}

`is.consistent.boost.galilean` <- function(L, give=FALSE, TOL=1e-10){
  stopifnot(all(dim(L) == 4))
  out <- crossprod(L[-1,-1])
  if(give){
    return(out)
  } else {
    return(
        all(L[1,] == c(1,0,0,0)) &&
        all(abs(out-diag(3)) < TOL)
    )
  }
}

`is.consistent.boost` <- function(L, give=FALSE, TOL=1e-10){
  if(is.infinite(sol())){return(is.consistent.boost.galilean(L, give=give, TOL=TOL))}
  out <- quad.form(eta(),L) # should be eta()
  if(give){
    return(out)
  } else {
    return(all(abs(out-eta())<TOL*sol()^2))
  }
}

`my_seg` <- function(u,start=as.3vel(0), bold=5, ...){
  start <- unclass(start)[,1:2,drop=FALSE]
  u <- unclass(u)
  u <- u[,1:2,drop=FALSE]   # now a two-column matrix
  
  for(i in seq_len(nrow(u))){
    if(i==bold | nrow(u)==1){
      lwd <- 4
      length <- 0.3
    } else {
      lwd <- 1
      length <- 0.1
    }
    arrows(
        x0=start[i,1],
        y0=start[i,2],
        x1=u[i,1],
        y1=u[i,2],
        angle=10,
        length=length,
        lwd=lwd,
        ...
    )
  }
}

`comm_fail1` <- function(u,v,bold=5,r=1){
    plot(NA, xlim=c(0,1), ylim=c(-0.2,1),
         type='n', asp=1,
         xlab='', ylab='', axes=FALSE,
         main="Failure of the parallelogram law")
    my_seg(u,start=0*u,col='purple',bold=bold)
    my_seg(u+v,start=u+v*0,col='black',bold=bold)
    my_seg((u+v)-u,start=u+v,col='red',bold=bold)
    my_seg(((u+v)-u)-v,start=(u+v)-u,col='blue',bold=bold)
    points(((u+v)-u)-v,pch=16,col='blue')
    legend("topright",lty=c(1,1,1,1,2),
           col=c("purple","black","red","blue","green"),
           legend=c("leg 1","leg 2","leg 3","leg 4", "c=1"))
    points(0,0,pch=16)
    
    theta <- seq(from=0,to=2*pi,len=100)
    points(r*sin(theta),cos(r*theta),type='l',lty=2,col='green')
}

`comm_fail2` <- function(u,v,bold=5,r=1){
    plot(NA, xlim=c(-0.2,0.9), ylim=c(-0.2,1),
         type='n',asp=1,
         xlab='',ylab='',axes=FALSE,
         main="Failure of the parallelogram law")
    my_seg(u,start=0*u,col='black',bold=bold)
    my_seg(u+v,start=u+v*0,col='blue',bold=bold)
    
    my_seg(v,start=0*v,col='blue',bold=bold)
    my_seg(v+u,start=v,col='black',bold=bold)
    
    my_seg(u+v,start=v+u,col='red',code=0,bold=bold)
    
    legend("topright",lty=c(1,1,1,2),
           col=c("black","blue","red","green"),
           legend=c("u","v","mismatch","c=1"))
    
    points(0,0,pch=16)
    
    theta <- seq(from=0,to=2*pi,len=100)
    points(r*sin(theta),r*cos(theta),type='l',lty=2,col='green')
}

`ass_fail` <- function(u,v,w,bold=5,r=1){
    plot(c(0,0),c(-0.1,1),xlim=c(0,1),ylim=c(-0.3,1),type='n',
         asp=1,pty='m',xlab='',ylab='',
         axes=FALSE,main='Failure of associative property')
    my_seg(u,start=v*0,col='black',bold=bold)
    my_seg(u+(v+w),start=v*0+u,col='black',bold=bold)
    
    my_seg(u+v,start=v*0,col='blue',bold=bold)
    my_seg((u+v)+w,start=u+v,col='blue',bold=bold)
    
    my_seg(u+(v+w),start=(u+v)+w,col='red',bold=bold)
    legend("topright",lty=c(1,1,1,2),col=c("black","blue","red","green"),legend=c("u+(v+w)","(u+v)+w","mismatch","c=1"))
    
    points(0,0,pch=16,cex=3)
    theta <- seq(from=0,to=2*pi,len=100)
    points(r*sin(theta),r*cos(theta),type='l',lty=2,col='green')
}



#' seq method for three velocities
#' 
#' Simplified version of \code{seq()} for three-velocities.
#' 
#' 
#' \code{seq(a,b,n)} returns \code{a + t*(-b+a)} where \code{t} is numeric
#' vector \code{seq(from=0,to=1,len=n)}.
#' 
#' This definition is one of several plausible alternatives, but has the nice
#' property that the first and last elements are exactly equal to \code{a} and
#' \code{b} respectively.
#' 
#' 
#' @param from,to Start and end of sequence
#' @param len Length of vector returned
#' @param ... Further arguments (currently ignored)
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' a <- as.3vel(c(4,5,6)/9)
#' b <- as.3vel(c(-5,6,8)/14)
#' x <- seq(a,b,len=9)
#' 
#' x[1]-a # should be zero
#' x[9]-b # should be zero
#' 
#' 
#' jj <- a + seq(0,1,len=9)*(b-a)
#' 
#' jj-x   # decidedly non-zero
#' 
#' 
#' 
#' @export seq.3vel
`seq.3vel` <- function(from, to, len,...){
  tee <- seq(from=0,to=1,length.out=len)
  return(from + tee*(-from+to))
}



#' Lorentz transformations
#' 
#' Lorentz transformations: boosts and rotations
#' 
#' Arguments \code{u,v} are coerced to three-velocities.
#' 
#' A rotation-free Lorentz transformation is known as a \dfn{boost} (sometimes
#' a \dfn{pure boost}), here expressed in matrix form.  Pure boost matrices are
#' symmetric if \eqn{c=1}.  Function \code{boost(u)} returns a \eqn{4\times
#' 4}{4x4} matrix giving the Lorentz transform of an arbitrary three-velocity
#' \code{u}.
#' 
#' Boosts can be successively applied with regular matrix multiplication.
#' However, composing two successive pure boosts does not in general return a
#' pure boost matrix: the product is not symmetric in general.  Also note that
#' boost matrices do not commute.  The resulting matrix product represents a
#' \dfn{Lorentz transform}.
#' 
#' It is possible to decompose a Lorentz transform \eqn{L} into a pure boost
#' and a spatial rotation.  Thus \eqn{L=OP} where \eqn{O} is an orthogonal
#' matrix and \eqn{P} a pure boost matrix; these are returned by functions
#' \code{orthog()} and \code{pureboost()} respectively.  If the speed of light
#' is not equal to 1, the functions still work but can be confusing.
#' 
#' Functions \code{pureboost.galilean()} and \code{orthog.galilean()} are the
#' Newtonian equivalents of \code{pureboost()} and \code{orthog()}, intended to
#' be used when the speed of light is infinite (which causes problems for the
#' relativistic functions).
#' 
#' As noted above, the composition of two pure Lorentz boosts is not
#' necessarily pure.  If we have two successive boosts corresponding to \eqn{u}
#' and \eqn{v}, then the composed boost may be decomposed into a pure boost of
#' \code{boost(u+v)} and a rotation of \code{rot(u,v)}.
#' 
#' The reason argument \code{include_sol} exists is that function
#' \code{orthog()} needs to call \code{pureboost()} in an environment where we
#' pretend that \eqn{c=1}.
#' 
#' @aliases boost rot thomas Thomas Thomas rotation wigner Wigner Wigner
#' rotation precession boostfun decompose pureboost orthog pureboost.galilean
#' orthog.galilean is.consistent.boost is.consistent.boost.galilean
#' is.consistent.galilean.boost
#' @param u,v Three-velocities, coerced to class \code{3vel}.  In function
#' \code{boost()}, if \code{u} takes the special default value \code{0}, this
#' is interpreted as zero three velocity
#' @param L Lorentz transform expressed as a \eqn{4\times 4}{4x4} matrix
#' @param TOL Numerical tolerance
#' @param give Boolean with \code{TRUE} meaning to return the transformed
#' metric tensor (which should be the flat-space \code{eta()}; qv) and default
#' \code{FALSE} meaning to return whether the matrix is a consistent boost or
#' not
#' @param space Boolean, with default \code{TRUE} meaning to return just the
#' spatial component of the rotation matrix and \code{FALSE} meaning to return
#' the full \eqn{4\times 4}{4x4} matrix transformation
#' @param tidy In \code{pureboost.galilean()}, Boolean with default \code{TRUE}
#' meaning to return a \dQuote{tidy} boost matrix with spatial components
#' forced to be a \eqn{3\times 3}{3x3} identity matrix
#' @param include_sol In function \code{pureboost()}, Boolean with default
#' \code{TRUE} meaning to correctly account for the speed of light, and
#' \code{FALSE} meaning to assume \eqn{c=1}. See details
#' @return Function \code{boost()} returns a \eqn{4\times 4}{4*4} matrix;
#' function \code{rot()} returns an orthogonal matrix.
#' @note Function \code{rot()} uses \code{crossprod()} for efficiency reasons
#' but is algebraically equivalent to
#' 
#' \code{boost(-u-v) %*% boost(u) %*% boost(v)}.
#' @author Robin K. S. Hankin
#' @references \itemize{ \item Ungar 2006. \dQuote{Thomas precession: a
#' kinematic effect\ldots{}}. European Journal of Physics, 27:L17-L20 \item
#' Sbitneva 2001. \dQuote{Nonassociative geometry of special relativity}.
#' International Journal of Theoretical Physics, volume 40, number 1, pages
#' 359--362 \item Wikipedia contributors 2018.  \dQuote{Wigner rotation},
#' Wikipedia, The Free Encyclopedia.
#' \url{https://en.wikipedia.org/w/index.php?title=Wigner_rotation&oldid=838661305}.
#' Online; accessed 23 August 2018 }
#' @examples
#' 
#' boost(as.3vel(c(0.4,-0.2,0.1)))
#' 
#' u <- r3vel(1)
#' v <- r3vel(1)
#' w <- r3vel(1)
#' 
#' boost(u) - solve(boost(-u))  # should be zero
#' 
#' boost(u) %*% boost(v)   # not a pure boost (not symmetrical)
#' boost(u+v)  # not the same!
#' boost(v+u)  # also not the same!
#' 
#' u+v  # returns a three-velocity
#' 
#' 
#' boost(u) %*% boost(v) %*% boost(w)  # associative, no brackets needed
#' boost(u+(v+w))  # not the same!
#' boost((u+v)+w)  # also not the same!
#' 
#' 
#' rot(u,v)
#' rot(v,u)    # transpose (=inverse) of rot(u,v)
#' 
#' 
#' rot(u,v,FALSE) %*% boost(v) %*% boost(u)
#' boost(u+v)     # should be the same.
#' 
#' 
#' orthog(boost(u) %*% boost(v)) - rot(u,v,FALSE)  # zero to numerical precision
#' pureboost(boost(v) %*% boost(u)) - boost(u+v)   # ditto
#' 
#' 
#' ## Define a random-ish Lorentz transform
#' L <- boost(r3vel(1)) %*% boost(r3vel(1)) %*% boost(r3vel(1))
#' 
#' ## check it:
#' 
#' 
#' \dontrun{   # needs emulator package
#' quad.form(eta(),L)  # should be eta()
#' }
#' 
#' ## More concisely:
#' is.consistent.boost(L)     # should be TRUE
#' 
#' ## Decompose L into a rotation and a pure boost:
#' U <- orthog(L)
#' P <- pureboost(L)
#' 
#' L - U %*% P              # should be zero (L = UP)
#' crossprod(U)               # should be identity (U is orthogonal)
#' P - t(P)                   # should be zero (P is symmetric)
#' 
#' ## First row of P should be a consistent 4-velocity:
#' is.consistent.4vel(P[1,,drop=FALSE],give=TRUE)
#' 
#' @export boost
`boost` <- function(u=0){  # v = (u,v,w)
  if(identical(u,0)){u <- c(0,0,0)}
  u <- as.3vel(u)
  if(is.infinite(sol())){return(flob(rbind(c(1,0,0,0),cbind(t(-u),diag(3)))))}
  g <- gam(u)  
  u <- as.vector(u)/sol()  # convert to c=1 units (NB previous line needs sol())
  jj <- -g*u
  
  out <- rbind(c(g,jj), cbind(jj,diag(3) + g^2*outer(u,u)/(1+g)))

  ## convert units back to SI or whatever:
  out <- quad3.form(out,diag(c(1/sol(),1,1,1)),diag(c(sol(),1,1,1)))

  rownames(out) <- coordnames()
  colnames(out) <- coordnames()

  return(out)
 }

`rot` <- function(u,v,space=TRUE){
  u <- as.3vel(u)
  v <- as.3vel(v)
  out <- boost(-u-v) %*% boost(u) %*% boost(v)
  if(space){
    return(out[2:4,2:4])
  } else {
    return(out)
  }
}
   
`pureboost.galilean` <- function(L,tidy=TRUE){
  stopifnot(is.consistent.boost.galilean(L))
  out <- crossprod(orthog.galilean(L),L)
  if(tidy){out[2:4,2:4] <- diag(3)}
  return(out)
}

`orthog.galilean` <- function(L){
  stopifnot(is.consistent.boost.galilean(L))
  magic::adiag(1,L[-1,-1])
}

`pureboost` <- function(L,include_sol=TRUE){
  if(is.infinite(sol())){return(pureboost.galilean(L))}
  if(include_sol){
    left <- ptm(TRUE)
    right <- ptm(FALSE)
  } else {
    left <- diag(nrow=4)
    right <- diag(nrow=4)
  }
    
  jj <- eigen(crossprod(quad3.form(L,left,right)))
  flob(quad3.form(quad.tform(sqrt(diag(jj$values)),jj$vectors),right,left))
}

`orthog` <- function(L){
  if(is.infinite(sol())){return(orthog.galilean(L))}
  L <- quad3.form(L,ptm(TRUE),ptm(FALSE))  # convert to natural units
  flob(tcrossprod(L, solve(pureboost(L,FALSE))))
} 



#' Coerce 3-vectors and 4-vectors to a matrix
#' 
#' Coerce 3-vectors and 4-vectors to a matrix.  A convenience wrapper for
#' \code{unclass()}
#' 
#' 
#' @aliases as.matrix.3vel as.matrix.4vel
#' @param x Object of class \code{3vel} or \code{4vel}
#' @param ... Further arguments (currently ignored)
#' @author Robin K. S. Hankin
#' @examples
#' 
#' as.matrix(r3vel(5))
#' as.matrix(r4vel(5))
#' 
#' @export as.matrix.3vel
`as.matrix.3vel` <- function(x,...){unclass(x)}
`as.matrix.4vel` <- function(x,...){unclass(x)}



#' Direction cosines
#' 
#' Given a vector of three-velocities, returns their direction cosines
#' 
#' 
#' @aliases cosines cosine dcosines direction.cosines
#' @param u A vector of three-velocities
#' @param drop Boolean, with default \code{TRUE} meaning to coerce return value
#' from a one-row matrix to a vector, and \code{FALSE} meaning to consistently
#' return a matrix
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' cosines(r3vel(7))
#' 
#' 
#' cosines(r3vel(1),drop=TRUE)
#' cosines(r3vel(1),drop=FALSE)
#' 
#' @export cosines
`cosines` <- function(u, drop=TRUE){
  out <- sweep(unclass(u),1,speed(u),"/")
  if(drop){out <- drop(out)}
  return(out)
}
