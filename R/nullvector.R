## functionality for null vectors, specifically the four-momentum of a
## photon.  Also some functionality for the four-momentum of objects
## with non-zero rest mass.

`vel_to_4mom` <- function(U,m=1){  # U is (four) velocity, m rest mass
  U <- as.4vel(U)
  jj <- cbind(seq_along(U),seq_along(m))
  as.4mom(sweep(unclass(U)[jj[,1],,drop=FALSE],1,m[jj[,2]],`*`))
}

`p_to_4mom` <- function(p,E=1){  # p is a 3-momentum, E the energy
  p <- rbind(p)
  jj <- cbind(seq_len(nrow(p)),seq_along(E))
  as.4mom(cbind(E[jj[,2]]/sol(),p[jj[,1],,drop=FALSE]))
}

`fourmom_mult` <- function(P,n){
  P <- unclass(P)
  jj <- cbind(seq_len(nrow(P)),seq_along(n))
  as.4mom(sweep(P[jj[,1],,drop=FALSE],1,n[jj[,2]],`*`))
}

`fourmom_add` <- function(e1,e2){
  e1 <- unclass(e1)
  e2 <- unclass(e2)
  jj <- cbind(seq_len(nrow(e1)),seq_len(nrow(e2)))
  as.4mom(e1[jj[,1],,drop=FALSE] + e2[jj[,2],,drop=FALSE])
}
  
`as.4mom` <- function(x){
  stopifnot(ncol(x) == 4)
  class(x) <- c("4mom","vec") # This is the only place class 4mom is assigned
  return(x)
}
  
`is.4mom` <- function(x){inherits(x,"4mom")}

`as.photon` <- function(x,E=1){
    if(is.3vel(x)){
        x <- unclass(x)
        out <- sweep(x,1,sqrt(rowSums(x^2)),`/`)
    } else if(is.vector(x)){
        out <- rbind(x)
    } else {
        stop("not recognised")
    }
    
    ## out <- cbind(1,out)
    ## out %<>% sweep(1,E,`*`) %>% sweep(2,c(sol(),1,1,1), `/`) %>% as.4mom()
    out <-
        as.4mom(sweep(sweep(cbind(1,out),1,E,`*`),2,c(sol(),1,1,1), `/`))
    class(out) <- c(class(out),"nullvec")
    return(out)
}

`is.consistent.nullvec` <- function(N,TOL=1e-10){
    inner4(N)<TOL*sol()^2
}

`print.4mom` <- function(x, ...){
  x <- unclass(x)
  colnames(x) <- c("E","p_x","p_y","p_z")
  return(invisible(print(x)))
}

`reflect` <- function(P,m,ref=1){  # 'P' is the four-momentum (canonically
                             # that of a photon but will work for
                             # anything)

  P <- rbind(P)
  if(missing(m)){  # direct reflection
    P[,-1] <- -P[,-1]
  } else {
    m <- rbind(m)
    jj <- cbind(seq_len(nrow(P)),seq_len(nrow(m)),seq_along(ref))
    
    P <- P[jj[,1],,drop=FALSE]
    m <- m[jj[,2],,drop=FALSE]
    ref <- ref[jj[,3]]

    P[,-1] <- P[,-1] - sweep(2*m,1,rowSums(P[,-1]*m)/rowSums(m*m),`*`)
  }
  return(as.4mom(sweep(P,1,ref,`*`)))
}


`Ops.4mom` <- function(e1,e2){
  f <- function(...){stop("odd---neither argument has class 3vel?")}
  unary <- nargs() == 1
  lclass <- nchar(.Method[1]) > 0
  rclass <- !unary && (nchar(.Method[2]) > 0)

  if (!is.element(.Generic, c("+", "-", "*")))
    stop("operator '", .Generic, "' is not implemented for 4mom objects")

  if(unary){
    if (.Generic == "+") {
      return(e1)
    } else if (.Generic == "-") {  # reflect about-face
      e1[, -1] <- -e1[, -1]
      return(e1)
    } else {
      stop("Unary operator '", .Generic, "' should not exist for 4mom objects")
    }
  }
  
  if (.Generic == "+"){
    if(lclass & rclass){
      return(fourmom_add(e1,e2))
    }  else {
      stop("error in Ops.4mom()")
    }
  } else if(.Generic == "-"){
    e2[,-1] <- -e2[,-1]
    return(e1+e2)
  } else if(.Generic == "*"){
    if(xor(lclass,rclass)){
      if(lclass & !rclass){
        return(fourmom_mult(e1,e2))
      } else if(!lclass & rclass){
        return(fourmom_mult(e2,e1))
      } else {
        stop("should not reach here")
      }
    } else {
      stop("Operator '", .Generic, "' not implemented for two 4moms")
    }
  } else {
    stop("should not reach here")
  }
}

`sum.4mom` <- function(..., na.rm=FALSE){
  as.4mom(rbind(colSums(do.call(`rbind`, list(...)), na.rm=na.rm)))
}
