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
    return(c)
  }
}

`eta` <- function(){
  diag(c(-sol()^2,1,1,1))
}

`as.3vel` <- function(x){
  x <- unclass(x)
  if(length(x)==1){
    if(x==0){
      x <- rbind(c(0,0,0))
    } else {
      stop("not defined")
    }
  }
  if(is.vector(x)){x <- t(x)}
  if(ncol(x) == 4){   # assumed to be a 4-velocity
    x <- to3(x)  
  }
  if(all(rowSums(x^2)<sol()^2)){
      class(x) <- '3vel'   # this is the only place where the class is set
      return(x)
  } else {
      stop("speed > c")
  }
}

`3vel` <- function(n){ as.3vel(matrix(0,n,3))  }

`threevel` <- `3vel`

`is.3vel` <- function(x){inherits(x,"3vel")}

`c.3vel` <- function(...){ as.3vel(do.call("rbind",list(...))) }

`print.3vel` <- function(x, ...){
  x <- unclass(x)
  if(is.null(colnames(x)) & ncol(x)==3){
    colnames(x) <- c("x","y","z")
  }
  return(invisible(print(x)))
}

`length.3vel` <- function(x){nrow(x)}

`names.3vel` <- function(x){rownames(x)}

`names<-.3vel` <- function(x,value){
  rownames(x) <- value
  return(x)
}
  
`r3vel` <- function(n,r=NA){
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

`speed` <- function(u){sqrt(rowSums(unclass(u)^2))}

`gam` <- function(u){
  UseMethod("gam",u)
}

`gam.3vel` <- function(u){
  1/sqrt(1-rowSums(unclass(u)^2)/sol()^2)  #inline code avoids taking unnecessary sqrt()
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
  
`gamm1.default` <- function(u){
  jj <- log1p(-u^2/sol()^2)/2
  return(-expm1(jj)/exp(jj))
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
    jj <- cbind(seq_along(v),seq_along(r))
    v <- v[jj[,1]]
    r <- r[jj[,2]]
  
    vc <- sqrt(prod3(v))/sol()
    as.3vel(sweep(unclass(v),1,tanh(r*atanh(vc))/vc,"*"))
}

`prod3` <- function(u,v=u){
  jj <- massage3(u,v)
  out <- rowSums(jj[[1]]*jj[[2]])
  names(out) <- jj$names
  return(out)
}

`[.3vel` <- function(x,...){
  out <- unclass(x)
  out <- out[...,,drop=FALSE]
  as.3vel(out)
}

`[<-.3vel` <- function(x,index,value){
  out <- unclass(x)
  if(length(unclass(value))==1){
    if(value==0){
      out[index,] <- 0
      return(as.3vel(out))
    } else {
      stop("value not defined")
    }
  }
  out <- t(out)
  out[,index] <- t(unclass(as.3vel(value)))
  return(as.3vel(t(out)))
}
  
`equal3` <- function(u,v){
  jj <- massage3(u,v)
  u <- jj[[1]]
  v <- jj[[2]]
  rowSums(unclass(u)!=unclass(v))==0
  }

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
      stop("Unary operator '", .Generic, "' is not implemented for 3vel objects")
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
    if(lclass & !rclass){
      return(dot3(e1,1/e2))
    }  else if (!lclass & rclass){
      return(dot3(e2,1/e1))
    } else {
      stop(" '*' requires a three-vector and a scalar")
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
  if(is.3vel(u)){
    out <- cbind(t=1,u)*gam(u)
  } else if(ncol(u)==4) { # assumes a 4-vector
    if(is.consistent.4vel(u)){
      out <- u
    } else {
      stop("inconsistent 4velocity")
    }
  } else if(is.vector(u)){
    return(Recall(t(u)))
  } else {
    stop("not recognised")
  }
  colnames(out) <- c("t","x","y","z")
  return(out)
}

`to3` <- function(U){  # takes a 4velocity, returns a 3vel
  if(is.consistent.4vel(U)){
    return(U[,-1]/U[,1])
  } else {
    stop("not consistent 4 velocity")
  }
}

`inner4` <- function(U){
  quad.tdiag(eta(),U)
}

`is.consistent.4vel` <- function(U,TOL=1e-10){
  all((inner4(U) + sol()^2)/sol()^2 < TOL)
}
