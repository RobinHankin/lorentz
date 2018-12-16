## functionality for null vectors, specifically the four-momentum of a
## photon.

`as.nullvec` <- function(x,E=1){
    if(is.3vel(x)){
        x <- unclass(x)
        out <- sweep(x,1,sqrt(rowSums(x^2)),`/`)
    } else if(is.vector(x)){
        out <- rbind(x)
    } else {
        stop("not recognised")
    }
    
    out %<>% cbind(1, .) %>% sweep(1,E,`*`) %>% sweep(2,c(sol(),1,1,1), `/`)  %>% `class<-`("nullvec")
}

`is.consistent.nullvec` <- function(N,TOL=1e-10){
    inner4(N)<TOL*sol()^2
}

`print.nullvec` <- function(x, ...){
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
    
    P <- P[jj[,1],]
    m <- m[jj[,2],]
    ref <- ref[jj[,3]]

    P[,-1] <- P[,-1] - sweep(2*m,1,rowSums(P[,-1]*m)/rowSums(m*m),`*`)
  }
  return(sweep(P,1,ref,`*`))
}
  



