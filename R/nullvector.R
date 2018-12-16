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



