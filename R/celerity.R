`cel_to_vel` <- function(x){
  x <- unclass(x)
  sweep(x,1,sqrt(1+rowSums(x^2)/sol()^2),"/")
}

`vel_to_cel` <- function(x){
  out <- as.4vel(x)
  as.3cel(unclass(out)[,-1])
}


