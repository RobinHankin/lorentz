require("gyrogroup")

test <- function(x,y, TOL= 1e-10){
  stopifnot(prod3(x-y)<TOL)
  return(TRUE)
}

g <- function(...){
  u <- r3vel(10)
  v <- r3vel(10)
  w <- r3vel(10)
  
  x <- as.3vel(c(0.4,0.1,-0.5))
  y <- as.3vel(c(0.1,0.2,-0.7))
  
  
  gyr(u,v,x)  # gyr[u,v]x
  
  f <- gyrfun(u,v)
  g <- gyrfun(v,u)
  
  test(f(g(x)), x)              # zero, by eqn 9
  test(g(f(x)), x)              # zero, by eqn 9
  test(u+v, f(v+u))             # zero by eqn 10
  test(u+(v+w)  ,  (u+v)+f(w))  # zero by eqn 11
  test((u+v)+w  ,  u+(v+g(w)))  # zero by eqn 11
}
