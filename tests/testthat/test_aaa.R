## This file follows the structure of aaa.R in the free group package.

## Some tests, mostly drawn from Ungar 1997 and Ungar 2006.  The
## functions are defined first and used at the very end.

## See file aab.R for more conventional tests of behaviour of
## four-vectors in special relativity, eg U.U=-c^2 for a
## four-velocity.



test_that("Test suite aaa.R",{

`test_close` <- function(x,y, TOL= 1e-9){  # tests x,y being 'close'
    if(is.3vel(x)){
        error <- prod3(x-y)
    } else {
        error <- abs(x-y)^2
    }

    expect_true(all(error<TOL))
}

`ungar2` <- function(u,v){ # equation numbers refer to Ungar 2006

    f <- gyrfun(u,v)
    g <- gyrfun(v,u)

    test_close(-u+(u+v), v)                  # eqn 3
    test_close(prod3(f(u),f(v)), prod3(u,v)) # eqn 7
    test_close(f(u+v), f(u)+f(v))            # eqn 8
    test_close(u+v, f(v+u))                  # eqn 10
    return(TRUE)  # dummy return value
}

`ungar3` <- function(u,v,w){ # equation numbers refer to Ungar 2006

    f <- gyrfun(u,v)
    g <- gyrfun(v,u)

    test_close(f(g(w)), w) # eqn 9
    test_close(g(f(w)), w) # eqn 9

    test_close(u+(v+w), (u+v)+f(w)) # eqn 11
    test_close((u+v)+w, u+(v+g(w))) # eqn 11

    return(TRUE)  # dummy return value
}

`ungardist` <- function(r1,r2,v){  # equation numbers refer to Ungar 1997
    test_close((r1+r2)*v  ,  r1*v + r2*v) # 6.6
    test_close((r1*r2)*v  ,  r1*(r2*v)  ) # 6.7
    ## I don't understand 6.8...
    return(TRUE)
}

`many123dottests` <- function(v){
    test_close(( 1)*v,  v)   
    test_close((-1)*v, -v)    # nontrivial!
    
    test_close(( 2)*v,  v+v)    
    test_close((-2)*v, -v-v)    
    
    test_close(( 3)*v,  v+v+v)    
    test_close((-3)*v, -v-v-v)    
}

`copies` <- function(v,n){  # v a 3-vector, n an integer
    if(n<0){return(Recall(-v,-n))}  # tested in many123dottests()
    x <- threevel(length(v))
    for(i in seq_len(n)){
        x <- x+v   #        x %<>% add(v)
    }
    return(x)
}

`manycopytests` <- function(v,n){
    test_close(copies(v,n),n*v)
    return(TRUE)
}

`sbitneva` <- function(u,v,w){
    test_close(u+(v+(u+w)), (u+(v+u))+w)   # left Bol property
    test_close((u+v)+(u+v), u+(v+(v+u)))   # left Bruck property
    return(TRUE)
}

for(i in 1:2){

  n <- 10  # size of vector
  s <- 0.9 # maximum speed
  u <- r3vel(n,s)
  v <- r3vel(n,s)
  w <- r3vel(n,s)

  r1 <- runif(1)*s
  r2 <- runif(1)*s
  

  ungar2(u,v)
  ungar3(u,v,w)
  ungardist(r1,r2,v)
  
  manycopytests(u,5) 
  manycopytests(u,-5)

  sbitneva(u,v,w)

}

})
