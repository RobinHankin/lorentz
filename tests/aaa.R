## Some tests, mostly drawn from Ungar 1997 and Ungar 2006.  The
## functions are defined first and used at the very end.

## See file aab.R for more conventional tests of behaviour of
## four-vectors in special relativity, eg U.U=-c^2 for a
## four-velocity.


require("lorentz")

`test` <- function(x,y, TOL= 1e-9){
    if(is.3vel(x)){
        error <- prod3(x-y)
    } else {
        error <- abs(x-y)^2
    }
    stopifnot(all(error<TOL))
    return(TRUE)
}

`manytests` <- function(n=10,x,y){ # equation numbers refer to Ungar 2006

    u <- r3vel(n)
    v <- r3vel(n)
    w <- r3vel(n)
    
    f <- gyrfun(u,v)
    g <- gyrfun(v,u)
    
    test(-u+(u+v)  ,  v)                  # eqn 3
    test(prod3(f(x),f(y))  ,  prod3(x,y)) # eqn 7
    test(f(x+y)  , f(x)+f(y))             # eqn 8
    test(f(g(x))  ,   x)                  # eqn 9
    test(g(f(x)) ,  x)                    # eqn 9
    test(u+v  ,  f(v+u))                  # eqn 10
    test(u+(v+w)  ,  (u+v)+f(w))          # eqn 11
    test((u+v)+w  ,  u+(v+g(w)))          # eqn 11
    
    return(TRUE)  # dummy return value
}

`manydottests` <- function(r1,r2,v){  # equation numbers refer to Ungar 1997
    test((r1+r2)*v  ,  r1*v + r2*v) # 6.6
    test((r1*r2)*v  ,  r1*(r2*v)  ) # 6.7
    ## I don't understand 6.8...
    return(TRUE)
}

`many123dottests` <- function(v){
    test(( 1)*v  ,   v)   
    test((-1)*v  ,  -v)    # nontrivial!
    
    test(( 2)*v  ,   v+v)    
    test((-2)*v  ,  -v-v)    
    
    test(( 3)*v  ,   v+v+v)    
    test((-3)*v  ,  -v-v-v)    
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
    test(copies(v,n),n*v)
    return(TRUE)
}

`manysbitneva` <- function(x,y,z){
    test(x+(y+(x+z))  ,   (x+(y+x))+z)   # left Bol property
    test((x+y)+(x+y)  ,   x+(y+(y+x)))   # left Bruck property
    return(TRUE)
}

manytests(n = 10, x = as.3vel(c(0.4,0.1,-0.5)), y = as.3vel(c(0.1,0.2,-0.7)))
manytests(n=10, x=r3vel(10),    y=r3vel(10))
manytests(n=10, x=r3vel(10,0.9),y=r3vel(10,0.9))

manydottests( 3, 2, r3vel(100,0.9))
manydottests( 1.3, 1.4, r3vel(10))

many123dottests(r3vel(10))

manycopytests(r3vel(10,0.9),5)  # "0.9" avoids numerical roundoff problems
manycopytests(r3vel(10,0.9),-5)


   
manysbitneva(r3vel(10),r3vel(10),r3vel(10))
