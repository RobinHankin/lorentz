## Tests of basic SR relationships, eg U.U=-c^2 for a four-velocity.

test_that("Test suite aab.R",{
  
  `checker1` <- function(U){
    expect_true(all(is.consistent.4vel(U)))
    return(TRUE)
  }
  
  `checkerB` <- function(B){
    expect_true(is.consistent.boost(B))
    return(TRUE)
  }
  
  
  for(i in 1:2){

    n <- 10
    s <- 0.9

    sol(1)         ; checker1(as.4vel(r3vel(n,s)))
    sol(10)        ; checker1(as.4vel(r3vel(n,s)))
    sol(100)       ; checker1(as.4vel(r3vel(n,s)))
    sol(299792458) ; checker1(as.4vel(r3vel(n,s)))
    
    sol(1)         ; checkerB(boost(r3vel(1,s)) %*% boost(r3vel(1,s)) %*% boost(r3vel(1,s)))
    sol(10)        ; checkerB(boost(r3vel(1,s)) %*% boost(r3vel(1,s)) %*% boost(r3vel(1,s)))
    sol(100)       ; checkerB(boost(r3vel(1,s)) %*% boost(r3vel(1,s)) %*% boost(r3vel(1,s)))
    sol(299792458) ; checkerB(boost(r3vel(1,s)) %*% boost(r3vel(1,s)) %*% boost(r3vel(1,s)))
    
    ## reset speed of light:
    sol(1)
  }
})
