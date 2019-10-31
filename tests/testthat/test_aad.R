## Tests of R/gyrogroup.R functionality

test_that("Test suite aad.R",{
  
  for(i in 1:2){
    options(showSOL = TRUE)
    sol(4)
    sol(1)

    expect_output(print(flob(diag(3))))
    expect_output(print(flob(diag(4))))
    expect_error(print(flob(diag(5))))

    expect_silent(eta(downstairs = FALSE))

    expect_silent(as.3vel(0))
    expect_error(as.3vel(1))

    expect_silent(as.3vel(r4vel(3)))
    expect_error(as.3vel(c(100,55,65656)))

    expect_error(as.3vel(diag(9)))
    expect_silent(`4vel`(7))

    u1 <- r3vel(3)
    u2 <- r3vel(5)
    expect_silent(c(u1,u2))
    expect_output(print(u1))
    colnames(u1) <- NULL
    expect_output(print(u1))



    names(u1) <- letters[1:3]
    expect_output(print(u1))

    U1 <- r4vel(3)
    U2 <- r4vel(5)
    expect_silent(c(U1,U2))
    colnames(U1) <- NULL
    expect_output(print(U1))
    expect_output(print(U1))
    names(U1) <- LETTERS[1:3]
    expect_output(print(U1))

    expect_error(rboost(65))
    expect_silent(rboost(0.3))

    u <- r3vel(10)
    names(u) <- letters[1:10]
    v <- r3vel(5)
    expect_silent(u+v)

    expect_true(all(speed(r3vel(10)) < 1))
    expect_true(all(speed(r4vel(10)) < 1))
    
    expect_true(all(speedsquared(r3vel(10)) < 1))
    expect_true(all(speedsquared(r4vel(10)) > 1))

    expect_true(all(gam(r3vel(10)) > 1))
    expect_true(all(gam(r4vel(10)) > 1))

    expect_true(all(gamm1(r3vel(10)) > 0))
    expect_true(all(gamm1(r4vel(10)) > 0))

    expect_true(all(gam(runif(100))>0))
    expect_true(all(gamm1(runif(100))>0))
    expect_true(all(gam_ur(runif(100))>0))
    
    sol(Inf)
    expect_silent(dot3(r3vel(4,2),3))
    sol(1)

    a <- r4vel(10)
    expect_silent(a[1:4])
    expect_error(a[])
    expect_silent(a[1,1])
    expect_error(a[1:4] <- 0)
    expect_error(a[] <- 0)
    expect_error(a[] <- 1)


    a <- r3vel(10)
    expect_silent(a[1:4])
    expect_error(a[])
    expect_silent(a[1:4] <- 0)
    expect_silent(a[1,1])
    expect_error(a[] <- 0)
    expect_error(a[] <- 1)

    

    
}    
    

})
