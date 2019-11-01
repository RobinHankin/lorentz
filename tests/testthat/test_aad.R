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
    expect_silent(orthog(rboost(0.5)))
    sol(1)

    expect_error(!vel_to_4mom(r3vel(5)))
    expect_error(!as.4mom(r4vel(1)))

    a <- r3vel(10)
    expect_silent(as.4vel(a))
    expect_silent(as.matrix(as.4vel(a)))
    expect_silent(as.4vel(c(1,0,0,0)))
    expect_silent(as.4vel(0))
    expect_error(as.4vel(0.2))
    expect_error(as.4vel(2))
    expect_error(as.4vel(function(x){x^2}))
    expect_error(as.4vel(rbind(1:4)))
    expect_error(as.4vel(1:4))
    expect_error(as.4vel(matrix(1:9,3,3)))
    expect_error(as.4vel(matrix(1:25,5,5)))

    a <- r3vel(10)
    expect_silent(a[1:4])
    expect_silent(a[])
    expect_silent(a[1:4] <- 0)
    expect_silent(a[1,1])
    expect_error(a[] <- 0)
    expect_error(a[2] <- 3)
    expect_silent(a[1:2] <- a[1])
    expect_silent(a[,2] <- 0)
    expect_silent(a[,2] <- 0.0001)
    expect_error(a[,2] <- 10)
    expect_silent(a[1,2] <- 10)

    a <- r4vel(10)
    expect_silent(a[1:4])
    expect_silent(a[])
    expect_silent(a[1,1])

    expect_error(a[] <- 0)
    expect_error(a[] <- 1)
    expect_silent(a[1] <- 0)
    expect_error(a[1] <- 1)

    expect_error(a[,2] <- 0)
    expect_error(a[,2] <- 0.0001)
    expect_error(a[,2] <- 10)


    expect_silent(a[4:6] <- a[1])
    expect_silent(a[4:6] <- 0)
    expect_error(a[4:6] <- 100)

    expect_error(a[5] <- function(x){x^2})
    expect_error(a[5] <- 1:2)
    
    expect_silent(a[2,3] <- 0)

    a <- r3vel(11)
    b <- r3vel(11)
    expect_true(all(a == a))
    expect_true(all(a != b))
    

    a <- r4vel(11)
    expect_error(+a)
    expect_error(-a)
    expect_error(a+a)
    expect_error(a-a)
    expect_error(a*2)

    a <- r3vel(11)
    expect_silent(+a)
    expect_silent(-a)
    expect_error(!a)
    expect_error(a&a)
    expect_silent(a*2)
    expect_error(2/a)
    expect_silent(a/2)
    expect_silent(a*2)
    expect_silent(2*a)
    expect_error(a*a)

    expect_silent(is.consistent.4vel(r4vel(4),give=TRUE))

    expect_silent(gyr.a(a,a,a))

    sol(Inf)
    expect_true(is.consistent.boost(rboost(1)))
    expect_silent(is.consistent.boost(rboost(1),give=TRUE))

    expect_silent(pureboost(rboost(0.5)))
    expect_silent(pureboost.galilean(rboost(1)))
    expect_silent(pureboost.galilean(rboost(1),tidy=TRUE))
    expect_silent(orthog.galilean(rboost(1)))

    sol(1)
    expect_true(is.consistent.boost(rboost(.1)))
    expect_silent(is.consistent.boost(rboost(.1),give=TRUE))

        
    u <- as.3vel(c(0.4,0,0))
    v <- seq(as.3vel(c(0.4,-0.2,0)), as.3vel(c(-0.3,0.9,0)),len=20)
    w <- as.3vel(c(0.8,-0.4,0))
    
    expect_silent(comm_fail1(u=u, v=v))
    expect_silent(comm_fail2(u=u, v=v))
    expect_silent(ass_fail(u=u, v=v, w=w, bold=10))

    rot(u,w)
    rot(u,w,space=FALSE)

    expect_silent(pureboost(rboost(0.5)))
    expect_silent(pureboost(rboost(0.5),include_sol=FALSE))
    expect_silent(orthog(rboost(0.5)))

    perfectfluid(1,1,0)
    dust(1)
    photongas(1)

    transform_uu(dust(1),rboost(0.4))
    transform_ud(dust(1),rboost(0.4))
    transform_dd(dust(1),rboost(0.4))

    raise(dust(1))
    lower(dust(1))

    ptm(TRUE,TRUE)
    ptm(TRUE,FALSE)
    ptm(FALSE,TRUE)
    ptm(FALSE,FALSE)

  }
  
    

})
