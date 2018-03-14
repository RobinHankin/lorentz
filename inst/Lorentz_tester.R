## This takes the Lorentz() function with c!=1 through its paces.


library(gyrogroup)


jj1 <- c(.1,.2,.3)
jj2 <- c(.2,.2123,-.3)
jj3 <- c(.2,.523,.3)


## c=1
sol(1)
u <- as.3vel(jj1)
v <- as.3vel(jj2)
w <- as.3vel(jj3)


u1 <- Lorentz(u)
m1 <- Lorentz(u) %*% Lorentz(v)


## now multiply the speed of light by 100 and multiply the velocities
## by 100.  So this is like changing from m/s to cm/s.

fac <- 100
sol(fac)
u <- as.3vel(jj1*fac)
v <- as.3vel(jj2*fac)
w <- as.3vel(jj3*fac)
u2 <- Lorentz(u)
m2 <- Lorentz(u) %*% Lorentz(v)


u1-u2 # should be zero (nontrivial)
m1-m2 # ditto
