library("lorentz")
library("sphereplot")



u <- as.3vel(c(0,0,0.8))
v <- r3vel(3000,0.9)
w <- as.3vel(c(0,0.8,0))

n1 <- (u+v)-(v+u)            # noncommutativity
n2 <- (u+(v+w)) - ((u+v)+w)  # nonassociativity



rgl.sphgrid()
rgl.sphpoints(car2sph(v))

rgl.sphgrid()
rgl.sphpoints(car2sph(n1))

rgl.sphgrid()
rgl.sphpoints(car2sph(n2))
