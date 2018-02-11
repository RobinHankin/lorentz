# plotting functions called right at the end of this file

library("gyrogroup")


# following three-velocities closely match those of comm_fail.Rd:
jj <- as.3vel(c(0.4,-0.2,0))
kk <- as.3vel(c(-0.4,0.6,0))
kk1 <- as.3vel(c(-0.4,0.3,0))

u <- as.3vel(c(0.5,0,0))
v <- jj + kk*seq(from=0,to=3,len=28)
w <- as.3vel(c(0.8,-0.4,0))

pdf(file="comm_fail1.pdf")
comm_fail1(u=u, v=v)
dev.off()

pdf(file="comm_fail2.pdf")
comm_fail2(u=u, v=v)
dev.off()

pdf(file="ass_fail.pdf")
ass_fail(
    u=u, 
    v = jj + kk1 * seq(from=0,to=3,len=28),
    w = w, bold=23
)
dev.off()

