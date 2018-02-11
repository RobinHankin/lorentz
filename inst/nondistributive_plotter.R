# plotting functions called right at the end of this file

library("gyrogroup")


# plot commands start

pdf(file="comm_fail1.pdf")
  comm_fail1(
    u =  as.3vel(c(0.4,0,0)),
    v = as.3vel(c(0.4,-0.2,0)) + as.3vel(c(-0.4,0.6,0)) * seq(from=0,to=3,len=28)
)

pdf(file="comm_fail2.pdf")
comm_fail2(
    u =  as.3vel(c(0.4,0,0)),
    v = as.3vel(c(0.4,-0.2,0)) + as.3vel(c(-0.4,0.6,0)) * seq(from=0,to=3,len=28)
)
dev.off()

pdf(file="ass_fail.pdf")
ass_fail(
    u = as.3vel(c(0.5,0,0)),
    v = as.3vel(c(0.4,-0.2,0)) + as.3vel(c(-0.4,0.3,0)) * seq(from=0,to=3,len=28),
    w = as.3vel(c(0.8,-0.4,0)),
    bold=23
    )
dev.off()
