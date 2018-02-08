library(gyrogroup)
sol(1)

seg <- function(u,start=as.3vel(0), ...){
  start <- start[,1:2]
  u <- u[,1:2]   # now a two-column matrix
  
  for(i in seq_len(nrow(u))){
    if(i==5 | nrow(u)==1){jj <- 4} else {jj <- 1}
    arrows(
        x0=start[i,1],
        y0=start[i,2],
        x1=u[i,1],
        y1=u[i,2],
        angle=10,
        length=0.1,
        lwd=jj,
        ...
    )
  }
}

pdf(file="commutator.pdf")

plot(NA,xlim=c(0,1),ylim=c(-0.7,1),type='n',asp=1,main="Failure of the parallelogram law",xlab='x',ylab='y')
seg(u,start=0*u,col='green')
seg(u+v,start=u+v*0,col='black')
seg((u+v)-u,start=u+v,col='red')
seg(((u+v)-u)-v,start=(u+v)-u,col='blue')
points(((u+v)-u)-v,pch=16,col='blue')
legend("topleft",lty=1,col=c("green","black","red","blue"),legend=c("leg 1","leg 2","leg 3","leg 4"))
points(0,0,pch=16)
sol(1)
dev.off()




s <- 1
u <- as.3vel(c(s*0.8,0,0))
v <- as.3vel(c(-s/30,-s*0.2,0)) + as.3vel(c(s/5,s*0.6,0)) * seq(from=0,to=3,len=20)
w <- as.3vel(c(-s*0.8,s*0.2,0))

pdf(file="associator.pdf")
plot(c(0,0),c(-1,1),xlim=c(0,1),ylim=c(-0.3,1),type='n',asp=1)
seg(u+v,start=v*0,col='black')
seg((u+v)+w,start=u+v,col='black')

seg(u,start=v*0,col='blue')
seg(u+(v+w),start=v*0+u,col='blue')

seg(u+(v+w),start=(u+v)+w,col='red',code=0)
dev.off()

#pdf(file="commutator2.pdf")
#plot(c(0,0),c(-1,1),xlim=c(0,1),ylim=c(-0.3,1),type='n',asp=1)
#seg(u+v,u+(v*0))


