library(gyrogroup)


s <- 0.4
u <- as.3vel(c(s,0,0))  # first leg;  green
v <- as.3vel(c(s/3,-s/2,0)) + as.3vel(c(s/5,s,0)) * seq(from=0,to=5,len=30)

seg <- function(u,start=as.3vel(0), ...){
  start <- start[,1:2]
  u <- u[,1:2]   # now a two-column matrix
  
  for(i in seq_len(nrow(u))){
    if(i==7 | nrow(u)==1){jj <- 4 } else {jj <- 1}
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
plot(c(0,0),c(-1,1),xlim=c(0,1),ylim=c(-0.3,1),type='n',asp=1)
seg(u,start=0*u,col='green')
seg(u+v,start=u+v*0,col='black')
seg((u+v)-u,start=u+v,col='red')
seg(((u+v)-u)-v,start=(u+v)-u,col='blue')
dev.off()

pdf(file="associator.pdf")
plot(c(0,0),c(-1,1),xlim=c(0,1),ylim=c(-0.3,1),type='n',asp=1)
seg(u,start=0*u,col='green')
seg(u+v,start=u+v*0,col='black')
seg((u+v)-u,start=u+v,col='red')
seg(((u+v)-u)-v,start=(u+v)-u,col='blue')
dev.off()

#pdf(file="commutator2.pdf")
#plot(c(0,0),c(-1,1),xlim=c(0,1),ylim=c(-0.3,1),type='n',asp=1)
#seg(u+v,u+(v*0))


