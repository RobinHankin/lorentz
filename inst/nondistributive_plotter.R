# plotting functions called right at the end of this file

library("gyrogroup")

`seg` <- function(u,start=as.3vel(0), bold=5, ...){
  start <- start[,1:2]
  u <- u[,1:2]   # now a two-column matrix
  
  for(i in seq_len(nrow(u))){
    if(i==bold | nrow(u)==1){
      lwd <- 4
      length <- 0.3
    } else {
      lwd <- 1
      length <- 0.1
    }
    arrows(
        x0=start[i,1],
        y0=start[i,2],
        x1=u[i,1],
        y1=u[i,2],
        angle=10,
        length=length,
        lwd=lwd,
        ...
    )
  }
}

`comm_fail1` <- function(u,v){
  plot(NA,xlim=c(0,1),ylim=c(-0.2,1),type='n',asp=1,main="Failure of the parallelogram law",xlab='',ylab='',axes=FALSE)
  seg(u,start=0*u,col='purple')
  seg(u+v,start=u+v*0,col='black')
  seg((u+v)-u,start=u+v,col='red')
  seg(((u+v)-u)-v,start=(u+v)-u,col='blue')
  points(((u+v)-u)-v,pch=16,col='blue')
  legend("topright",lty=c(1,1,1,1,2),col=c("purple","black","red","blue","green"),
         legend=c("leg 1","leg 2","leg 3","leg 4", "c=1"))
  points(0,0,pch=16)

  theta <- seq(from=0,to=2*pi,len=100)
  points(sin(theta),cos(theta),type='l',lty=2,col='green')
}

`comm_fail2` <- function(u,v){
  plot(NA,xlim=c(-0.2,0.9),ylim=c(-0.2,1),type='n',asp=1,main="Failure of the parallelogram law",xlab='',ylab='',axes=FALSE)
  seg(u,start=0*u,col='black')
  seg(u+v,start=u+v*0,col='blue')

  seg(v,start=0*v,col='blue')
  seg(v+u,start=v,col='black')

  seg(u+v,start=v+u,col='red',code=0)

  legend("topright",lty=c(1,1,1,2),col=c("black","blue","red","green"),legend=c("u","v","mismatch","c=1"))
  
  points(0,0,pch=16)

  theta <- seq(from=0,to=2*pi,len=100)
  points(sin(theta),cos(theta),type='l',lty=2,col='green')
}

`ass_fail` <- function(u,v,w,bold){
  plot(c(0,0),c(-0.1,1),xlim=c(0,1),ylim=c(-0.3,1),type='n',asp=1,pty='m',xlab='',ylab='',axes=FALSE,main='Failure of associative property')
  seg(u,start=v*0,bold=bold,col='black')
  seg(u+(v+w),start=v*0+u,bold=bold,col='black')

  seg(u+v,start=v*0,bold=bold,col='blue')
  seg((u+v)+w,start=u+v,bold=bold,col='blue')
  
  seg(u+(v+w),start=(u+v)+w,bold=bold,col='red',code=0)
  legend("topright",lty=c(1,1,1,2),col=c("black","blue","red","green"),legend=c("u+(v+w)","(u+v)+w","mismatch","c=1"))

  points(0,0,pch=16,cex=3)
  theta <- seq(from=0,to=2*pi,len=100)
  points(sin(theta),cos(theta),type='l',lty=2,col='green')

}


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
