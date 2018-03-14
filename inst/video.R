## This creates a sequence of pdf diagrams suitable for creating a movie

library("gyrogroup")


sss <- seq(from=1,to=2,len=100)
for(i in seq_along(sss)){
  filename <- paste("~/temp/file",sprintf("%03d",i),".pdf",sep="")

  sol(sss[i])
  u <- as.3vel(c(0.4,0,0))
  v <- seq(as.3vel(c(0.4,-0.2,0)), as.3vel(c(-0.3,0.9,0)),len=20)
  w <- as.3vel(c(0.8,-0.4,0))
  pdf(file=filename)
  comm_fail1(u=u, v=v,bold=17)
  dev.off()
}
