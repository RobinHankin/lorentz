library("hexSticker")

pdf(file="lorentz_icon.pdf",bg="#7733FF")

n <- 2
plot(NA,xlim=c(0,2),ylim=c(0,2),xlab="",ylab="",axes=FALSE,asp=1)

arrows(-2,0,2,0,lwd=5) # x axis
arrows(0,-2,0,2,lwd=5) # t axis

theta <- 0.3
arrows(-2,-2*sin(theta),2,2*sin(theta),lty=2,lwd=5) # x' axis
arrows(-2*sin(theta),-2,2*sin(theta),2,lty=2,lwd=5) # t' axis

arrows(1.9,1.9*sin(theta),2,2*sin(theta),lty=1,lwd=5) # x' axis
arrows(1.9*sin(theta),1.9,2*sin(theta),2,lty=1,lwd=5) # t' axis

segments(-2,-2,-2,2,lwd=5) # leftward-moving null
segments(-2,-2,3,3,lwd=5) # rightward-moving null

x <- seq(from=-1,to=1.9,len=250)
y <- sqrt(1+x^2)

points(x,y,type='l')

points(y,x,type='l')
dev.off()

sticker("lorentz_icon.pdf", package="lorentz", p_size=28, s_x=1, s_y=1.02,
s_width=0.9,asp=sqrt(3)/2, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="lorentz.png")

