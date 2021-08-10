library("lorentz")
library("hexSticker")
library("pdftools")

u <- as.3vel(c(0.4,0,0))
v <- seq(as.3vel(c(0.4,-0.2,0)), as.3vel(c(-0.3,0.9,0)),len=20)
w <- as.3vel(c(0.8,-0.4,0))

`comm_fail_sticker` <-
function (u, v, bold = 5, r = 1) 
{
    plot(NA, xlim = c(0, 0.3), ylim = c(-0.2, 0.2), type = "n", asp = 1, 
        xlab = "", ylab = "", axes = FALSE)
    my_seg(u, start = 0 * u, col = "black", bold = bold)
    my_seg(u + v, start = u + v * 0, col = "black", bold = bold)
    my_seg((u + v) - u, start = u + v, col = "black", bold = bold)
    my_seg(((u + v) - u) - v, start = (u + v) - u, col = "black", 
        bold = bold)
    points(((u + v) - u) - v, pch = 16, col = "black")
    points(0, 0, pch = 16)
    theta <- seq(from = 0, to = 2 * pi, len = 100)
    points(r * sin(theta), cos(r * theta), type = "l", lty = 2, 
        col = "black")
}

png(file="lorentz_sticker_icon.png",bg="transparent")
comm_fail_sticker(u=u, v=v)
dev.off()

sticker("lorentz_sticker_icon.png", package="lorentz", p_size=8, s_x=0.9, s_y=0.95,
s_width=1.1, asp=0.85, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="lorentz.png")



