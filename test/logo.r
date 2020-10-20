require(png)

logo <- readPNG("~/git/My-SVG-library/Wappen/MeinWappen-v2.1k.png")
myplot <- plot( 1:6
          , 6:1
          , xlim = c(1,6)
          , ylim = c(1,6)
)

w <- length(logo[1,,1])
h <- length(logo[,1,1])
print(c(w,h))

rasterImage(logo,xleft = 1, xright = 2, ybottom = 1, ytop = 1 + h/w)
