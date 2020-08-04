# Predictive power

png(file="Vorhersagekraft.png",width=1920,height=1080)

f <- c(0.0001,0.0002,0.0005,0.001,0.002,0.005,0.01,0.02,0.05)

vk <- function (x,f) {

  return( x*(1-f) / ( (1-f)*x + (1-x)*f ) )

}

xabs <- c(0,2)

plot (  NA
      , NA
      , type="n"
      , xlim=xabs
      , ylim=c(0,100)
      , xlab="Prävalenz [%]"
      , ylab="Vorhersagekraft [%]"
      , main="Vorhersagekraft eines positiven Tests"
      , sub="Vorhersagekraft nach Prävalenz und Testfehler"
    )
    
grid()

for (i in 1:length(f)) {

  par(new=TRUE) 
  curve(vk(x/100,f[i])*100,
         , xlim=xabs
         , ylim=c(0,100)
         , xaxt="n"
         , yaxt="n"
         , col=i
         , type="l"
         , xlab=""
         , ylab=""
         
         )


}


legend( "bottomright"
      , legend=paste(f*100," %")
      , title="Testfehler"
      , col=1:length(f)
      , lty=rep(1,length(f))
      , bg=rgb(224/256,224/256,192/256)
      , cex=1
      )

abline(h=90,lty=3)
abline(h=95,lty=3)    
abline(h=99,lty=3)
abline(v=0.25,lty=2)

dev.off()
