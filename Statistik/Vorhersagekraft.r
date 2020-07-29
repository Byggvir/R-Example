# Predictive power

png(file="Vorhersagekraft.png",width=1920,height=1080)

p <- seq(from=0.000,to=0.005,by=0.0001)

f <- c(0.0001,0.0002,0.0005,0.001,0.002,0.005,0.01,0.02,0.05)

plot (  p * 100
      , p
      , type="n"
      , ylim=c(0,100)
      , xlab="Prävalenz [%]"
      , ylab="Vorhersagekraft [%]"
      , main="Vorhersagekraft eines positiven Tests"
      , sub="Vorhersagekraft nach Prävalenz und Testfehler"
    )
grid()
for (i in 1:length(f)) {
   
    v <- p*(1-f[i]) /((1-f[i])*p+(1-p)*f[i])
    lines( p * 100
         , v * 100
         , col=i
         , type="l"
         )
}

legend( "bottomright"
      , legend=paste(f*100," %")
      , title="Testfehler"
      , col=1:length(f)
      , lty=rep(1,length(f))
      , bg=rgb(224/256,224/256,192/256)
      , cex=2
      )

abline(h=90,lty=3)
abline(h=95,lty=3)    
abline(h=99,lty=3)
abline(v=0.25,lty=2)

dev.off()
