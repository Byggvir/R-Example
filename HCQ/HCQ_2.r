# Title: Treatment with HCQ example
# Autor: Thomas Arend
# Rheinbach, Germany

png("HCQ_NeitherMed.png",width=1920,height=1080)

x<-30:120
n_a<-dnorm(x,63.7,16.5)*100
n_b<-dnorm(x,68.1,18.9)*100
n_c<-dnorm(x,63.2,15.6)*100
n_d<-dnorm(x,63.3,17.3)*100
n_e<-dnorm(x,62.3,15.9)*100

M<-c(63.7,68.1,63.2,63.3,63.3)
S<-c(16.5,18.9,15.6,17.3,15.9)

plot(x,c,type="l",
     col="blue",lwd=2,
     xlab="Age [years]",ylab="Share",
     main="Treatment with HCQ",
     sub="Comparison of the age of the patients" )
     
polygon(c(x,rev(x)),c(n_c,rev(dnorm(x,68.1,18.9))),col="yellow",border="blue")

lines(x,dnorm(x,68.1,18.9),type="l",col="red", lty=1,lwd=2)

abline(v=83); text(90,0.002,"Age > 83")

legend("topright", inset=0.05,
       legend=c("HCQ alone\nMean,SD=63.2,15.6\n", "Neither med\nMean,SD=68.1,18.9\n"),
       title="Treatment groups",
       col=c("blue", "red"), lty=1, cex=0.8)

dev.off()
