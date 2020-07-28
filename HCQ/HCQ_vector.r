png("HCQ.png",width=1920,height=1080)


x<-30:100

M<-c(63.7,68.1,63.2,63.3,63.3)
S<-c(16.5,18.9,15.6,17.3,15.9)

color<-c("black","red","green","blue","yellow")


plot(x,0*x+100/70,type="l", col="grey", asp=10,xlab="Age [yr]", ylab="[%]",
    main="Treatment with Hydroxychloroquine, Azithromycin, and Combination in Patients Hospitalized with COVID-19",
    sub="Approximated age distribution by normal distribution" )
    
polygon(c(x,rev(x)),c(dnorm(x,M[2],S[2])*100,rev(dnorm(x,M[3],S[3]))*100),col="grey")

for (n in 2:3)
{
    lines(x,dnorm(x,M[n],S[n])*100,col=color[n],lty=1,lwd=2)
}
abline(v=65,col="blue",lwd=2,lty=2);text(66,0.5,"Age > 65\n61,4% Neither med\n48.9% HCQ Alone",adj=0);
abline(v=71,col="blue",lwd=2,lty=2);text(72,0.5,"Age > 71\n50% Neither med",adj=0);
abline(v=83,col="blue",lwd=2,lty=2);text(84,0.5,"Age > 65\n25% Neither med",adj=0);
legend("bottom",legend=c("Total","Neither Med","HCQ Alone","AZM Alone","HCQ + AZM"),col=color,lty=1,cex=2)
dev.off()
