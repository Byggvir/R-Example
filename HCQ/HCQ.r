png("test.png",width=1920,height=1080)

x<-30:100
a<-dnorm(x,63.7,16.5)*100
b<-dnorm(x,68.1,18.9)*100
c<-dnorm(x,63.2,15.6)*100
d<-dnorm(x,63.3,17.3)*100
e<-dnorm(x,62.3,15.9)*100


plot(x,a,type="l", col="black", asp=10,xlab="Age [yr]", ylab="[%]",
    main="Treatment with Hydroxychloroquine, Azithromycin, and Combination in Patients Hospitalized with COVID-19",
    sub="Approximated age distribution by normal distribution")
lines(x,b,col="red",lty=1)
lines(x,c,col="green",lty=1)
lines(x,d,col="blue",lty=1)
lines(x,e,col="yellow",lty=1)

legend(35,2,legend=c("Total","Neither Med","HCQ Alone","AZM Alone","HCQ + AZM"),col=c("black","red","green","blue","yellow"),lty=1)
dev.off()
