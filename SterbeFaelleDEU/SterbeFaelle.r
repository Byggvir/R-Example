png("SterbeFaelle2018.png",width=1920,height=1080)

a<-0:100

Einwohner <- read.csv(file = '12411-0006.csv')
SterbeFaelle <- read.csv(file = 'SterbeFaelle2018.csv')

plot(2018-SterbeFaelle$age,SterbeFaelle$female/sum(SterbeFaelle$female),type="l",col="red")
lines(2018-SterbeFaelle$age,SterbeFaelle$male/sum(SterbeFaelle$male),type="l",col="blue")
lines(2018-Einwohner$age,Einwohner$female/sum(Einwohner$female),type="l",col="green")
lines(2018-Einwohner$age,Einwohner$male/sum(Einwohner$male),type="l",col="yellow")

dev.off()
