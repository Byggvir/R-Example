#!usr/bin/env Rscript

require(graphics)

#
# Diagram with population of male and female of Germany
#

setwd("~/git/R-Example")
options(scipen=10)

png("png/DEPopulationAge.png",width=1920,height=1080)

data <- read.table(file("data/DEPopulationAge.csv"), sep=";", header=TRUE)

plot( data$age
    , data$female
    , type="l"
    , col="red"
    , ylim=c(200000,800000)
    , xlab="Age [years]"
    , ylab="Number"
    , main="DEU female/male by age"
    , sub="Deadline: 31.12.20190" 
    , lwd=2
    )

#polygon(c(data$age,rev(data$age)),c(data$female,rev(data$male)),col="yellow",border="red",lwd=2)

lines(data$age,data$male,type="l",col="blue",lwd=2)

legend("bottom", inset=0.05, legend=c("Male","Female"),col=c("blue","red"),lty=1)

dev.off()
