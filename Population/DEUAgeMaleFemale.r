#!usr/bin/env Rscript

require(graphics)
library(XML)
library(reshape2)
library(ggplot2)
library(plyr)
#
# Diagram with population of male and female of Germany
#

setwd("~/git/R-Example")
source("common/rki_download.r")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

options(scipen=10)

png("png/DEPopulationAge.png",width=1920,height=1080)

germany <- read.table(file("data/DEPopulationAge.csv"), sep=";", header=TRUE)


plot( germany$age
    , germany$female + germany$male
    , type = "l"
    , col = "black"
    , ylim = limbounds(germany$female + germany$male)/2
    , xlab = "Age [years]"
    , ylab = "Number"
    , main = "DEU female + male by age"
    , sub = "Deadline: 31.12.2019"
    , lwd=2
    )

grid()

#polygon(c(data$age,rev(data$age)),c(data$female,rev(data$male)),col="yellow",border="red",lwd=2)

# lines(data$age,data$male,type="l",col="blue",lwd=2)

# legend("bottom", inset=0.05, legend=c("Male","Female"),col=c("blue","red"),lty=1)

dev.off()

