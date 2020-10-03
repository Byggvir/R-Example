#!usr/bin/env Rscript

require(data.table)
library(REST)

setwd("~/git/R-Example")
source("common/rki_download.r")


png("png/RKI_regression.png"
    , width = 1920
    , height = 1080
    )
par ( mar = c(10,5,10,5))

# par( mfcol = c(1,2) )

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

daily <- get_rki_kumtab()

m <- length(daily[,1])

zr <- 2:29
z <- length(zr)

pylim <- c(min(daily$incCases[zr]),max(daily$incCases[zr]))

ra1 <- lm(daily$incCases[zr] ~ daily$Date[zr])

ra2 <- lm(log(daily$incCases[zr]) ~ daily$Date[zr])

print(summary(ra1))
print(summary(ra2))


a <- ra2$coefficients[1]
b <- ra2$coefficients[2]

plot(daily$Date[zr], daily$incCases[zr]
  , main = ""
  , sub = paste("Vom", daily$Date[zr[1]], "bis", daily$Date[zr[z]] )
  , xlab = "Datum"
  , ylab = "Täglichen Fälle"
  , ylim = pylim
  , type = "l"
  , lwd = 3
  )

title ( 
    main = "Tägliche Fälle DE"
  , cex.main = 4)

grid()

abline( ra1
  , col="red"
  , lwd = 3
)

par (new = TRUE)

curve( exp(a+b*x)
  , from = as.numeric(daily$Date[zr[1]])
  , to = as.numeric(daily$Date[zr[z]])
  , col="green"
  , axes = FALSE
  , xlab = ""
  , ylab = ""
  , ylim = pylim
  , lwd = 3
  )

legend ( "top"
         , legend = c("Fälle", "Lineare Regression", "Exponentielle Regression")
         , col = c("black", "red", "green")
         , lwd = 2
         , cex = 3
         )
         
dev.off()

