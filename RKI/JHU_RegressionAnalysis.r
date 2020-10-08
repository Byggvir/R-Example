#!usr/bin/env Rscript

require(data.table)
library(REST)

setwd("~/git/R-Example")
source("common/rki_download.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
startdate <- as.Date("2020-02-24")
enddate <- as.Date("2020-12-31")
                     
png(  "png/RKI_regression.png"
    , width = 1920
    , height = 1080
    )
par ( mar = c(10,5,10,5))

par( mfcol = c(1,2) )

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

daily <- get_rki_kumtab()

m <- length(daily[,1])
# while( is.na(daily$Cases[m])) { m <- m-1}

zr <- 130:m
z <- length(zr)

pylim <- c(min(daily$incCases[zr]),max(daily$incCases[zr]))
pylim <- c(0,(max(daily$incCases[zr])%/%1000+1)*1000)


ra <- lm(log(daily$incCases[zr]) ~ daily$Date[zr])

print(summary(ra))
# fehler <- confint(ra)

Tage <- data.frame(
   nr = 1:(enddate-today)
)

Tage$Date <- as.Date(Tage$nr+today-1)

plot(daily$Date[zr]
  , daily$incCases[zr]
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

par (new = TRUE)

a <- ra$coefficients[1]
b <- ra$coefficients[2]

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
         , legend = c( 
               "Fälle"
#            , "Lineare Regression" 
             , "Exponentielle Regression"
             )
         , col = c(
               "black"
#            , "red"
             , "green"
             )
         , lwd = 2
         , cex = 3
)

pylim <- c(0,(exp(a+b*as.numeric(enddate))%/%1000+1)*1000)

plot( Tage$Date
     , rep(0,1,enddate-today)
     , main = ""
     , sub = paste("Vom", as.character(today), "bis", as.character(enddate) )
     , xlab = "Datum"
     , ylab = "Täglichen Fälle"
     , ylim = pylim
     , type = "l"
     , lwd = 0
#     , xaxt = "n"

)

grid()

title ( 
  main = "Prognose tägliche Fälle DE"
  , cex.main = 4)

a <- ra$coefficients[1]
b <- ra$coefficients[2]

par( new = TRUE )
curve(  exp(a+b*x)
        , from = as.numeric(today)
        , to = as.numeric(enddate)
        , col="green"
        , axes = FALSE
        , xlab = ""
        , ylab = ""
        , ylim = c(0,exp(a+b*as.numeric(enddate)))
        , lwd = 3
)

# print(daily)

dev.off()