#!usr/bin/env Rscript

require(data.table)
library(REST)
library(ggpubr)

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

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)
SQL <- "select 
    t1.day as Date
    , WEEK(t1.day,1) as Kw
    , WEEKDAY(t1.day) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases-t2.cases as incCases
    , t1.deaths-t2.deaths as incDeaths
from rki as t1 
inner join rki as t2 
on t1.day=adddate(t2.day,1);"

#daily <- get_rki_sql(sql=SQL) # Leider hǵibt es in der RMariaDB library eine Fehler

daily <- get_rki_kumtab()

m <- length(daily[,1])

zr <- 130:m
z <- length(zr)


ra <- lm(log(daily$incCases[zr]) ~ daily$Date[zr])
a <- ra$coefficients[1]
b <- ra$coefficients[2]
ci <- confint(ra)
ylim <- c(0,(exp(a+b*as.numeric(enddate))%/%1000+1)*1000)

# print(summary(ra))

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
  , ylim = ylim
  , type = "l"
  , lwd = 3
  , xlim = c(daily$Date[zr[1]],as.Date("2020-12-31"))
)

t <- title ( 
    main = "Tägliche Fälle DE mit exponentieller Prognose "
  , cex.main = 4
  )

grid()

par (new = TRUE)

curve( exp(a+b*x)
       , from = as.numeric(daily$Date[zr[1]])
     # , to = as.numeric(daily$Date[zr[z]])
       , to = as.numeric(as.Date("2020-12-31"))
       
       , col="blue"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , ylim = ylim
       , lwd = 3
)

par ( new = TRUE )

a <- ci[1,2]
b <- ci[2,1]

curve( exp(a+b*x)
       , from = as.numeric(daily$Date[zr[1]])
       # , to = as.numeric(daily$Date[zr[z]])
       , to = as.numeric(as.Date("2020-12-31"))
       
       , col="red"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , ylim = ylim
       , lwd = 3
)

par ( new = TRUE )

a <- ci[1,1]
b <- ci[2,2]

curve( exp(a+b*x)
       , from = as.numeric(daily$Date[zr[1]])
       # , to = as.numeric(daily$Date[zr[z]])
       , to = as.numeric(as.Date("2020-12-31"))
       
       , col="red"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , ylim = ylim
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
             , "blue"
             )
         , lwd = 2
         , cex = 3
         , inset = 0.02
)

DailyPercentageIncrease <- (exp(ra$coefficients[2])-1)*100

legend( 
  "bottomright"
  , inset = 0.02
  , title = "Tägliche Steigerung CI 95%"
  , paste(round((exp(ci[2,1])-1)*100,1),"% <", round(DailyPercentageIncrease,2),"% <",round((exp(ci[2,2])-1)*100,1),"%") 
  , cex = 3)

grid()

dev.off()
