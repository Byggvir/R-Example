#!usr/bin/env Rscript

require(data.table)
library(REST)
# library(ggpubr)

setwd("~/git/R-Example")
source("common/rki_download.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
                     
options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI

daily <- get_rki_tag_csv()

StartDate <- daily$Date[1]
EndDate <- as.Date("2020-05-31")

StartRegADate <- as.Date("2020-04-01")
EndRegADate <- max(daily$Date)
EndRegADate <- as.Date("2020-04-30")

zr <- daily$Date >= StartRegADate & daily$Date <= EndRegADate
FromTo <- daily$Date[zr] - StartRegADate

ra <- lm(log(daily$incCases[zr]) ~ FromTo)
a <- ci[1,2]
b <- ci[2,1]

ylim <- c(  0
            , ( max(
              c(   exp(a)
                   , exp(a+b*as.numeric(EndDate-StartRegADate))
              ) 
            ) %/% 1000 + 1 ) * 1000
)


a <- ra$coefficients[1]
b <- ra$coefficients[2]
ci <- confint(ra)


xlim <- c(StartRegADate,EndDate)

png(  "png/RKI_PrognoseB.png"
      , width = 1920
      , height = 1080
)
par ( mar = c(10,5,10,5))

plot(daily$Date[zr]
  , daily$incCases[zr]
  , main = ""
  , sub = paste("Vom", StartRegADate, "bis", EndRegADate )
  , xlab = "Datum"
  , ylab = "Täglichen Fälle"
  , ylim = ylim
  , type = "l"
  , lwd = 3
  , xlim = xlim
)

t <- title ( 
    main = "Tägliche Fälle DE mit exponentieller Prognose "
  , cex.main = 4
  )

grid()
zr <- daily$Date >= EndRegADate
lines ( daily$Date[zr]
        , daily$incCases[zr]
        , col = "gray"
        , lwd = 3
)

par (new = TRUE)

curve( exp(a+b*x)
       , from = 0
       , to = as.numeric(EndDate - StartRegADate)
       
       , col="orange"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , xlim = c(0,EndDate - StartRegADate)
       , ylim = ylim
       , lwd = 3
)
text( as.numeric(EndDate-StartRegADate) 
     , exp(a+b*as.numeric(EndDate-StartRegADate))
     , round(exp(a+b*as.numeric(EndDate-StartRegADate)),0)
     , col = "orange"
     , adj = 0
     , cex = 1
     )

par ( new = TRUE )

a <- ci[1,2]
b <- ci[2,2]

curve( exp(a+b*x)
       , from = 0
       , to = as.numeric(EndDate - StartRegADate)
       , col="red"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , xlim = c(0,EndDate - StartRegADate)
       , ylim = ylim
       , lwd = 3
)
text( as.numeric(EndDate-StartRegADate) 
      , exp(a+b*as.numeric(EndDate-StartRegADate))
      , round(exp(a+b*as.numeric(EndDate-StartRegADate)),0)
      , col = "red"
      , adj = 0
      , cex = 1
)

par ( new = TRUE )

a <- ci[1,1]
b <- ci[2,1]

curve( exp(a+b*x)
       , from = 0
       , to = as.numeric(EndDate - StartRegADate)
       
       , col="green"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , xlim = c(0,EndDate - StartRegADate)
       , ylim = ylim
       , lwd = 3
)
text( as.numeric(EndDate-StartRegADate)
      , exp(a+b*as.numeric(EndDate-StartRegADate))
      , round(exp(a+b*as.numeric(EndDate-StartRegADate)),0)
      , col = "green"
      , adj = 0 
      , cex = 1
)

legend ( "bottomleft"
         , legend = c( 
           "Fälle innerhalb der RA"
           , "Fälle nach der RA"
           , "Obere Grenze CI 95%"
           , "Mittelere exp. Regression"
           , "Untere Grenze CI 95%"
         )
         , col = c(
           "black"
           , "gray"
           , "red"
           , "orange"
           , "green"
         )
         , lwd = 2
         , cex = 2
         , inset = 0.02
)

legend( 
  "top"
  , inset = 0.02
  , title = "Tägliche Steigerung CI 95%"
  , paste(round((exp(ci[2,1])-1)*100,1),"% <", round((exp(ra$coefficients[2])-1)*100,2),"% <",round((exp(ci[2,2])-1)*100,1),"%") 
  , cex = 3)

grid()

dev.off()
