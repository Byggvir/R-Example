#!usr/bin/env Rscript

require(data.table)
library(REST)
# library(ggpubr)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("common/ta_regressionanalysis.r")

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

runingaverage <- function () {
  
  cases <-sum(daily$Cases[length(daily$Cases)])
  cf <- aggregate(incCases ~ WTag, FUN  = "sum", data = daily)
  cf$p <- cases/cf$incCases/7

  for (i in 1:7) {
    daily$incCases[daily$WTag == cf$WTag[i]] <- daily$incCases[daily$WTag == cf$WTag[i]] * cf$p[i]
  }
}

StartDate <- daily$Date[1]
#EndDate <- as.Date("2020-12-31")
EndDate <- daily$Date[length(daily$Date)] + 28

StartRegADate <- as.Date("2020-06-29")
EndRegADate <- max(daily$Date)

zr <- daily$Date >= StartRegADate & daily$Date <= EndRegADate
FromTo <- daily$Date[zr] - StartRegADate

ra <- lm(log(daily$incCases[zr]) ~ FromTo)
ci <- confint(ra,level = 0.95)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])

xlim <- c(StartRegADate,EndDate)
ylim <- c(  0
            , ( max(
              c(   exp(a)
                   , exp(a+b*as.numeric(EndDate-StartRegADate))
              ) 
            ) %/% 1000 + 1 ) * 1000
)

png( paste( "png/RKI_Prognose"
            , "_"
            , as.character(StartRegADate)
            , "_"
            , as.character(EndRegADate)
            , "_"
            , as.character(EndDate)
            , ".png"
            )
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
  , col = "black"
)

t <- title ( 
    main = paste( "Tägliche Fälle DE mit exponentieller Prognose bis", as.character(EndDate)) 
  , cex.main = 4
  )

zr <- daily$Date >= StartRegADate & daily$Date <= EndRegADate

lines ( daily$Date[zr]
        , daily$incCases[zr]
        , col = "black"
        , lwd = 3
)

zr <- daily$Date >= EndRegADate

lines ( daily$Date[zr]
        , daily$incCases[zr]
        , col = "gray"
        , lwd = 3
)

abline(
  v = EndRegADate
  , col = "blue"
  , lwd = 3
  , lty = 4
)

text ( EndRegADate -1
       , 0
       , "Zeitraum der Regressionsanalyse"
       , adj = 1 
       , cex = 3
       , col = "blue"
)

text ( EndRegADate + 1
       , 0
       , "Zeitraum der Prognose"
       , adj = 0
       , cex = 3 
       , col = "blue"
)
grid()

plotregression(a, b, xlim= c(0, as.numeric(EndDate - StartRegADate)), ylim = ylim )

lr <- ifelse (b[2] > 0 ,"topleft", "topright")

legend ( lr
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
         , inset = 0.05
         , lty = c(1,1,3,3,3)
)

legend( 
  "top"
  , inset = 0.02
  , title = "Tägliche Steigerung CI 95%"
  , paste(round((exp(ci[2,1])-1)*100,1),"% <", round((exp(ra$coefficients[2])-1)*100,2),"% <",round((exp(ci[2,2])-1)*100,1),"%") 
  , cex = 3)

dev.off()
