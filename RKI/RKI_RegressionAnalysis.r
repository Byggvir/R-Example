#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2021-02-22
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "RKI_RegressionAnalysis"

require(data.table)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)

setwd("~/git/R-Example")

source("lib/copyright.r")
source("common/ta_regressionanalysis.r")
source("common/rki_sql.r")

Wochentage <- c("So","Mo","Di","Mi","Do","Fr","Sa")

LookBack <- 20

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  FromUntil <- c( Sys.Date() - LookBack - 1, Sys.Date() - 1 )
  
} else if (length(args) == 1) {
  tempD <- as.Date(args[1])
  FromUntil <- c( tempD - LookBack, tempD )
  
} else if (length(args) >= 2){
  FromUntil <- sort(c(as.Date(args[1]),as.Date(args[2])))
}

print(FromUntil)
today <- Sys.Date()
heute <- format(today, "%d %b %Y")
                     
options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

SQL <- '
select 
    Meldedatum as Meldedatum
    , (@i:=@i+1) as Day
    , week(Meldedatum,3) as Kw
    , dayofweek(Meldedatum) as WTag
    , sum(AnzahlFall) as AnzahlFall
from RKIFaelle
group by Meldedatum;
'
SQLWTag <- "select weekday(Meldedatum) as WTag, sum(AnzahlFall) / (select sum(AnzahlFall) /7  from RKIFaelle) as AnteilWoche from RKIFaelle group by WTag;"

Kor <- sqlGetRKI(SQLWTag)  

rkidata <- sqlGetRKI(SQL )

# Function execute a regression analysis 

CI <- 0.95

regression_analysis <- function (
  StartDate
  , EndDate
  , StartRegADate
  , EndRegADate
  , PrognoseDate
  , data 
) {
  
  zr <- data$Meldedatum >= StartRegADate & data$Meldedatum <= EndRegADate
  FromTo <- data$Meldedatum[zr] - StartRegADate
  
  ra <- lm(log(data$AnzahlFall[zr]) ~ FromTo)
  ci <- confint(ra,level = CI)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])
  
  xlim <- c(StartRegADate,PrognoseDate)
  ylim <- c(  0
              , ( max(
                c(   exp(a)
                     , exp(a+b*as.numeric(PrognoseDate-StartRegADate))
                )
              ) %/% 1000 + 1 ) * 1000
  )
  
  Tage <- as.numeric(StartRegADate:PrognoseDate)-as.numeric(StartRegADate)

  K <-cbind(
    data$WTag[zr]
    , ( data$AnzahlFall[zr] / (exp(a[2] + b[2] * as.numeric(FromTo)) )
    )
  )
  
  colnames(K) <- c("WTag","Anteil")
  
  PrognoseTab <- data.table (
    Date = (Tage + StartRegADate)
    , WTag = Wochentage[wday(Tage + StartRegADate, week_start = 1)]
    , assumed = round(exp(a[2] + b[2] * Tage)
    * Kor[wday(Tage + StartRegADate, week_start = 1),2])
    , lower = round(exp(a[1] + b[1] * Tage)
    * Kor[wday(Tage + StartRegADate, week_start = 1),2])
    , upper = round(exp(a[3] + b[3] * Tage)
    * Kor[wday(Tage + StartRegADate, week_start = 1),2])
  )

  print(PrognoseTab)

  png( paste( "png/RKI_Prognose"
              , "_"
              , as.character(StartRegADate)
              , "_"
              , as.character(EndRegADate)
              , "_"
              , as.character(PrognoseDate)
              , ".png"
              , sep = ""
  )
  , width = 1920
  , height = 1080
  )

  par (   mar = c(10,5,10,5) 
          , bg = "lightyellow")
  
  plot(data$Meldedatum[zr]
       , data$AnzahlFall[zr]
       , main = ""
       , sub = paste("Vom", StartRegADate, "bis", EndRegADate )
       , xlab = "Datum"
       , ylab = "Anzahl"
       , ylim = ylim
       , type = "l"
       , lwd = 3
       , xlim = xlim
       , col = "black"

  )
  
  
  t <- title ( 
    main = paste( "Daily cases DEU and exponential prognose until", as.character(PrognoseDate)) 
    , cex.main = 4
  )
  copyright(c("RKI","TA"))
  
  zr <- data$Meldedatum >= StartRegADate & data$Meldedatum <= EndRegADate
  
  lines ( data$Meldedatum[zr]
          , data$AnzahlFall[zr]
          , col = "black"
          , lwd = 3
  )
  
  lines ( PrognoseTab$Date
          , PrognoseTab$assumed
          , col = "blue"
          , lwd = 3
          , lty = 4
  )
  
  zr <- data$Meldedatum >= EndRegADate
  
  lines ( data$Meldedatum[zr]
          , data$AnzahlFall[zr]
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
         , "Regressionsanalyse"
         , adj = 1 
         , cex = 2
         , col = "blue"
  )
  
  text ( EndRegADate + 1
         , 0
         , "Prognose"
         , adj = 0
         , cex = 2 
         , col = "blue"
  )
  
  text ( EndRegADate + 1
         , ylim[2]/2
         , as.character(EndRegADate)
         , adj = 0
         , cex = 3 
         , col = "blue"
         , las = 3

  )

  grid()
  
  plotregression(a, b, xlim= c(0, as.numeric(PrognoseDate - StartRegADate)), ylim = ylim )
  
  lr <- ifelse (b[2] > 0 ,"topleft", "topright")
  
  legend ( lr
           , legend = c( 
             "Cases inside timeframe of RA"
             , "Cases outside timeframe of RA"
             , "model cases"
             , paste("Upper limit CI ",  CI * 100, "%", sep="")
             , "Mean"
             , paste("Lower limit CI ",  CI * 100, "%", sep="")
           )
           , col = c(
             "black"
             , "gray"
             , "blue"
             , "red"
             , "orange"
             , "green"
           )
           , lwd = 2
           , cex = 2
           , inset = 0.05
           , lty = c(1,1,4,3,3,3)
  )
  
  legend(
    "top"
    , inset = 0.02
    , title = paste( "Tägliche Steigerung CI  ",  CI * 100, "%", sep="")
    , legend = c( 
          paste(round((exp(ci[2,1])-1)*100,2),"% / R =",round((exp(4*ci[2,1])),2))
        , paste(round((exp(ra$coefficients[2])-1)*100,2),"% / R =", round((exp(4*ra$coefficients[2])),2))
        , paste(round((exp(ci[2,2])-1)*100,2),"% / R =",round((exp(4*ci[2,2])),2)))
    , col = c(
        "green"
      , "orange"
      , "red"
    )
    , lty = 3 
    , lwd = 3
    , cex = 3)

  # vp <- viewport(
  #       x = 0.2
  #     , y = 0.45
  #     , width = 0.5
  #     , height = 0.6
  #     )
  # tt <- ttheme_default(
  #   base_size = 12
  #   , core=list(
  #     fg_params=list(   hjust = 1
  #                     , x = 0.95
  #     )
  #   )
  # )
  # 
  # g <- tableGrob(
  #   PrognoseTab[PrognoseTab$Date >= EndRegADate,]
  #   , theme = tt
  #   , cols = c("Datum", "WTag", "Mean", "Min\nCI95%","Max\nCI95%" )
  #   , vp = vp
  # )
  # 
  # grid.draw(g
  #           , )
  
  
  dev.off()
  
}

eDate <- rkidata$Meldedatum[length(rkidata$Meldedatum)-1]

for (k in c(0,7,14)) {
for (j in c(14)) {
for (i in c(20,34)) {
    
  regression_analysis (
      StartDate = rkidata$Meldedatum[1]
    , EndDate = eDate
    , StartRegADate <- eDate - i - k
    , EndRegADate <- eDate - k
    , PrognoseDate = eDate + j
    , data = rkidata

)

} # End for i
} # End for j
} # End for k

# eDate <- rkidata$Meldedatum[length(rkidata$Meldedatum)]
# regression_analysis (
#   StartDate = rkidata$Meldedatum[1]
#   , EndDate = eDate
#   , StartRegADate <- FromUntil[1]
#   , EndRegADate <- FromUntil[2]
#   , PrognoseDate = today + 14
#   , data = rkidata
# )
# 
