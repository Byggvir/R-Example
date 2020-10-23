#!usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_RegressionAnalysis"

require(data.table)
library(REST)

setwd("~/git/R-Example")

source("common/rki_download.r")
source("lib/copyright.r")
source("common/ta_regressionanalysis.r")
source("common/rki_sql.r")

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
    t1.date as Date
    , (@i:=@i+1) as Day
    , WEEK(t1.date,3) as Kw
    , dayofweek(t1.date) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases-t2.cases as incCases
    , t1.deaths-t2.deaths as incDeaths
from rki as t1 
inner join rki as t2 
on t1.date=adddate(t2.date,1)
where t1.cases > t2.cases;
'

# Function execute a regression analysis 

CI <- 0.80

regression_analysis <- function (
  StartDate
  , EndDate
  , StartRegADate
  , EndRegADate
  , PrognoseDate
  , data 
) {
  
  zr <- data$Date >= StartRegADate & data$Date <= EndRegADate
  FromTo <- data$Date[zr] - StartRegADate
  
  ra <- lm(log(data$incCases[zr]) ~ FromTo)
  ci <- confint(ra,level = CI)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])
  
  xlim <- c(StartRegADate,PrognoseDate)
  # ylim <- c(0,30000)
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
    , ( data$incCases[zr] / (exp(a[2] + b[2] * as.numeric(FromTo)) )
    )
  )
  
  colnames(K) <- c("WTag","Anteil")
  
  Kor <- aggregate(Anteil ~ WTag, FUN = mean, data = K)
  
  PrognoseTab <- data.table (
    Date = (Tage + StartRegADate)
    , WTag = wday(Tage + StartRegADate)
    , assumed = round(exp(a[2] + b[2] * Tage)
    * Kor[wday(Tage + StartRegADate),2])
    , lower = round(exp(a[1] + b[1] * Tage)
    * Kor[wday(Tage + StartRegADate),2])
    , upper = round(exp(a[3] + b[3] * Tage)
    * Kor[wday(Tage + StartRegADate),2])
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
  
  plot(data$Date[zr]
       , data$incCases[zr]
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
    main = paste( "Tägliche Fälle DE mit exponentieller Prognose bis", as.character(PrognoseDate)) 
    , cex.main = 4
  )
  copyright(c("RKI","TA"))
  
  zr <- data$Date >= StartRegADate & data$Date <= EndRegADate
  
  lines ( data$Date[zr]
          , data$incCases[zr]
          , col = "black"
          , lwd = 3
  )
  
  lines ( PrognoseTab$Date
          , PrognoseTab$assumed
          , col = "blue"
          , lwd = 3
          , lty = 4
  )
  
  zr <- data$Date >= EndRegADate
  
  lines ( data$Date[zr]
          , data$incCases[zr]
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
         , cex = 3
         , col = "blue"
  )
  
  text ( EndRegADate + 1
         , 0
         , "Prognose"
         , adj = 0
         , cex = 3 
         , col = "blue"
  )
  text ( EndRegADate + 1
         , 0
         , "Prognose"
         , adj = 0
         , cex = 3 
         , col = "blue"
  )
  text ( EndRegADate + 1
         , ylim[2]/2
         , as.character(EndRegADate)
         , adj = 0
         , cex = 3 
         , col = "blue"
  )
  
  grid()
  
  plotregression(a, b, xlim= c(0, as.numeric(PrognoseDate - StartRegADate)), ylim = ylim )
  
  lr <- ifelse (b[2] > 0 ,"topleft", "topright")
  
  legend ( lr
           , legend = c( 
             "Fälle innerhalb der RA"
             , "Fälle nach der RA"
             , paste("Obere Grenze CI ",  CI * 100, "%", sep="")
             , "Mittelere exp. Regression"
             , paste("Untere Grenze CI ",  CI * 100, "%", sep="")
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
  
  # Add labels to pronosed dates
  
  for (x in EndRegADate:PrognoseDate) {
    
    
    # regression_label(
    #   x - as.numeric(StartRegADate)
    #   , a
    #   , b
    #   , xlim = c(0, as.numericPrognoseDate - StartRegADate)
    #   , ylim = ylim)
    
  }

  legend(
    "top"
    , inset = 0.02
    , title = paste( "Tägliche Steigerung CI  ",  CI * 100, "%", sep="")
    , legend = c( 
          paste(round((exp(ci[2,1])-1)*100,1),"% / R7 =",round((exp(7*ci[2,1])),1))
        , paste(round((exp(ra$coefficients[2])-1)*100,1),"% / R7 =", round((exp(7*ra$coefficients[2])),1))
        , paste(round((exp(ci[2,2])-1)*100,1),"% / R7 =",round((exp(7*ci[2,2])),1)))
    , col = c(
        "green"
      , "orange"
      , "red"
    )
    , lty = 3 
    , lwd = 3
    , cex = 3)
  
  dev.off()
  
}


rkidata <- sqlGetRKI(SQL )

eDate <- rkidata$Date[length(rkidata$Date)]

for (j in c(0)) {
for (i in c(27,34)) {
  
  regression_analysis (
      StartDate = rkidata$Date[1]
    , EndDate = eDate
    , StartRegADate <- eDate - i - j
    , EndRegADate <- eDate - j
    , PrognoseDate = as.Date("2020-11-01")
    , data = rkidata

)

}
}
