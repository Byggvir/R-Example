#!usr/bin/env Rscript
#
#
# Script: JHU_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"JHU_RegressionAnalysis"


require(data.table)
library(REST)
library(stringr)

setwd("~/git/R-Example")

source("common/rki_download.r")
source("common/ta_regressionanalysis.r")
source("lib/averages.r")
source("lib/myfunctions.r")

SQL <- '
select
    t2.reported as Date
    , (@i:=@i+1) as Day
    , WEEK(t2.reported,3) as Kw
    , WEEKDAY(t2.reported) as WTag
    , t2.cases as Cases
    , t2.deaths as Deaths
    , t2.cases-t3.cases as incCases
    , t2.deaths-t3.deaths as incDeaths
from jhucountry as t1 
inner join jhu as t2 
  on t1.cid = t2.cid
inner join jhu as t3 
  on t2.cid = t3.cid
  and
  t2.reported=adddate(t3.reported,1)
where t2.cases-t3.cases > 0 and t1.cid = 
'

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
                     
options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

# Function do a regression analysis 

CI <- 0.95

regression_analysis <- function (
  StartDate
  , EndDate
  , StartRegADate
  , EndRegADate
  , PrognoseDate
  , data
  , cname = "Germany"
) {
  
  print(c(StartRegADate,EndRegADate))
  
  zr <- data$Date >= StartRegADate & data$Date <= EndRegADate
  FromTo <- data$Date[zr] - StartRegADate
  
  smooth <- exponential_smoothing(data$incCases,g=1/4)
  
  ra <- lm(log(smooth[zr]) ~ FromTo)
  ci <- confint(ra,level = CI)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])
  
  xlim <- c(StartRegADate,PrognoseDate)

  ylim <- limbounds( 
             c(  exp(a)
               , exp(a+b*as.numeric(PrognoseDate-StartRegADate))
               , data$incCases[zr]
                )
                )

  pngname <- str_replace (
    paste( "JHU_Prognose"
                    , "_"
                    , cname
                    , "_"
                    , as.character(StartRegADate)
                    , "_"
                    , as.character(EndRegADate)
                    , "_"
                    , as.character(PrognoseDate)
                    , ".png"
                    , sep = ""
  )
  , '\\/'
  , '_'
  )
  
  png( paste("png",pngname, sep="/")
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
       , ylab = "Täglichen Fälle"
       , ylim = ylim
       , type = "l"
       , lwd = 3
       , xlim = xlim
       , col = "black"
  )

  zr <- data$Date >= StartRegADate & data$Date <= EndDate
  
  lines(data$Date[zr]
        , smooth[zr]
        , type = "l"
        , lwd = 5
        , col = "orange"
  )
  
  t <- title ( 
    main = paste( cname ,": Exp. gegl. tägl. CoViD-19-Fälle",  "; Prognose bis", as.character(PrognoseDate)) 
    , cex.main = 3
  )
  copyright(holders=c("JHU","TAr"))

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
         , "RA"
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
             , "Fälle geglättet"
             , paste("Obere Grenze CI ",  CI * 100, "%", sep="")
             , "Mittelere exp. Regression"
             , paste("Untere Grenze CI ",  CI * 100, "%", sep="")
           )
           , col = c(
             "black"
             , "gray"
             , "orange"
             , "red"
             , "orange"
             , "green"
           )
           , lwd = 2
           , cex = 2
           , inset = 0.05
           , lty = c(1,1,1,3,3,3)
  )
  
  legend(
    "top"
    , inset = 0.02
    , title = paste( "Tägliche Steigerung CI  ",  CI * 100, "%", sep="")
    , legend = c( 
          paste(round((exp(ci[2,1])-1)*100,1),"%")
        , paste(round((exp(ra$coefficients[2])-1)*100,2),"%")
        , paste(round((exp(ci[2,2])-1)*100,1),"%"))
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


countries <- get_rki_sql(
  "select jhucountry.* from jhucountry join jhu on jhucountry.cid = jhu.cid group by jhu.cid having max(jhu.cases) >= 1000000;" 
)

for (country in as.integer(countries[,1]) ) {

  rkidata <- get_rki_sql(
      paste(SQL , country , ";" )
      )

  # Bestimmen des Regressionszeitraumes.
  
  tempcases <- exponential_smoothing(rev(rkidata$incCases[rkidata$Date>=as.Date("2020-06-29")& rkidata$incCases > 0]))
  minIdx <- which.min( tempcases)
  maxIdx <- which.max( tempcases)

  tempdate <- rev(rkidata$Date[rkidata$Date>=as.Date("2020-06-29") & rkidata$incCases > 0 ])

  if (maxIdx > minIdx ) { 
    RegFrom <- tempdate[maxIdx]
    RegTo   <- tempdate[minIdx]
  }
  else {
    RegFrom <- tempdate[minIdx]
    RegTo   <- tempdate[maxIdx]
  }

  try (
    regression_analysis (
        StartDate = rkidata$Date[1]
      , EndDate = as.Date("2020-12-31") # rkidata$Date[length(rkidata$Date)]
      , StartRegADate <- today - 120 # RegFrom
      , EndRegADate <- as.Date("2020-11-30") # rkidata$Date[length(rkidata$Date)]  # RegTo
      , PrognoseDate = as.Date("2020-12-31")
      , data = rkidata
      , cname = countries[countries[,1]==country,3]
    )
  )

}

