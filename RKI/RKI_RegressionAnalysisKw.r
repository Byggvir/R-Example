#!usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysisKw.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_RegressionAnalysisKw"


require(data.table)
library(REST)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("common/rki_sql.r")
source("common/ta_regressionanalysis.r")
source("lib/copyright.r")

SQL <- "
select 
    (@i:=@i+1) as Woche
    , min(t1.date) as Date
    , week(t1.date,3) as Kw
    , count(week(t1.date,3)) as Tage
    , sum(t1.cases-t2.cases) as Cases
    , sum(t1.deaths-t2.deaths) as Deaths
    , min(t1.cases-t2.cases) as WMin
    , max(t1.cases-t2.cases) as WMax 
    from rki as t1
    inner join rki as t2 
    on t1.date=adddate(t2.date,1)
    group by week(t1.date,3);
"

CI <- 0.95

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

options(
  digits = 7
  ,   scipen = 7
  ,   Outdec = "."
  ,   max.print = 3000
)

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI

kw <- get_rki_sql(sql = SQL, prepare = "set @i:=1")
daily <- get_rki_sql()

regression_analysis_kw <- function (
  StartKw
  , EndKw
  , StartRegAKw
  , EndRegAKw
  , PrognoseKw
  , data 
) {
  
  zr <- kw$Kw >= StartRegAKw & kw$Tage == 7 & kw$Kw <= EndRegAKw
  
  FromTo <- kw$Kw[zr] - StartRegAKw
  
  ra1 <- lm(log(kw$Cases[zr] / 7) ~ FromTo)
  ci1 <- confint(ra1, level = CI)
  
  a <- c(ci1[1, 1], ra1$coefficients[1] , ci1[1, 2])
  b <- c(ci1[2, 1], ra1$coefficients[2] , ci1[2, 2])
  
  xlim <- c(StartRegAKw, PrognoseKw)
  ylim <- c(0
            , ( max(
                  c(
                      exp(a)
                    , exp(a + b * (PrognoseKw - StartRegAKw))
                  )
                  ) %/% 1000 + 1 ) * 1000 
            )
  
  png(
    paste(
      "png/RKI_RegressionKw"
      , StartRegAKw
      , "-"
      , EndRegAKw
      , "-"
      , PrognoseKw
      , ".png"
      , sep = ""
    )
    , width = 1920
    , height = 1080
  )
  
  par(
    mar = c(10, 5, 10, 5)
    )
  
  plot(
    kw$Kw[zr]
    , kw$Cases[zr] / kw$Tage[zr]
    , main = ""
    , sub = paste(
      "Regession auf der Basis der Fallzahlen des RKI von der "
      ,
      StartRegAKw
      ,
      ". bis zur "
      ,
      EndRegAKw
      ,
      ". Kw 2020"
      ,
      sep = ""
    )
    , xlab = "Woche"
    , ylab = "Fallzahl pro Tag"
    , xlim = xlim
    , ylim = ylim
    , type = "l"
    , lwd = 3
  )
  
  zr2 <- kw$Kw >= EndRegAKw & kw$Kw <= PrognoseKw
  
  lines (kw$Kw[zr2]
         , kw$Cases[zr2] / kw$Tage[zr2]
         , col = "gray"
         , lwd = 3)
  
  zr3 <- kw$Kw >= StartRegAKw & kw$Kw <= PrognoseKw
  
  lines (kw$Kw[zr3]
         , kw$WMin[zr3]
         , col = "green"
         , lwd = 3)
  
  lines (kw$Kw[zr3]
         , kw$WMax[zr3]
         , col = "red"
         , lwd = 3)
  
  abline(
    v = EndRegAKw
    , col = "blue"
    , lwd = 3
    , lty = 4
  )
  
  text (
    EndRegAKw - 0.1
    , 0
    , "Regressionsanalyse"
    , adj = 1
    , cex = 2
    , col = "blue"
  )
  
  text (
    EndRegAKw + 0.1
    , 0
    , "Prognose"
    , adj = 0
    , cex = 2
    ,
    col = "blue"
  )
  
  t <- title (
    main = paste(
      "Mittlere CoViD-19 Fälle je Tag nach Kalenderwoche mit Prognose bis Kw",
      PrognoseKw
    )
    , cex.main = 3
  )
  
  grid()
  
  plotregression(a,
                 b,
                 xlim = c(0, PrognoseKw - StartRegAKw),
                 ylim = ylim)
  
  lr <- ifelse (b[2] > 0 , "left", "right")
  
  legend (
    lr
    , legend = c(
      "Mittlere Fallzahl pro Tag in der Kw"
      ,
      "... nach Regressionsanalyse"
      ,
      "Maximale Fallzahl in Kw"
      ,
      "Minimale Fallzahl in Kw"
      ,
      paste("Obere Grenze CI ", CI * 100, "%", sep = "")
      ,
      "Schätzung durchscnittliche Fallzahl pro Tag"
      ,
      paste("Untere Grenze CI  ", CI * 100, "%", sep = "")
    )
    , col = c("black"
            , "gray"
            , "red"
            , "green"
            , "red"
            , "orange"
            , "green")
    , lwd = 2
    , cex = 1.5
    , inset = 0.02
    , lty = c(1, 1, 1, 1, 3, 3, 3)
    
  )
  
  legend(
    "top"
    , inset = 0.02
    , title = paste("Wöchentliche Steigerung CI ", CI * 100, "%", sep = "")
    , paste( round( (exp(b[1]) - 1) * 100, 1)
            , "% <"
            , round( (exp(b[2]) - 1) * 100)
            , "% <"
            , round( (exp(b[3]) - 1) * 100, 1)
            , "%")
    , cex = 3
  )
  
  copyright()
  
  dev.off()
}  

m <- max(kw$Kw[kw$Tage == 7])
         
regression_analysis_kw(
    StartKw = kw$Kw[1]
  , EndKw = kw$Kw[length(kw$Kw)]
  , StartRegAKw = kw$Kw[length(kw$Kw)] - 3
  , EndRegAKw = m
  , PrognoseKw <- m + 2
)
