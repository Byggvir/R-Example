#!usr/bin/env Rscript
#
#
# Script: JHU_RegressionAnalysisKw.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"JHU_RegressionAnalysisKw"


require(data.table)
library(REST)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("common/ta_regressionanalysis.r")
source("lib/copyright.r")

SQL <- c( paste(
'select'
, '(@i:=@i+1) as Woche'
, ', min(t2.reported) as Date'
, ', ( case when (t2.reported< "2021-01-04") then week(t2.reported,3) else 53 + week(t2.reported,3) end) as Kw'
, ', count(*) as Tage'
, ', sum(t2.cases-t3.cases) as Cases'
, ', sum(t2.deaths-t3.deaths) as Deaths'
, ', min(t2.cases-t3.cases) as WMin'
, ', max(t2.cases-t3.cases) as WMax' 
, 'from jhucountry as t1'
, 'inner join jhu as t2'
, 'on t1.cid = t2.cid'
, 'inner join jhu as t3'
, 'on t2.cid = t3.cid and t2.reported=adddate(t3.reported,1)'
, 'where t1.cid ='
, sep = ' ')
,'group by Kw;' ) 

CI <- 0.95

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

options(
  digits = 7
  ,   scipen = 7
  ,   Outdec = "."
  ,   max.print = 3000
)

# Einlesen der Daten aus den Fällen der JHU

countries <- get_rki_sql(
  "select jhucountry.* from jhucountry join jhu on jhucountry.cid = jhu.cid group by jhu.cid having max(jhu.cases) >= 100000;" 
)

for (country in as.integer(countries[,1]) ) {
  
kw <- get_rki_sql(sql = paste(SQL[1],country,SQL[2]), prepare = "set @i:=1")

StartKw <- kw$Kw[1]
EndKw <- kw$Kw[length(kw$Kw)]
PrognoseKw <- EndKw + 4

StartRegAKw <- EndKw - 8

m <- max(kw$Kw[kw$Tage == 7])
print(m)

EndRegAKw <- 57
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
      "png/JHU/JHU_RegressionKw"
      , countries[country,3]
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
    , main = country
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
  
  # --- Anfang Lineare Regesssion
  
  
  # ra2 <- lm(kw$Cases[zr]/7 ~ FromTo)
  # ci2 <- confint(ra2,level = CI )
  #
  # a <- c( ci2[1,1], ra2$coefficients[1] , ci2[1,2])
  # b <- c( ci2[2,1], ra2$coefficients[2] , ci2[2,2])
  #
  # print(a)
  #
  # linecol = c("green","orange","red")
  #
  # for ( i in 1:3 ) {
  #   par ( new = TRUE )
  #   curve(  a[i] + b[i] * x
  #       , from = 0
  #       , to = PrognoseKw - StartRegAKw
  #       , col = linecol[i]
  #       , xlab = ""
  #       , ylab = ""
  #       , xlim = c(0,PrognoseKw - StartRegAKw)
  #       , ylim = ylim
  #       , lwd = 3
  #   )
  # }
  
  copyright()
  
  dev.off()
  
}

