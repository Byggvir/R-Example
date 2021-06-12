#!usr/bin/env Rscript
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

require(data.table)
library(REST)
library(ggplot2)

setwd("~/git/R-Example")
MyScriptName <- "RKI_MeldendeLK"

source("lib/copyright.r")
source("lib/myfunctions.r")
source("common/rki_sql.r")

today <- Sys.Date()
heute <- format(today, "%Y-%m-%d")

par ( mar = c(10,10,10,10)
      , mfcol = c(2,1)
      )

png( paste( "png/", MyScriptName, heute, ".png", sep = "")
    , width = 1920
    , height = 1080)
par ( mar = c(10,10,10,10)
      , mfcol = c(2,1)
)

# ---- Erstes Diagramm

SQL <- paste(
  'SELECT Meldedatum,
    count(IdLandkreis) 
  FROM (
  SELECT IdLandkreis as IdLandkreis,
      max(Meldedatum) as Meldedatum
  FROM RKIFaelle as A
  GROUP BY IdLandkreis ) as B GROUP BY Meldedatum;'
    , sep = "")

LetzteMeldung <-sqlGetRKI(SQL = SQL)

# par ( new = TRUE)

bp2 <- barplot(LetzteMeldung[,2]
        , main = "Meldende Landkreise"
        , cex.main = 4
        , cex.axis = 2
        , cex.names = 2
        , sub = ""
        , names.arg = LetzteMeldung[,1]
        , col = "cyan"
        , las = 1
        , xlab = ""
        , ylab = ""
        , ylim = limbounds(LetzteMeldung[,2]) * 1.1
)

title ( sub = paste( "Datum letzte Fallmeldung des GA ans RKI")
        , cex.sub = 2
)

text( bp2
      , LetzteMeldung[,2] 
      , LetzteMeldung[,2]
      , cex = 2
      , adj = 0.5
      , pos = 3
      , offset = 0.2
      
)

mtext( paste("Datenbestand", as.character(today))
      , side = 4
      , outer = FALSE
      , line = 3
      , cex = 2
)
             
grid()

copyright()
 
dev.off()
