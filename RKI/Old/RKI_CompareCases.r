#!/usr/bin/env Rscript
#
#
# Script: RKI_PrognoseWeek.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_CompareCases"

require(data.table)
library(REST)
library(gridExtra)
library(grid)
setwd("~/git/R-Example")

#source("common/rki_download.r")
source("common/rki_sql.r")
source("common/ta_regressionanalysis.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

# Auswertung insgesamt

LookBack <- 42

ShowFrom <- today - LookBack -42
ShowUntil <- today -42 

dFrom <- as.character(ShowFrom)
dUntil <-as.character(ShowUntil)

SQL = paste('call CompareCasesPerDay("', dFrom,'","',dUntil,'"); ', sep="")

CaseTab <- sqlGetRKI(SQL = SQL)

png( filename = paste("png/", MyScriptName, "-", dFrom,"-", dUntil, ".png", sep="")
     , width = 1980
     , height = 1080
)
par ( mar = c(15,6,10,6))   


mycol <- c("black",rainbow(3)
)

ylim <- limbounds(CaseTab[,2:5])

plot (CaseTab[,1]
      , CaseTab[,2]
      , type = "l"
      , col = mycol[1]
      , lwd = 4
      , main = "Vergleich der Fallzahlen"
      , sub = "Tägliche Fallzahlen nach Datum"
      , xlab = ""
      , ylab = "Anzahl kumuliert"
      , ylim = ylim
      , cex.main = 3
      , cex.sub = 2
      , cex.lab = 1
      )

for (l in 2:5) {
lines ( CaseTab[,1]
        , CaseTab[,l]
        , type = "l"
        , col = mycol[l-1]
        , lwd = 3
        , lty = 3
        )
        
}

legend( "bottomright"
        , title = "Fallzahlen"
        , legend = c( "Eingang beim RKI"
                   , "Meldedatum (Gesundheitsamt)"
                   , "Referenzdatum"
                   , "Fallzahl JHU")
        , col = mycol
        , lty = 1
        , cex = 1
)

grid()

vp <- viewport(x=0.25,y=0.7,width=0.5,height = 0.4)

tt <- ttheme_default(
  base_size = 12
  , core=list(
    fg_params=list(fontface=c("plain")
    )
  )
)

g <- tableGrob(
  CaseTab[as.numeric(CaseTab[,1]) > as.numeric(ShowUntil-14),]
  , theme = tt
  , cols = c("Tag", "Tägl. Meldung RKI","Meldedatum", "Referenzdatum","Tägl. Meldung JHU" )
  , vp = vp      
)

grid.draw(g
          , )
copyright(c("RKI","JHU","TAr"))
dev.off()

