#!/usr/bin/env Rscript
#
#
# Script: RKI_PrognoseWeek.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_CompaseCases"

require(data.table)
library(REST)

setwd("~/git/R-Example")

source("common/rki_download.r")
source("common/rki_sql.r")
source("common/ta_regressionanalysis.r")
source("lib/copyright.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

# Auswertung insgesamt

SQL = paste('call CompareCases("', as.character(today -30),'","',as.character(today),'"); ', sep="")

CaseTab <- sqlGetRKI(SQL = SQL)
                     
print(CaseTab)

png( filename = paste("png/",MyScriptName,".png", sep="")
     , width = 1980
     , height = 1080
)

mycol <- c("black",rainbow(3)
)
plot (CaseTab[,1]
      , CaseTab[,2]
      , type = "l"
      , col = mycol[1]
      , lwd = 3
      )

for (l in 3:5) {
lines ( CaseTab[,1]
        , CaseTab[,l]
        , type = "l"
        , col = mycol[l-1]
        , lwd = 1
        )
        
}

legend( "bottomright"
        , title = "Fallzahlen"
        , legend = c( "RKI Eingang kumuliert"
                   , "RKI nach Meldedatum"
                   , "RKI nach Referenzdatum"
                   , "JHU nach Datum")
        , col = mycol
        , lty = 1
        , cex = 3
)
grid()

dev.off()
