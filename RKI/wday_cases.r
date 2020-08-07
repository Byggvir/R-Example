#!/usr/bin/env Rscript

# Load statistics from RKI test report
# Source https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/2020-08-05-de.pdf?__blob=publicationFile
#
# 

cases <- read.csv("../data/Germany.csv")

ShortDayNames <- c(
      "Mo"
    , "Di"
    , "Mi"
    , "Do"
    , "Fr"
    , "Sa"
    , "So"
    )

l <- length(cases$Kw)

cases$incCases <- c(0, cases$Cases[2:l]-cases$Cases[1:l-1])
cases$incDeath <- c(0, cases$Deaths[2:l]-cases$Deaths[1:l-1])

WTag <- data.frame ( 
      Tag=0:6
    , Name=ShortDayNames
    , Cases=rep(0,7)
    )


for ( i in 0:6 ) {
  WTag$Cases[WTag$Tag == i] <- sum(cases$incCases[cases$WTag == i])
}

plot( 
    WTag$Name
    , WTag$Cases
    , type="b")
