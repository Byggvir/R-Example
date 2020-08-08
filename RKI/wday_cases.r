#!/usr/bin/env Rscript

# Load statistics from RKI test report
# Source https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/2020-08-05-de.pdf?__blob=publicationFile
#
# 

cases <- read.csv("../data/Germany.csv")

png("Wochentag.png")

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
cases$incDeaths <- c(0, cases$Deaths[2:l]-cases$Deaths[1:l-1])

WTag <- aggregate(cbind(incCases,incDeaths)~WTag, FUN=sum, data=cases)

plot( 
      0:6
    , WTag$incCases/sum(WTag$incCases)*100
    , type="b"
    , col="blue"
    , xaxt="n"
    , ylim= c(0,30)
    , xlab="Wochentag"
    , ylab="Fälle pro Tag [%]"
    , main="Anzahl der Fälle nach Wochentag"
    )
lines( 
      0:6
    , WTag$incDeaths/sum(WTag$incDeaths)*100
    , type="b"
    , col="black"
    )
axis(1
    , at= 0:6
    , labels=ShortDayNames
    )
legend( "topright"
    , legend=c("Erkrankte","Gestorbene")
    , col=c("blue", "black")
    , lty=1
    )
    
dev.off()
