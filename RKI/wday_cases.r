#!/usr/bin/env Rscript

#
# Auswertung der Fallzahlen und Tote nach den Meldungen der John Hopkins University
# Quelle: https://github.com/CSSEGISandData/COVID-19
#

cases <- read.csv("../data/Germany.csv")

png("Wochentag.png",width=1920,height=1080)

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

cases$UpDown <- c(0, sign(cases$incCases[2:l]-cases$incCases[1:l-1]))

UpDown <- table(cases$WTag[cases$Kw>9],cases$UpDown[cases$Kw>9])

colnames(UpDown, do.NULL = FALSE)
colnames(UpDown) <- c("Down","Up")

par(mfcol=c(1,2))

plot( 0:6
    , UpDown[1:7,"Down"]
    , type="b"
    , col=c("green")
    , ylim=c(0,22)
    , xaxt="n"
    , xlab="Wochentag"
    , ylab="Anzahl"
    , main="Änderungen gegenüber Vortag (Deutschland)"
    , sub="Tageswert höher / niedirger als Vortag, seit Kalenderwoche 10"
    )
    
lines( 0:6
    , UpDown[1:7,"Up"]
    , type="b"
    , col=c("red")
    )

axis(1
    , at= 0:6
    , labels=ShortDayNames
    )

legend( "topright"
    , legend=c("Höher als Vortag", "Niedriger als Vortag")
    , col=c("red", "green")
    , lty=1
    )

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
    , main="Erkrankungen und Todesfälle (Deutschland)"
    , sub="Verteilt auf Wochentage; Quelle: https://github.com/CSSEGISandData/COVID-19"
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

options(digits=3)

print(c(WTag$incCases/sum(WTag$incCases),WTag$incDeaths/sum(WTag$incDeaths)))

dev.off()
