#!/usr/bin/env Rscript

library(data.table)

CFR <- read.csv("../data/DE_CFR.csv")

print (CFR)
png("DE_fatality.png", width = 1920, height = 1080)

par(mar=c(10,10,10,10))

plot( CFR$Age
    , CFR$Male
    , type = "b"
    , col = "blue"
    , lwd = 3
    , xlab = "Altersgruppe"
    , ylab = "Todesfälle"
    , xaxt = "n"
    , ylim = c(0,2500)
    , main="Todesfälle COVID-19 in Deutschland"
    , cex.main = 4
    , cex.lab = 3
    )


lines( CFR$Age
      , CFR$Female
      , type="b"
      , col = "red"
      , lwd = 3
      , ylim=c(0,2500)
      
)

axis( side=1
     , at = seq(0,100,10)
     , labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")
     , cex = 2
)

title(sub="Quelle RKI, Altersverteilung 32. Kw",line = 5)

legend( "topleft"
        , legend = c("Männer","Frauen")
        , col = c("blue","red")
        , lty = 1
        , lwd = 3
        , cex = 5
        )

grid()
dev.off()
