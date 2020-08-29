#!/usr/bin/env Rscript

library(data.table)

data <- as.matrix(readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 3))
dpop <- read.table(file("data/DEPopulationAge.csv"), sep=";", header=TRUE)

population <- sum(dpop$both)
age59 <- sum(dpop$both[1:60])
age80 <- sum(dpop$both[81:86])

png("png/DEFalleAlterKw.png", width = 3840, height = 2160)

par(mar=c(5,5,8,5), mfrow = c(2,5))

plot(
   data[,1]
   , apply(data[,8:12], 1, FUN=sum) / apply(data[,2:12], 1, FUN=sum) *100
   , type = "b"
   , col = "black"
   , xlab = "Kalenderwoche"
   , ylab = "%"
   , ylim = c(0,40)
   , main = "Anteil Altersgruppe 60+ an wöchentlichen COVID-19 Fällen"
   , cex.main = 2
   , sub = "Quelle: Fallzahlen rki.de; Bevölkerung: destatis.de"
   )

abline (
  h = (1 - age59/population)*100
  , col = "blue"
  )

options (digits = 3)

text( 30, 
      (1 - age59/population)*100 +0.5 , 
      paste("Anteil an Bevölkerung [",round((1 - age59/population)*100,1),"%]")
      )

legend(
  "topright"
  , legend = "Altersgruppe 60+"
  , col = "black"
  , lty = 1
  , cex = 5
  )

grid()

for (a in 2:9) {
   l <- ((a-2)*10+1)
   u <- l+9
   calter <- sum(dpop$both[l:u])
   print(calter)
   
   plot(
      data[,1]
      , data[,a] / apply(data[,2:12], 1, FUN=sum) *100
      , type = "b"
      , col = "black"
      , xlab = "Kalenderwoche"
      , ylab = "%"
      , ylim = c(0,40)
      , main = paste("Anteil Altersgruppe", l-1, "-", u-1 ," an wöchentlichen COVID-19 Fällen")
      , cex.main = 2
      , sub = "Quelle: Fallzahlen rki.de; Bevölkerung: destatis.de"
   )
   
   abline (
      h = calter/population*100
      , col = "blue"
   )
   
   options (digits = 3)
   
   text( 30
         , calter/population*100 +0.5
         , paste("Anteil an Bevölkerung [",round(calter/population*100,1),"%]")
         , pos = 4 )
   
   legend(
      "topright"
      , legend = paste("Altersgruppe", l-1,"-",u-1)
      , col = "black"
      , lty = 1
      , cex = 5
   )
   
   grid()
   
   
}

plot(
   data[,1]
   , apply(data[,10:12], 1, FUN=sum) / apply(data[,2:12], 1, FUN=sum) *100
   , type = "b"
   , col = "black"
   , xlab = "Kalenderwoche"
   , ylab = "%"
   , ylim = c(0,40)
   , main = "Anteil Altersgruppe 80+ an wöchentlichen COVID-19 Fällen"
   , cex.main = 2
   , sub = "Quelle: Fallzahlen rki.de; Bevölkerung: destatis.de"
)

abline (
   h = age80/population*100
   , col = "blue"
)

options (digits = 3)

text( 30, 
      age80/population*100 +0.5 , 
      paste("Anteil an Bevölkerung [",round(age80/population*100,1),"%]")
)

legend(
   "topright"
   , legend = "Altersgruppe 80+"
   , col = "black"
   , lty = 1
   , cex = 5
)

grid()

dev.off()

