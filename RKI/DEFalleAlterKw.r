#!/usr/bin/env Rscript

library(data.table)

data <- as.matrix(readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 3))
dpop <- read.table(file("data/DEPopulationAge.csv"), sep=";", header=TRUE)

AgeGroups <- c("60+","0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")

Kw <- max(data[,"Kw"])

population <- sum(dpop$both)

age59 <- sum(dpop$both[1:60])
age80 <- sum(dpop$both[81:86])

png("png/DEFalleAlterKw.png", width = 3840, height = 2160)

par(mar=c(5,5,8,5), mfrow = c(2,5))

single_plot <- function(x, AG, sumAG) {
   
   plot(
      data[,1]
      , x * 100
      , type = "b"
      , lwd = 3
      , col = "black"
      , xlab = "Kalenderwoche"
      , ylab = "%"
      , ylim = c(0,40)
      , main = paste("Anteil Altersgruppe", AG, "an wöchentlichen COVID-19 Fällen")
      , cex.main = 2
      , sub = "Quelle: Fallzahlen rki.de; Bevölkerung: destatis.de"
   )

   abline (
      h = sumAG/population*100
      , col = "blue"
   )
   
   options (digits = 3)
   
   text( Kw
         ,  sumAG/population*100 +0.5
         ,  paste("Anteil an Bevölkerung [",round(sumAG/population*100,1),"%]")
         , cex = 3
         , adj = 1
   )
   
   legend(
      "topright"
      , legend = paste("Altersgruppe", AG)
      , col = "black"
      , lty = 1
      , cex = 5
   )
   
   grid()
   
   
}

single_plot(apply(data[,8:12], 1, FUN=sum) / apply(data[,2:12], 1, FUN=sum) , "60+", population - age59)

for (a in 2:9) {
   l <- ((a-2)*10+1)
   u <- l+9
   calter <- sum(dpop$both[l:u])
   single_plot(data[,a] / apply(data[,2:12], 1, FUN=sum), paste(l-1, "-", u-1), calter)
}

single_plot(apply(data[,10:12], 1, FUN=sum) / apply(data[,2:12], 1, FUN=sum), "80+", age80)

dev.off()