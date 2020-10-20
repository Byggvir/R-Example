#!/usr/bin/env Rscript

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The daily cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

library(REST)
require(data.table)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("lib/copyright.r")

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

png(  "png/RKI_weekly.png"
      , width=1920
      , height=1080
)

reganalysis <- function (data, zr,  main = "Wöchentliche Fälle DE" ) {
  
  z <- length(zr)
  
  pylim <- c(min(data$Count[zr]),max(data$Count[zr]))
  
  ra1 <- lm(data$Count[zr] ~ data$Kw[zr])
  ra2 <- lm(log(data$Count[zr]) ~ data$Kw[zr])
  
  a <- ra2$coefficients[1]
  b <- ra2$coefficients[2]
  
  plot(
      data$Kw[zr] 
    , data$Count[zr]
       , main = ""
       , sub = paste("Vom", data$Kw[zr[1]], "bis", data$Kw[zr[z]], "Kw")
       , xlab = "Datum"
       , ylab = "Wöchentliche Fälle"
       , ylim = pylim
       , type = "b"
       , lwd = 3
  )
  title ( 
    main = main
    , cex.main = 4)
  
  grid()

  abline( ra1
          , col="red"
          , lwd = 3
  )

  par (new = TRUE)

  curve( exp(a+b*x)
         , from = data$Kw[zr[1]]
         , to = data$Kw[zr[z]]
         , col="green"
         , axes = FALSE
         , xlab = ""
         , ylab = ""
         , ylim = pylim
         , lwd = 3
  )
  legend ( "top"
           , legend = c("Fälle", "Lineare Regression", "Logarithmische Regression")
           , col = c("black", "red", "green")
           , lwd = 2
           , cex = 2
  )
  
}
# ----

daily <- get_rki_tag_csv()

zeitraum <- 1:4

par( mfcol = c(1,2))

weekly <- aggregate(incCases ~ Kw, FUN = sum, data = daily)
colnames(weekly) <-  c("Kw","Count")
reganalysis(weekly, zeitraum,main = "Wöchentliche Fälle DE" )

rm(weekly)

weekly <- aggregate(incDeaths ~ Kw, FUN = sum, data = daily)

colnames(weekly) <- c("Kw","Count")
reganalysis(weekly, 29:31, main = "Wöchentliche Sterbefälle DE")

copyright()

dev.off()
