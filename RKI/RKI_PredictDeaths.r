#!/usr/bin/env Rscript

# Entwicklung der CFR nach Altersgruppe und Kw

library(data.table)
library(readr)
library(readODS)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("lib/copyright.r")

options(digits=3)

week_offset <- 0

cumulate <- function(x) {
  
  s <- x
  for (i in 2:length(x)) s[i] <- s[i-1] + x[i]
  
  return (s)
}

deaths <- as.matrix(
  readODS::read_ods(
    path = "data/SterbeFälleAlter.ods" 
    , sheet = 12
  )
)

cases <- as.matrix(readODS::read_ods(path = "data/SterbeFälleAlter.ods", sheet = 3))

AgeGroups <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")

png( "png/RKI_PredictDeaths.png", width = 3840, height = 2160)

par( 
    mar = c(10,10,10,10)
  , mfcol=c(1,2)
  , family = "Carlito"
)

confidence_interval <- function ( p , n, CI = 0.95, bound = "lower") {
  
  sd <- sqrt(p*(1-p)*n)
  f <- qnorm(0.5 + CI/2)
  
  if (bound == "lower") return (p*n-f*sd)
  else return(p*n+f*sd)
  
}

linecolors <- c (
    "black"
  , "blue"
  , "orange"
  , "green"
  , "red"
  
)

linetypes = c (1, 1, 1, 3, 3)

single_plot <- function(data) {
  
  my <- max(data[,2:5])
  plot(
     data[,1]
    #    , data[,2]
        , type = "l"
        , col = linecolors[2]
        , xlim = c( data[1,1], data[length(data[,1]),1]+2)
        , ylim = c(0,my)
        , lwd = 0
        , main = "CoViD19 : Deaths and expected deaths per week"
        , sub = ""
        , xlab = "Calendarweek"
        , ylab = "Count"
        , cex.main = 6
        , cex.sub = 4
        , cex.lab = 3
        , lty = linetypes[2]
        , ann = TRUE
        , cex.axis = 3
        , las = 1
    )
  title (
    sub = "Expected deaths calutate from cases[t-2] * CFR by agegroups"
    , line = 8
    , cex.sub = 6
  )
  

  polygon(
    c(data[,1]+2,rev(data[,1])+2)
    , c(data[,4],rev(data[,5]))
    , col = "lightgrey"
  )
  
  lines( data[,1]
         , data[,2]
         , col = linecolors[2]
         , lwd = 6
         , lty = linetypes[2]
  )
  
  for (i in 3:5) {
  lines( data[,1]+2
        , data[,i]
        , col = linecolors[i]
        , lwd = 6
        , lty = linetypes[i]
  )
  }
  legend ( 
    "top"
    , legend = c(
        "reale Todesfälle"
      , "vorgegesagte Todesfälle"
      , "unterer Wert"
      , "oberer Wert"
    )
    , col = linecolors[2:5]
    , lty = linetypes[2:5]
    , lwd = 4
    , cex = 4
  )

}

lcaces <-length(cases[,1])

CFR <- colSums(deaths[,2:12])/colSums(cases[,2:12])

prediction <- cbind( 
      cases[,1] # calender week
    , rowSums(deaths[,2:12]) # real deaths in week
    , colSums(t(cases[,2:12])*CFR) # expected value
    , colSums(
        confidence_interval(
           CFR
          , t(cases[,2:12])
          , bound = "lower"
          )
        ) # ... lower bound
    , colSums(
        confidence_interval (
            CFR
          , t(cases[,2:12])
          , bound = "upper"
          )
        )  # ... upper bound
    )

single_plot(prediction)

grid()

r <- prediction[,1]>=20

single_plot(prediction[r,])

grid()

copyright()

dev.off()
