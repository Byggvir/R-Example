#!/usr/bin/env Rscript
#
#
# Script: RKI_PredictDeaths.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_PredictDeaths"


# Entwicklung der CFR nach Altersgruppe und Kw

library(data.table)
library(readr)
library(readODS)
library(gridExtra)
library(grid)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("lib/copyright.r")

options(digits=3)

week_offset <- 3

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
  
  mx <- max(data[length(data[,1]),1])
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
    for ( j in 1:3 ){ 
      text( 
        data[data[,1] > mx-j,1]+2
        , data[data[,1] > mx-j,i]
        , round(data[data[,1] > mx-j,i])
        , col = linecolors[i]
        , cex = 3
        , adj = 1

      )
    }
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

pl <- length(prediction[,1])

ptab <- cbind(
  prediction[(pl-5),1]:(prediction[pl,1]+2)
  , c(prediction[(pl-5):pl,2],0,0)
  , prediction[(pl-7):pl,3]
  , prediction[(pl-7):pl,4]
  , prediction[(pl-7):pl,5]
)

vp <- viewport(x=0.75,y=0.5,width=0.5,height = 0.6)

tt <- ttheme_default(
        base_size = 48
        , core=list(
          fg_params=list(fontface=c(rep("plain", 6), rep("bold.italic",2))
                 )
          )
        )

g <- tableGrob(
      round(ptab)
      , theme = tt
      , cols = c("Kw", "Tote","Erwartet", "Min CI95%","Max CI95%" )
      , vp = vp      
      )

grid.draw(g
          , )

copyright()

dev.off()
