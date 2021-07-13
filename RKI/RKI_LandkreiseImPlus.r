#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(OutDec=',')

MyScriptName <- "RKI_Landkreis.r"

require(data.table)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)

setwd("~/git/R-Example/")

source("lib/copyright.r")
source("common/rki_sql.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
To_Kw <- isoweek(today) + 53

LandkreisePlus <- function (From_Kw = 0, To_Kw = 53) {

data <- sqlGetRKI(paste('call Landkreise(', From_Kw , ',', To_Kw, ');' )) 

print(data)
labs <- data$Kw
j20 <- data$Kw < 54
j21 <- data$Kw > 53

labs[labs>53] <- labs[j21] - 53
labs[j20] <- paste(labs[j20],20,sep='/')
labs[j21] <- paste(labs[j21],21,sep='/')


p <- ggplot(data, aes( x = sKw)) +
  geom_line(aes(y = GT, colour="> Fälle"), show.legend = TRUE) +
#  geom_line(aes(y = LEQ, colour="<= Fälle"), show.legend = TRUE) +
  geom_line(aes(y = Zero, colour="Keine Fälle") ,show.legend = TRUE) +
  scale_colour_manual(""
                      , breaks = c("Keine Fälle", "<= Fälle", "> Fälle")
                      , values=c("darkblue","darkgreen","darkred")) +
  scale_x_continuous("Kalenderwoche"
                   , breaks = c(20,40,60,80)
                   , labels = c("20/20","40/20","7/21","27/21" )) +
  
  geom_text(aes(label=GT,y=GT), angle = 0, vjust=0, hjust=1, color="red", size=2) +
#  geom_text(aes(label=LEQ,y=LEQ), angle = 0, vjust=1, hjust=1, color="blue", size=2) +
  geom_text(aes(label=Zero,y=Zero), angle = 0, vjust=1, hjust=0, color="blue", size=2) +
  ggtitle( paste('Corona: Zahl der Landkreise ohne oder mit mehr Fällen als in der Vorwoche' )) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="bottom") +
  xlab("Kalenderwoche") +
  ylab("Fallzahlen pro Kw")

gg <- grid.arrange(p, ncol=1)

plot(gg)

ggsave(plot = gg, file = paste('png/Landkreise-', From_Kw,"-",To_Kw,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.1, height = 21, units = "cm", dpi = 150)
}

# Main program

  LandkreisePlus(9,80)
