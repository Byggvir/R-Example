#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_DEUSWE"
op <- options(nwarnings = 10000)

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The weekly cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

setwd("~/git/R-Example")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)
library(ggplot2)
library(plyr)
library(gridExtra)
library(viridis)
library(hrbrthemes)

# Stacked


options( 
    digits = 7
  , scipen = 999
  , Outdec = "."
  , max.print = 3000
  )

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL = 'call MortalityDEUSWE;'

data <- sqlGetRKI(SQL = SQL)


alt <- data[data$Altersgruppe>=50,]

jung <- data[data$Altersgruppe<50,]

p1 <- ggplot(alt, aes(fill=Region, y=TodesFallzahl, x=as.numeric(Altersgruppe))) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=TodesFallzahl), vjust=-0.1, color="black",
            position = position_dodge(width=10), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  ggtitle("Corona: Vergleich Todesfallzahlen DEU - SWE (Alters-standardisiert auf DEU) >= 50 Jahre"
  , subtitle="Stand: 31.01.2021") +
  theme_minimal() +
  xlab("Altersgruppe") +
  ylab("Todesfälle")

p2 <- ggplot(jung, aes(fill=Region, y=TodesFallzahl, x=as.numeric(Altersgruppe))) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=TodesFallzahl), vjust=-0.1, color="black",
            position = position_dodge(width=10), size=4)+
  scale_fill_brewer(palette="Paired")+
  ggtitle("Corona: Vergleich Todesfallzahlen DEU - SWE (Alters-standardisiert auf DEU) >= 50 Jahre") +
  theme_minimal() +
  xlab("Altersgruppe") +
  ylab("Todesfälle")


gg <- grid.arrange(p1, p2, ncol=1)

plot(gg)

ggsave(plot = gg, file = paste('png/', MyScriptName,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

summary(warnings())
