#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_CompareCountries"
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

SQL = 'call CompareCountries(276);'

data <- sqlGetRKI(SQL = SQL)


alt <- data[data$Altersgruppe>=50,]

jung <- data[data$Altersgruppe<50,]

p1 <- ggplot(alt, aes(fill=Region, y=VergleichsTodesfallzahl, x=as.numeric(Altersgruppe))) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=round(VergleichsTodesfallzahl,1)), vjust=-0.1, color="black",
            position = position_dodge(width=9), size=2)+
  scale_fill_brewer(palette="Paired")+
  ggtitle("Corona: Todesfallzahlen pro 100k Einw. (Alters-standardisiert auf DEU) >= 50 Jahre"  
  , subtitle="Stand DE: 16.02.2021") +
  theme_minimal() +
  xlab("Altersgruppe") +
  ylab("Todesfälle")

p2 <- ggplot(jung, aes(fill=Region, y=VergleichsTodesfallzahl, x=as.numeric(Altersgruppe))) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=round(VergleichsTodesfallzahl,1)), vjust=-0.1, color="black",
            position = position_dodge(width=9), size=2)+
  scale_fill_brewer(palette="Paired")+
  ggtitle("Corona: Todesfallzahlen pro 100k Einw.  (Alters-standardisiert auf DEU) < 50 Jahre") +
  theme_minimal() +
  xlab("Altersgruppe") +
  ylab("Todesfälle")


gg <- grid.arrange(p1, p2, ncol=1)

plot(gg)

ggsave(plot = gg, file = paste('png/', MyScriptName,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

SQL = 'call CompareCountriesSum(276);'

data <- sqlGetRKI(SQL = SQL)

p3 <- ggplot(data, aes(fill=Region,y=VergleichsTodesfallzahl, x=Region)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=round(VergleichsTodesfallzahl,1)), vjust=-0.1, color="black",
            position = position_dodge(width=1), size=5)+
  scale_fill_viridis(discrete = T) +
  ggtitle("Corona: Todesfallzahlen pro 100k Einw. (Alters-standardisiert auf DEU)"  
          , subtitle="Stand DE: 15.02.2021") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Region") +
  ylab("Todesfälle")

gg <- grid.arrange(p3, ncol=1)

plot(gg)

ggsave(plot = gg, file = paste('png/', MyScriptName,"-Sum.png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

