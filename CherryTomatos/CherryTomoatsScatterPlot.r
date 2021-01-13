#!/usr/bin/env Rscript
#
#
# Script: CherryTomatos.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(scipen=999)  # turn-off scientific notation like 1e+48
MyScriptName <- "CherryTomatosScatterPlot"

require(data.table)

source("common/rki_download.r")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)
library(ggplot2)

theme_set(theme_bw())  # pre-set the bw theme.

data <- read.csv2('data/CherryTomatos2.csv')

# Scatterplot

gg <- ggplot(data, aes(x=as.numeric(Volumen), y=as.numeric(Gewicht))) + 
  geom_point(aes(size=1)) + 
  geom_smooth(method="lm", se=TRUE) + 
  labs(subtitle="Volumen - Gewicht", 
       y="Gewicht", 
       x="Volumen", 
       title="Cherry Tomaten", 
       caption = "Source: Thomas Arend")

plot(gg)

ggsave(plot = gg, file = paste('png/', MyScriptName,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)
