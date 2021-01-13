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
MyScriptName <- "RKI_DeathsCasesScatterPlot"

require(data.table)

source("common/rki_download.r")
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)
library(ggplot2)

theme_set(theme_bw())  # pre-set the bw theme.

data <- sqlGetRKI(SQL = 'call CasesPerWeekOffset(3);')
print(nrow(data))

# Scatterplot

gg <- ggplot(data, aes(x=as.numeric(Cases), y=as.numeric(Deaths))) + 
  geom_point(aes(size=Deaths/Cases)) + 
  geom_smooth(method="loess", se=TRUE) + 
  labs(subtitle="Vergleich Neuinfektionen zu Todesfälle 19 Tage später", 
       y="Täglich gemeldete Tote", 
       x="Täglich gemeldete Neufälle", 
       title="DEU Todesfälle / Fälle", 
       caption = "Source: RKI")

plot(gg)

ggsave(plot = gg, file = paste('png/', MyScriptName,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)
