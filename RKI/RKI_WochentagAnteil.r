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
MyScriptName <- "RKI_WochentagAnteil"


wd <- setwd("~/git/R-Example")
require(data.table)
source("common/rki_sql.r")
source("lib/copyright.r")
source("lib/myfunctions.r")

library(REST)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(Cairo)
library(extrafont)
extrafont::loadfonts()

startdate <- "2020-03-15"
theme_set(theme_bw())  # pre-set the bw theme.
SQL = paste('
select 
  F.Meldedatum
  , weekday(F.Meldedatum) as WTag
  , sum(F.AnzahlFall) / W.FallWoche * 100 as AnteilWoche
from RKIFaelle F 
join ( 
  select 
    week(Meldedatum,3) as Kw
    , sum(AnzahlFall) as FallWoche
  from RKIFaelle 
  group by Kw 
  ) as W 
on 
  week(F.Meldedatum,3) = W.Kw
where Meldedatum >'
,'"2020-05-31"'
,'and Meldedatum < adddate(curdate(),-weekday(curdate()))
group by F.Meldedatum;'
, sep= ' '
)


data <- sqlGetRKI(SQL = SQL)
print(nrow(data))

# Scatterplot

wt <- c("Mo","Di","Mi","Do","Fr","Sa","So")

gg <- ggplot(data, aes(x=WTag, y=AnteilWoche)) + 
  geom_point() + 
  geom_boxplot(aes(x=WTag,y=AnteilWoche, group=WTag)) + 
  # geom_smooth(method="loess", se=TRUE) +
  scale_x_continuous(breaks=0:6, labels=wt ) +
  theme_ipsum() +
  labs(
       y="%-Anteil des Wochentages", 
       x="Wochentag", 
       subtitle="Nach Meldedatum (ab Kw 12)", 
       title="Anteil der Wochentage an den Fallzahlen der Woche", 
       caption = "Quelle: Robert Koch-Institut (RKI), dl-de/by-2-0 / (cc) Thomas Arend")

plot(gg)

ggsave(plot = gg, file = paste('png/', MyScriptName,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

for (i in 0:6) {
  print (paste ("Wochentag","=",wt[i]))
  print(quantile(data[data[,2]==i,3]))
  
}

for (i in 0:6) {
  print (paste ("Wochentag","=",wt[i]))
  print( c(
            mean(data[data[,2]==i,3])
            , sd(data[data[,2]==i,3])
         )
  )
  
}


setwd(wd)

