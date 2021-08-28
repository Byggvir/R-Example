#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2021-02-22
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "RKI_RegressionAnalysisV2"

require(data.table)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)

setwd("~/git/R-Example")

source("lib/copyright.r")
source("common/ta_regressionanalysis.r")
source("common/rki_sql.r")

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  ThisDay <- today - 2
  
} else if (length(args) == 1) {
  ThisDay <- as.Date(args[1])
  
} else if (length(args) >= 2){
  ThisDay <- as.Date(args[1])
}

Wochentage <- c("So","Mo","Di","Mi","Do","Fr","Sa")

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

SQLWTag <- 
  paste(
  'select' 
, 'WTag'
, ', avg(AnteilAnWoche) as AnteilAnWoche'
, ', avg(AnteilAnWoche) * 7 as KorFaktor'
, ', stddev(AnteilAnWoche) as StdAbweichung'
, ' from ('
, 'select' 
, 'F.Meldedatum'
, ', weekday(F.Meldedatum) as WTag'
, ', sum(F.AnzahlFall) / W.FallWoche as AnteilAnWoche'
, 'from RKIFaelle F'
, 'join ('
, 'select' 
, 'week(Meldedatum,3) as Kw'
, ', sum(AnzahlFall) as FallWoche'
, 'from RKIFaelle' 
, 'where Meldedatum >'
, '"2020-05-03"'
, 'and Meldedatum < adddate("'
        , ThisDay
        , '",-weekday("'
        , ThisDay
        , '"))'
, 'group by Kw' 
, ') as W' 
, 'on' 
, 'week(F.Meldedatum,3) = W.Kw'
, 'where Meldedatum >'
        , '"2020-05-03"'
        , 'and Meldedatum < adddate("'
        , ThisDay
        , '",-weekday("'
        , ThisDay
        , '"))'
, 'group by F.Meldedatum'
, ') as T'
, 'group by WTag;'
, sep= ' '
  )

Kor <- sqlGetRKI(SQLWTag)  

print(Kor)
