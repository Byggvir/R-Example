#!/usr/bin/env Rscript
#
#
# Script: map.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

library(tidyverse)
library(purrr)
require(data.table)

MyScriptName <- "map"

setwd("~/git/R-Example")
source("common/rki_sql.r")

sqlGetRKI()[,5:6] %>% group_by(RKI[,3]) %>% summarize (out = sum(RKI[,5])) -> mx
print(mx)
1
