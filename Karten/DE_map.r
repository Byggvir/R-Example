#!/usr/bin/env Rscript

library("maps")
library("mapdata")
if (requireNamespace("mapdata", quietly=TRUE) && packageVersion("mapdata") >= "2.3.0") {
  map("mapdata::worldLores", region="germany",fill = FALSE, col = 1)
}
