#!/usr/bin/env Rscript

LookBack <- 14
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  FromUntil <- c( Sys.Date()-14, Sys.Date())
  
} else if (length(args)==1) {
  tempD <- as.Date(args[1])
  FromUntil <- c( tempD - LookBack, tempD )
  
} else if (length(args)>=2){
  FromUntil <- sort(c(as.Date(args[1]),as.Date(args[2])))
  }

print(FromUntil)
