#!/usr/bin/env Rscript

#library(data.table)
#library(MatchIt)
#library(dplyr)
library(fBasics)

asympt <- c(  18,19,20,21,21,21,22,22,23,23
             ,24,24,24,24,24,25,25,25,25,25
             ,26,26,26,27,27,27,28,28,28,28
             ,29,29,30,30,31,31,32,33,33)
sympt <- c(   21,22,24,24,24,26,28,28,29,30
             ,30,30,31,32,32,33,33,33,36)

t <- t.test( asympt, sympt
             , alternative = "t"
             , var.equal = FALSE
             , conf.level = 0.95)
print(t)
