#!/usr/bin/r
#
#
# Script: Cox.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"Cox"


# Thomas Arend
# (c) 2020 
# GnuPL 3.0


# Simulation of the survival times for a Cox regression example

setwd("~/git/R-Example")

library("survival")
library("survminer")
library("ggplot2")

if(!exists("survivals", mode="function")) source("common/Survival.r")

n <-1000    # Number of patients 
d <-14      # Maximum time of observation / end of study

pats <- data.frame (
      time=c(survivals(0.02,n),survivals(0.01,n))
    , status=rep(2,2*n)
    , sex=c(rep(0,n),rep(1,n))
    )

pats$status[pats$time>d] <- 1
pats$time[pats$time>d] <- d+1

res.cox <- coxph(Surv(time,status) ~ sex, data = pats)

print(summary(res.cox))
