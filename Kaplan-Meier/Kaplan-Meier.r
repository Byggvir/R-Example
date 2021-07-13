#!/usr/bin/env Rscript
#
#
# Script: Kaplan-Meier.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"Kaplan-Meier"


# Thomas Arend
# (c) 2020 
# GnuPL 3.0


# Simulation of the survival times for a Kaplan-Meier example

setwd("~/git/R-Example")

library("survival")
library("survminer")
library("ggplot2")

if(!exists("survivals", mode="function")) source("common/Survival.r")

n <-50    # Number of patients 
d <-14      # Maximum time of observation / end of study

pats <- data.frame (
      time=c(survivals(0.1,n),survivals(0.05,n))
    , status=rep(2,2*n)
    , sex=c(rep(0,n),rep(1,n))
    )

pats$status[pats$time>d] <- 1
pats$time[pats$time>d] <- d+1

print(pats)

fit <- survfit(Surv(time, status) ~ sex, data = pats)

surfp <- ggsurvplot(fit
        , data = pats
        , main = "Survival curve",
        font.main = c(16, "bold", "darkblue"),
        font.x = c(14, "bold.italic", "red"),
        font.y = c(14, "bold.italic", "darkred"),
        font.tickslab = c(12, "plain", "darkgreen"))

        # , pval = TRUE
        # , conf.int = TRUE
        # , risk.table = TRUE
        # , risk.table.col = "strata"
        # , break.time.by = 7
        # , ncensor.plot = TRUE
        # , linetype = "strata"
        # , surv.median.line = "hv"
        # # , ggtheme = theme_light()
        # , palette = c("#E7B800", "#2E9FDF")
        # , legend.labs =
        #     c("Male", "Female")
        # , xlim = c(1,10)
        # , fun = "event"
        # )

ggsave( plot = print(surfp), file = 'png/Kaplan-Meier.png'
       , type = "cairo-png"
       , bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

print(summary(fit))
