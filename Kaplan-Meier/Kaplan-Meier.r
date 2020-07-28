#!/usr/bin/r

library("survival")
library("survminer")
library("ggplot2")

if(!exists("survivals", mode="function")) source("../common/Survival.r")

png("Kaplan-Meier.png",width=1920,height=1080)

n <-1000    # Number of patients 
d <-14      # Maximum time of observation / end of study

# Sim

pats <- data.frame (
      time=c(survivals(0.1,n),survivals(0.05,n))
    , status=rep(2,2*n)
    , sex=c(rep(0,n),rep(1,n))
    )

pats$status[pats$time>d] <- 1
pats$time[pats$time>d] <- d+1

fit <- survfit(Surv(time, status) ~ sex, data = pats)

ggsurvplot(fit
        , pval = TRUE
        , conf.int = TRUE
        , risk.table = TRUE
        , risk.table.col = "strata"
        , break.time.by = 7
        , ncensor.plot = TRUE
        , linetype = "strata"
        , surv.median.line = "hv"
        , ggtheme = theme_light()
        , palette = c("#E7B800", "#2E9FDF")
        , legend.labs = 
            c("Male", "Female")
        , xlim = c(1,10)
        , fun = "event"
        )
        
dev.off()
print(summary(fit)$table)
