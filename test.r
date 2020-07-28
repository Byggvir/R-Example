#!/usr/bin/r

library("survival")
library("survminer")
library("ggplot2")


data("lung")
head(lung)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

fit2 <- survfit( Surv(time, status) ~ sex + rx + adhere,
                data = colon )
                
ggsurvplot(fit,
          conf.int = TRUE,
          risk.table.col = "strata", # Change risk table color by groups
          ggtheme = theme_bw(), # Change ggplot2 theme
          palette = c("#E7B800", "#2E9FDF"),
          fun = "cumhaz")


