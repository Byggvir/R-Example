#!/usr/bin/env Rscript

# Schätzung der Standardabweichung

MyScriptName <- 'GuessSD'
setwd("~/git/R-Example")

library(data.table)
library(MatchIt)
library(dplyr)
library(REST)
library(ggplot2)
library(viridis)
library(hrbrthemes)

#set.seed(42)

trials <- 1000

n <- c(39,19)
means <- c(25.8,28.8)
stdevs <- c(4.2,3.8)

anz <- 33
ctm <- 25.8
cts <- 3.8

rtrial <- data.table( 
    m = rep(0,trials)
  , s = rep(0,trials)
)

for (i in 1:trials) {
  d <- c(rnorm(n[1],means[1],stdevs[1]),rnorm(n[2],means[2],stdevs[2]))
  rtrial$m[i] <- mean(d)
  rtrial$s[i] <- sd(d)
}

mm <- mean(rtrial$m)
ms <- sd(rtrial$m)

sm <- mean(rtrial$s)
ss <- sd(rtrial$s)

print(c(mean(rtrial$m),sd(rtrial$m)))
print(c(mean(rtrial$s),sd(rtrial$s)))

z <- sum(n*x)/sum(n)
sz <- sqrt(sum(n*s^2)/sum(n)+sum(n*x^2)/sum(n) - z^2)
print(c(z,sz))

blp <- ggplot(rtrial, aes(x=m)) + 
  geom_histogram( bins=100, color="black", fill="lightblue" ,stat = "density") +
  stat_function(fun = function(x) {dnorm(x, mm, ms)}, color = "red") +
  lims(x = c(mm - 3*ms, mm + 3*ms))

ggsave(plot = blp, file = paste('/tmp/', MyScriptName, "-1.png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

blp <- ggplot(rtrial, aes(x=s)) + 
  geom_histogram( bins=100, color="black", fill="lightblue", stat = "density") +
  stat_function(fun = function(x) {dnorm(x, sm, ss)}, color = "red") +
  lims(x = c(sm - 3*ss, sm + 3*ss))

ggsave(plot = blp, file = paste('/tmp/', MyScriptName, "-2.png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)
