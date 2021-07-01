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

sympt <- c(  18,19,20,21,21,21,22,22,23,23
             ,24,24,24,24,24,25,25,25,25,25
             ,26,26,26,27,27,27,28,28,28,28
             ,29,29,30,30,31,31,32,33,33)
asympt <- c(  21,22,24,24,24,26,28,28,29,30
             ,30,30,31,32,32,33,33,33,36)


n <- c(39,19)
means <- c(25.74,29.95) #28.74)
stdevs <- c(4.21,3.80)

f <- function(x) {
  
  return ( (n[1]*dnorm(x, means[1], stdevs[1]) + n[2]*dnorm(x, means[2], stdevs[2])))
  
}

f1 <- function(x) {
  
  return ( (n[1]*dnorm(x, means[1], stdevs[1])))

}

f2 <- function(x) {
  
  return ( n[2]*dnorm(x, means[2], stdevs[2]))
  
}

d <- round(c(rnorm(n[1],means[1],stdevs[1]),rnorm(n[2],means[2],stdevs[2])))
#d <- c(asympt,sympt)

print(c(mean(d),sd(d)))

z <- sum(n*means)/sum(n)
sz <- sqrt(sum(n*stdevs^2)/sum(n)+sum(n*means^2)/sum(n) - z^2)

print(c(z,sz))

df <- data.frame(values=d)
dfa <- data.frame(values=asympt)
dfs <- data.frame(values=sympt)

blp <- ggplot(df, aes(x=values)) + 
  geom_histogram( mapping = aes(x=values), data = df, bins=100, color="black", fill="white", alpha=0.3) +
  geom_histogram( mapping = aes(x=values), data = dfs, bins=100, color="black", fill="green", alpha=0.3) +
  geom_histogram( mapping = aes(x=values), data = dfa, bins=100, color="black", fill="blue", alpha=0.3) +
  stat_function(fun = "f", color = "red") +
  stat_function(fun = "f1", color = "green") +
  stat_function(fun = "f2", color = "blue") +
  lims(x = c(means[1] - 4*stdevs[1], means[2] + 4*stdevs[2]))

ggsave(plot = blp, file = paste('/tmp/', MyScriptName, "-3.png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)
