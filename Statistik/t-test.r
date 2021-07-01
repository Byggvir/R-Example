#!/usr/bin/env Rscript

library(data.table)
library(MatchIt)
library(dplyr)

#set.seed(142)
anz <- 10000

DiceProb <- c(1/9,rep(1/6,4),2/9)

Kohorten <- data.table(
  id = 1:(2*anz)
  , Age = c(
    round(rnorm(anz,50,15),2)
    , round(rnorm(anz,55,10),2)
  )
  , K = c(rep(0,anz),rep(1,anz))
  , dice = c(sample(1:6, anz, replace=TRUE),
             sample(1:6, anz, replace=TRUE, prob = DiceProb)
  )
)

Kohorten[Kohorten$Age<0] <- 0


t1 <- t.test(Age ~ K , data = Kohorten, var.equal=FALSE)
print(t1)

t2 <- t.test(Age ~ K , data = Kohorten, var.equal=TRUE)
print(t2)

t3 <- t.test(dice ~ K , data = Kohorten, var.equal=FALSE)
print(t3)
