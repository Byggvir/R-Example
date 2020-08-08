#!/usr/bin/r

tests <- read.csv("data/rki_testungen.csv")
cases <- read.csv("data/Germany.csv")

l <- length(cases$Date)
cases$incCases <- c(0, cases$Cases[2:l]-cases$Cases[1:l-1])

Kw <- unique(tests$Kw)
for ( i in Kw ) {
  tests$New[tests$Kw ==i] <- sum(cases$incCases[cases$Kw==i])
}