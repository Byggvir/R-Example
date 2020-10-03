#!/usr/bin/env Rscript

library(rvest)
library(xlsx)

setwd("~/git/R-Example")

all_tables <- read_html( "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Gesundheit/Todesursachen/Tabellen/sterbefaelle-unfaelle.html") %>%
  html_table(
      fill= TRUE
    , header= TRUE
    )

drop <- c(1,2)

unfaelle <- all_tables[[1]][-drop,]

colnames(unfaelle) <- c("Altersgruppe",all_tables[[1]][1,-1])

for (i in 1:length(unfaelle[,1])) {
  for (j in 2:length(unfaelle[1,])) {
    unfaelle[i,j] <- gsub("[^0-9-]*","",unfaelle[i,j])
    unfaelle[i,j] <- gsub("-",0,unfaelle[i,j])
  }  
}

print(unfaelle)

write.csv2( 
  unfaelle
  , file="data/sterbefaelle-unfall.csv"
  , quote = FALSE
  )

write.xlsx( 
  unfaelle
  , file="data/sterbefaelle-unfall.xlsx"
)