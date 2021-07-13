#!/usr/bin/env Rscript

library(rvest)

all_tables <- read_html( "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Gesundheit/Todesursachen/Tabellen/sterbefaelle-unfaelle.html") %>%
  html_table(
      fill= TRUE
    , header= TRUE
    )

drop <- c(1,2)
unfaelle <- all_tables[[1]][-drop,]
colnames(unfaelle) <- all_tables[[1]][1,]
print(unfaelle)
