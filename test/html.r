#!/usr/bin/env Rscript

library(rvest)

all_tables <- read_html( "https://covid.cdc.gov/covid-data-tracker/") %>%
  html_table(
      fill= TRUE
    , header= TRUE
    )

print (all_tables)

US <- read.csv("blob:https://covid.cdc.gov/3dca7c8a-2c10-4314-b2db-089e289b751c"
               , header=TRUE
               , sep = ","
               , dec = "."
               , fill = TRUE
               )
head (US)
