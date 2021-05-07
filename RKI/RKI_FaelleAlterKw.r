#!/usr/bin/env Rscript
#
#
# Script: DEFalleAlterKw.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI_FalleAlterKw"

library(data.table)


setwd("~/git/R-Example")
source("common/rki_sql.r")
source("common/rki_download.r")
source("lib/copyright.r")

plotcol <- "white"

AGrp <- sqlGetRKI(SQL="select distinct AgeGroup from RKI_CasesByAge order by AgeGroup;")

CalWeeks <- sqlGetRKI(SQL="select distinct (Jahr-2020)*53 + Kw as Kw from RKI_CasesByAge order by Jahr, Kw;") 
CalWeekSum <- sqlGetRKI(SQL="select (Jahr-2020)*53 + Kw as Kw, sum(Count) as Count from RKI_CasesByAge group by Jahr,Kw;")

dpop <- sqlGetRKI( 
   SQL = "
select age, sum(male) as male,sum(female) as female, (sum(male)+sum(female))  'both' 
from DEU 
where sdate='2019-12-31' 
group by age;
")

# read.table(file("data/DEPopulationAge.csv"), sep=";", header=TRUE)

AgeGroups <- NULL

for (i in c("",1,2,3,4,5,6,7,8) ) { 
AgeGroups <- c(  AgeGroups
               , paste(i,"0-",i,"4",sep='')
               , paste(i,"5-",i,"9",sep=''))
}

AgeGroups <- c(AgeGroups, "90+")

Kw <- max(CalWeeks[,"Kw"])

population <- sum(as.numeric(dpop[,4]))
                  
par(mar=c(5,5,8,5), mfrow = c(4,5))

single_plot <- function(x, AG, sumAG,ylim=c(0,20)) {
   
   png(paste( "png/", MyScriptName,"-", AG, ".png", sep=""), width = 960, height = 540)
   plot(
      data[,'Kw']
      , data[,'Count']/CalWeekSum[,'Count'] * 100
      , type = "l"
      , lwd = 3
      , col = "black"
      , xlab = "Kalenderwoche"
      , ylab = "%"
      , ylim = ylim
      , main = paste("Anteil Altersgruppe", AG, "an wöchentlichen COVID-19 Fällen")
      , cex.main = 2
      , sub = "Quelle: Fallzahlen rki.de; Bevölkerung: destatis.de"
   )


   abline (
      h = sumAG/population*100
      , col = "blue"
   )
   
   options (digits = 3)
   
   text( 30
         ,  sumAG/population*100 +0.5
         ,  paste("Anteil an Bevölkerung [",round(sumAG/population*100,1),"%]")
         , cex = 1
         , adj = 1
   )
   
   legend(
      "topright"
      , legend = paste("Altersgruppe", AG, "\nAnteil an Bevölkerung [",round(sumAG/population*100,1),"%]" )
      , col = "black"
      , lty = 1
      , cex = 2
   )
   
   grid()

   copyright()
   
   dev.off()
   
}

# --- 20- ----
# Nur ein Blick auf die Altersgruppe 20-

sql <- paste("select (Jahr-2020)*53 + Kw as Kw, sum(Count) as Count from RKI_CasesByAge where AgeGroup < ", 20 , ' group by Jahr,Kw order by Jahr, Kw;', sep="")

print(sql)

data <- sqlGetRKI(SQL=sql)

l <- 1
u <- 20
calter <- sum(dpop$both[l:u])

single_plot(data, "0-19", calter, c(0,40))


i <- 1
for (a in AGrp[,1]) {
   
   if (a < 85) {
   sql <- paste("select (Jahr-2020)*53 + Kw as Kw, Count from RKI_CasesByAge where AgeGroup = ", a , '  ;', sep="")
   print(sql)
   data <- sqlGetRKI(SQL=sql)
   l <- a + 1
   u <- a + 5
   calter <- sum(dpop$both[l:u])
   single_plot(data, AgeGroups[i], calter)
   
   }
   
   i<-i+1
}

#--- 85+ ---
# Die Daten der Population vom Statistikamt reichen nur bis 85+, RKI liefert 90+
# Daher werden die 85+ / 85-89,90+ zusammengefasst.

sql <- paste("select (Jahr-2020)*53 + Kw as Kw, sum(Count) as Count from RKI_CasesByAge where AgeGroup >= ", 85 , '  group by Jahr, Kw order by Jahr, Kw;', sep="")

print(sql)

data <- sqlGetRKI(SQL=sql)

l <- 86
u <- 86
calter <- (dpop$both[l:u])

single_plot(data, "85+", dpop$both[length(dpop$both)])


# --- 60+ ----
# Nur ein Blick auf die Altersgruppe 60+

sql <- paste("select AgeGroup,(Jahr-2020)*53 + Kw as Kw,sum(Count) as Count from RKI_CasesByAge where AgeGroup >= ", 60 , '  group by Jahr,Kw order by Jahr,Kw;', sep="")

print(sql)

data <- sqlGetRKI(SQL=sql)

l <- 61
u <- 86
calter <- sum(dpop$both[l:u])

single_plot(data, "60+", calter, c(0,40))

# --- 20 - 49----
# Nur ein Blick auf die Altersgruppe 60+

sql <- paste("select AgeGroup,(Jahr-2020)*53 + Kw as Kw,sum(Count) as Count from RKI_CasesByAge where AgeGroup >= 20 and  AgeGroup <= 49 group by Jahr,Kw order by Jahr,Kw;", sep="")

print(sql)

data <- sqlGetRKI(SQL=sql)

l <- 20
u <- 49
calter <- sum(dpop$both[l:u])

single_plot(data, "20-49", calter, c(0,80))



dev.off()
