#!/usr/bin/r

require(data.table)

png("Deaths.png",width=1920,height=1080)

wdays <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

fconfirmed <- read.csv(file = 'confirmed.csv', header=TRUE, sep=",")
fdeaths <- read.csv(file = 'deaths.csv')

cd = data.frame(UID=fconfirmed$UID,date=fconfirmed$day,wday=(fconfirmed$day+2)%%7,confirmed=fconfirmed$count)
dd = data.frame(UID=fdeaths$UID,date=fdeaths$day,wday=(fdeaths$day+2)%%7,deaths=fdeaths$count)

cases <- aggregate(confirmed~date,FUN=sum,data=cd)
deaths <- aggregate(deaths~date,FUN=sum,data=dd)

m <- length(cases$confirmed)
n <- length(deaths$deaths)

daily = data.frame( 
    date=cases$date,
    wday=(cases$date+2)%%7,
    incCases=c(0,cases$confirmed[2:m]-cases$confirmed[1:m-1]),
    incDeaths=c(0,deaths$deaths[2:n]-deaths$deaths[1:n-1])
    )

wdayCases   <-  aggregate(incCases~wday,FUN=sum,data=daily)
wdayDeaths  <-  aggregate(incDeaths~wday,FUN=sum,data=daily)

options(digits=7)
options(scipen=7)
options(Outdec=".")

barplot( daily$incDeaths
    , ylim=c(0,5000)
    #, col=colors[1]
    , main = "US Deaths per day" 
    , sub = paste("Date:", heute )
    , xlab="Day",
    , ylab="Count"
    )     

grid()
