#!/usr/bin/r

require(data.table)

png("CaseFatalityRate.png",width=1920,height=1080)

wdays <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

fconfirmed <- read.csv(file = '../data/US-confirmed.csv', header=TRUE, sep=",")
fdeaths <- read.csv(file = '../data/US-deaths.csv')

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

upordown    = data.frame(date=daily$date,wday=daily$wday,updown= c(0,sign(daily$incCases[2:m]-daily$incCases[1:m-1])))

message("Counts of ups or downs previous weekday [>0 more ups, <0 more downs]")

options(digits=5)

start <- 100
offset <- seq(0,28,7)

plot( cases$date[start:m]
    , rep(0,m-start+1)
    , type="n"
    , ylim=c(0,20)
    #, col=colors[1]
    , main = "US Case Fatality Rate (CFR)" 
    , sub = paste("Date:", heute )
    , xlab="Day",
    , ylab="CFR [%]"
    )     
grid()

for (o in offset) {
    cfr <- deaths$deaths[start:m]/cases$confirmed[(start-o):(m-o)]*100
    lines(cases$date[start:m],cfr,type="l",ylim=c(0,100),col=colors[o%/%7+1],lwd=2)
}

cfr <- deaths$deaths[m]/cases$confirmed[m-offset]*100

options(digits=2)

legend ( 140
    , 15
    #, inset = c(-5,-5)
    , title = "CFR[t,n] = Deaths[t] / Cases [t-n]"
    , legend = paste("n =", offset," days")
    , col = colors
    , cex = 2
    , lty = 1

)

options(digits=3)

legend ( 120
    , 5
    #, inset = c(-5,-5)
    , title = "CFR[today,n]"
    , legend = paste( round(cfr,3),"%")
    , col = colors
    , cex = 2
    , lty = 1
)
dev.off()
