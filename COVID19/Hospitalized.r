#!/usr/bin/env Rscript 
# Plot hospitalized cases

setwd("~/git/R-Example")

d <- read.csv2("data/Hospitalized.csv")

d$sum <- d$YR0 + d$YR5  + d$YR18 + d$YR50 + d$YR65

colors <- c("black", "green","blue", "orange", "darkred", "red")
png ("png/hospitalized.png"
     , width = 1920
     , height = 1080
     )
plot( d$week
    , d$sum
    , type="l"
    , col=colors[1]
    , xlab="Week"
    , ylab="Counts"
    , xlim=c(10,28)
    , ylim=c(0,4000)
    , main="Hospitalized cases by age and week"
    )
lines ( d$week
    , d$YR0
    , type="l"
    , col=colors[2]
    )
lines ( d$week
    , d$YR5
    , type="l"
    , col=colors[3]
    )
lines ( d$week
    , d$YR18
    , type="l"
    , col=colors[4]
    )
lines ( d$week
    , d$YR50
    , type="l"
    , col=colors[5]
    )
lines ( d$week
    , d$YR65
    , type="l"
    , col=colors[6]
    )
legend (
      "topright"
    , legend=c( "sum"
	, "0-4 yr"
	, "5-17 yr"
	, "18-49 yr"
	, "50-64 yr"
	, "65+ yr"
	)
    , lty=rep(1,length(colors))
    , col=colors
    , cex=1
    , text.font=4
    , bg='lightblue'
    )

dev.off()