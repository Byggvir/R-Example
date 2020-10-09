#!usr/bin/env Rscript

require(data.table)
library(REST)
# library(ggpubr)

setwd("~/git/R-Example")
source("common/rki_download.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
                     
options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

# Einlesen der Daten aus den aufbereiteten kummulierten Fällen des RKI

kw <- get_rki_kw_csv()

StartKw <- kw$Kw[1]
EndKw <- 52
StartRegAKw <- 27

EndRegAKw <- max(kw$Kw[kw$Tage==7])
#EndRegAKw <- 37

m <- length(kw[,1])

zr <- kw$Kw >= StartRegAKw & kw$Tage==7 &kw$Kw <= EndRegAKw
z <- length(zr)

# EndRegAKw <- EndKw - StartRegAKw

FromTo <- kw$Kw[zr] - StartRegAKw

ra <- lm(log(kw$Cases[zr]) ~ FromTo)

a <- ra$coefficients[1]
b <- ra$coefficients[2]

ci <- confint(ra)
xlim <- c(StartRegAKw,EndKw)
ylim <- c(0,(exp(a+b*(EndKw - StartRegAKw))%/%1000+1)*1000)

png(  paste("png/RKI_regressionKw",EndRegAKw,".png", sep="")
      , width = 1920
      , height = 1080
)

par ( mar = c(10,5,10,5))
plot(kw$Kw[zr]
  , kw$Cases[zr]
  , main = ""
  , sub = paste("Regession auf der Basis der Fallzahlen des RKI von "
                , StartRegAKw
                , ". Kw bis "
                , EndRegAKw
                , ".Kw 2020"
                , sep="")
  , xlab = "Woche"
  , ylab = "Wöchentliche Fallzahlen"
  , xlim = xlim
  , ylim = ylim
  , type = "l"
  , lwd = 3
)

zr <- kw$Kw >= EndRegAKw

par( new = TRUE)

lines ( kw$Kw[zr]
     , kw$Cases[zr]
     , col = "gray"
     , lwd = 3
)

t <- title ( 
    main = "Wöchentliche CoViD-19 Fälle DE mit exponentieller Prognose "
  , cex.main = 4
  )

grid()

par (new = TRUE)

curve( exp(a+b*x)
       , from = 0
       , to = EndKw - StartRegAKw
       
       , col="orange"
       , axes = FALSE
       , xlab = ""
       , xlim = c(0,EndKw - StartRegAKw)
       , ylab = ""
       , ylim = ylim
       , lwd = 3
)
text( EndKw - StartRegAKw
     , exp(a+b*(EndKw - StartRegAKw))
     , round(exp(a+b*(EndKw-StartRegAKw)),0)
     , col = "blue"
     , adj = 1 
     , cex = 3
     )

par ( new = TRUE )

a <- ci[1,2]
b <- ci[2,2]

curve( exp(a+b*x)
       , from = 0
       , to = EndKw - StartRegAKw
       , col="red"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , xlim = c(0,EndKw - StartRegAKw)
       , ylim = ylim
       , lwd = 3
)

par ( new = TRUE )

a <- ci[1,1]
b <- ci[2,1]

curve( exp(a+b*x)
       , from = 0
       , to = EndKw - StartRegAKw
       
       , col="green"
       , axes = FALSE
       , xlab = ""
       , ylab = ""
       , xlim = c(0,EndKw - StartRegAKw)
       , ylim = ylim
       , lwd = 3
)
text( EndKw - StartRegAKw
      , exp(a+b*(EndKw - StartRegAKw))
      , round(exp(a+b*(EndKw-StartRegAKw)),0)
      , col = "green"
      , adj = 1 
      , cex = 3
)

legend ( "bottomright"
         , legend = c( 
               "Fälle innerhalb der RA"
             , "Fälle nach der RA"
             , "Obere Grenze CI 95%"
             , "Mittelere exp. Regression"
             , "Untere Grenze CI 95%"
             )
         , col = c(
               "black"
             , "gray"
             , "red"
             , "orange"
             , "green"
             )
         , lwd = 2
         , cex = 2
         , inset = 0.02
)

legend( 
  "topleft"
  , inset = 0.02
  , title = "Wöchentliche Steigerung CI 95%"
  , paste(round((exp(ci[2,1])-1)*100,1),"% <", round((exp(ra$coefficients[2])-1)*100),"% <",round((exp(ci[2,2])-1)*100,1),"%") 
  , cex = 3)

grid()

dev.off()

