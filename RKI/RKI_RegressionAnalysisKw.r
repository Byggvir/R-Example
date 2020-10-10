#!usr/bin/env Rscript

require(data.table)
library(REST)
# library(ggpubr)

setwd("~/git/R-Example")
source("common/rki_download.r")
source("common/ta_regressionanalysis.r")

CI <- 0.95

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
# EndKw <- max(kw$Kw)
EndKw <- 45
StartRegAKw <- 36

EndRegAKw <- max(kw$Kw[kw$Tage==7])

for (EndRegAKw in 40) {

m <- length(kw[,1])

zr <- kw$Kw >= StartRegAKw & kw$Tage==7 &kw$Kw <= EndRegAKw
z <- length(zr)

# EndRegAKw <- EndKw - StartRegAKw

FromTo <- kw$Kw[zr] - StartRegAKw

ra1 <- lm(log(kw$Cases[zr]) ~ FromTo)
ci1 <- confint(ra1,level = CI)

a <- c( ci1[1,1], ra1$coefficients[1] , ci1[1,2])
b <- c( ci1[2,1], ra1$coefficients[2] , ci1[2,2])

xlim <- c(StartRegAKw,EndKw)
ylim <- c(  0
            , ( max(
              c(   exp(a)
                   , exp(a+b*(EndKw - StartRegAKw))
              ) 
            ) %/% 1000 + 1 ) * 1000
)

png(  paste("png/RKI_RegressionKw",StartRegAKw,"-",EndRegAKw,"-",EndKw,".png", sep="")
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

zr2 <- kw$Kw >= EndRegAKw

par( new = TRUE)

lines ( kw$Kw[zr2]
     , kw$Cases[zr2]
     , col = "gray"
     , lwd = 3
)

t <- title ( 
    main = paste( "Wöchentliche CoViD-19 Fälle DE mit exponentieller Prognose bis Kw", EndKw)
  , cex.main = 3
  )

grid()

print(exp(a))

plotregression(a, b, xlim= c( 0, EndKw - StartRegAKw ) )

lr <- ifelse (b[2] > 0 ,"left", "right")

legend ( lr
         , legend = c( 
           "Fälle innerhalb der RA"
           , "Fälle nach der RA"
           , paste("Obere Grenze CI ", CI*100, "%", sep="" )
           , "Mittelere exp. Regression"
           , paste("Untere Grenze CI  ", CI*100, "%", sep="" )
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
  "top"
  , inset = 0.02
  , title = paste("Wöchentliche Steigerung CI ", CI*100, "%", sep="" )
  , paste(round((exp(b[1])-1)*100,1),"% <", round((exp(b[2])-1)*100),"% <",round((exp(b[3])-1)*100,1),"%" ) 
  , cex = 3
  )

# --- Lineare Regesssion


# ra2 <- lm(kw$Cases[zr] ~ FromTo)
# ci2 <- confint(ra2,level = CI )
# 
# a <- c( ci2[1,1], ra2$coefficients[1] , ci2[1,2])
# b <- c( ci2[2,1], ra2$coefficients[2] , ci2[2,2])
# 
# print(a)
# 
# linecol = c("green","orange","red")
# 
# for ( i in 1:3 ) {
#   par ( new = TRUE )
#   curve(  a[i] + b[i] * x
#       , from = 0
#       , to = EndKw - StartRegAKw
#       , col = linecol[i]
#       , xlab = ""
#       , ylab = ""
#       , xlim = c(0,EndKw - StartRegAKw)
#       , ylim = ylim
#       , lwd = 3
#   )
# }
# 
# print(summary(ra1))
# print(summary(ra2))

dev.off()

}
