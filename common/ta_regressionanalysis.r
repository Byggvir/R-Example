#
# ---- Zeichnen des Ergebnisses einer exponentiellen Regressiosnanalyse auf einem Plot
#

plotregression <- function( a , b, xlim = c(0,1),  linecol = c("green","orange","red")) {
  
  for (i in 1:3 ) {
    par (new = TRUE)    
    curve( exp(a[i]+b[i]*x)
           , from = xlim[1]
           , to = xlim[2]
           , col = linecol[i]
           , axes = FALSE
           , xlab = ""
           , ylab = ""
           , xlim = xlim
           , ylim = ylim
           , lwd = 3
    )
    text( xlim[2]
          , exp(a[i]+b[i]*xlim[2])
          , round(exp(a[i]+b[i]*xlim[2]),0)
          , col = linecol[i]
          , adj = 0
          , cex = 1
    )
    
  }
  
}

# ----11
