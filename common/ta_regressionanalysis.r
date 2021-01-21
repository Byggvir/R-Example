#
# ---- Zeichnen des Ergebnisses einer exponentiellen Regressiosnanalyse auf einem Plot
#

plotregression <- function( a , b, xlim = c(0,1), ylim = c(0,3), linecol = c("green","orange","red")) {
  
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
           , lty = 3
    )
    text( xlim[2]
          , exp(a[i]+b[i]*xlim[2])
          , round(exp(a[i]+b[i]*xlim[2]),0)
          , col = linecol[i]
          , adj = 1
          , cex = 2
    )
    
  }
  
}

plotExpRegression <- function( a , b, From = 0, To = 1, xlim = c(0,1), ylim = c(0,3), linecol = c("green","orange","red")) {
  
  for (i in 1:3 ) {
    par (new = TRUE)    
    curve( exp(a[i]+b[i]*x)
           , from = From
           , to = To
           , col = linecol[i]
           , axes = FALSE
           , xlab = ""
           , ylab = ""
           , xlim = xlim
           , ylim = ylim
           , lwd = 3
           , lty = 3
    )
    text( xlim[2]
          , exp(a[i]+b[i]*xlim[2])
          , round(exp(a[i]+b[i]*xlim[2]),0)
          , col = linecol[i]
          , adj = 1
          , cex = 2
    )
    
  }
  
}
regression_label <- function( x , a , b, xlim = c(0,1), ylim = c(0,3), linecol = c("green","orange","red")) {
  
  for (i in 1:3 ) {
    text( x
          , exp(a[i]+b[i]*x)
          , round(exp(a[i]+b[i]*x),0)
          , col = linecol[i]
          , adj = 0.5
          , cex = 1
    )
    
  }
  
}

# ----
