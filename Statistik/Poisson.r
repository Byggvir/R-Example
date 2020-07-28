#!/usr/bin/r

poisson <- function (k , lambda = 1) {

    if (k == 0) { 
        p <- exp(-lambda)
    }
    else {
        p <- lambda/k*poisson(k-1,lambda)
    }
    return ( p )

}

x <- 1:10

print(poisson(4,1))
