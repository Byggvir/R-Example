#!/usr/bin/re

# (c) 2020 by Thomas Arend

# Licence: GPL-3.0 License

#
# Functions used for Cox Regression Model examples

# Randomize days of survival for one element which will die with probability p per day

survival_time <- function (p) {

    if ( runif(1,0,1) < p ) {

        return (1)
    }
    else {
        return (survival_time(p)+1)
    }

}

# Create a vector with days of survival 

survivals <- function (p,n = 100) {

    s <- NA
    for (i in 1:n) {
        s[i] <- survival_time (p)
    }
    return(s)
}
