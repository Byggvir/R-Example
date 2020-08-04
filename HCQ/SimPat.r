#!/usr/bin/r
library(gridExtra)

# pdf("Prediction.pdf", height = 11, width = 8.5)

require(data.table)

sdata = data.table(
      treatment = c("Total","Nm","HCQ","AZM","AZM+HCQ", "UK HCQ", "UK Nm")
    , patients = c(2541,409,1202,147,783,1581,3155)
    , deaths = c(460,108,162,33,157,418,788)
    , age_mean = c(63.7,68.1,63.2,63.4,62.3,65.2,65.4)
    , age_sd = c(16.5,18.9,15.6,17.3,15.9,15.2,15.4)
)

age <- 1:120

MnCFR <- rep(0,120)
MnCFR[1:9]   <- 0.0002
MnCFR[10:19] <- 0.0002
MnCFR[20:29] <- 0.0003
MnCFR[30:39] <- 0.0008
MnCFR[40:49] <- 0.0026
MnCFR[50:59] <- 0.0082
MnCFR[60:69] <- 0.0415
MnCFR[70:79] <- 0.1388
MnCFR[80:89] <- 0.2530
MnCFR[90:99] <- 0.3154
MnCFR[100:120] <- 0.3401

inhospital <- seq(from=2.00,to=2.02,by=0.001)

for (k in 1:length(inhospital) ) {

CFR <- MnCFR*inhospital[k]

lt<- length(sdata$treatment)

result = data.table(
      treatment = sdata$treatment
    , study = sdata$deaths/sdata$patients
    , prediction = rep(0,lt)
    , CI_l  = rep(0,lt)
    , CI_u  = rep(0,lt)
    , inCI  = rep(FALSE,lt)
    )

options(digits=4)

message("New ---")

print (inhospital[k])

for ( j in 1:lt) {

a <- 1:120
pa <- pnorm(a+1,mean=sdata$age_mean[j],sd=sdata$age_sd[j])-pnorm(a,mean=sdata$age_mean[j],sd=sdata$age_sd[j])

result$prediction[j] = sum(CFR*pa)

tempSD <- sqrt((1-result$prediction[j])*result$prediction[j]/sdata$patients[j])

result$CI_l[j] = result$prediction[j] - 2 * tempSD
result$CI_u[j] = result$prediction[j] + 2 * tempSD
}
result$inCI <- (result$study >= result$CI_l) & (result$study <= result$CI_u)

print(result)
# grid.table(result)

}

dev.off()
