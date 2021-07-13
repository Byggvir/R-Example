#!/usr/bin/r

# Beispiel um Daten aus LibreOffice statt CSV zu lesen.

require("readODS")

s <- as.matrix(read_ods("data/SterbeFälleAlter.ods",sheet=2))

barplot(s[,2:3]
        , beside=TRUE
)
axis(1,at=1:23,labels = c(seq(0,100,10)," ",seq(0,100,10)))
