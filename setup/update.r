#!/usr/bin/env RScript
ip <- installed.packages()
print(ip[,1])
update.packages(checkBuilt = TRUE, ask = FALSE)

