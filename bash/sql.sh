#!/bin/bash

sudo rm /tmp/us.csv ; 
mysql -u thomas -p -b < ~/git/R-Example/SQL/US_cases.sql

pushd ~/git/R-Example/COVID19

for r in *.r
do
	echo -e "--- $r ---"
	Rscript $r
done
popd
