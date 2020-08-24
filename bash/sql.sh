#!/bin/bash

sudo rm /tmp/us.csv ; 
mysql -u thomas -p -b < ~/git/R-Example/SQL/US_cases.sql
sudo mv /tmp/us.csv ~/git/R-Example/data/

pushd ~/git/R-Example/COVID19

for r in *.r
do
	echo -e "--- $r ---"
	Rscript $r
done
popd
