USE COVID19;

DROP TABLE IF EXISTS Testungen;

CREATE TABLE Testungen ( Datum datetime, Tage INT, Altersgruppe INT, Testungen BIGINT, Positiv BIGINT, INDEX (Datum , Altersgruppe));

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/TestungenAlter.csv' 
    INTO TABLE Testungen 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
