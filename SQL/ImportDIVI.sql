USE COVID19;

DROP TABLE IF EXISTS DIVI;

CREATE TABLE DIVI (
    Tag DATE
  , Anzahl BIGINT
  , PRIMARY KEY (Tag)
  );

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/divi.csv' 
    INTO TABLE DIVI 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
