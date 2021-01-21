USE COVID19;

DROP TABLE IF EXISTS DEU;

CREATE TABLE DEU (
    sdate DATE
  , age INT
  , male BIGINT
  , female BIGINT
  , INDEX (sdate, age)
  );

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/12411-0006.csv' 
    INTO TABLE DEU 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
