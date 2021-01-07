USE Bevoelkerung;

DROP TABLE IF EXISTS DEU;

CREATE TABLE DEU (
    Stichtag DATE
  , Age INT
  , Male BIGINT
  , Female BIGINT
  , Insgesamt BIGINT
  , INDEX (Stichtag, Age)
  );

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/12411-0006.csv' 
    INTO TABLE DEU 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
