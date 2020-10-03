USE Bevoelkerung;

CREATE TABLE DEU (
    Stichtag DATE
  , Age INT
  , Male BIGINT
  , Female BIGINT
  , Insgesamt BIGINT
  , INDEX (Stichtag, Age)
  );

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/DE2015-2019.csv' 
    INTO TABLE DEU 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
