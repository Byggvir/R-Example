USE COVID19;

DROP TABLE IF EXISTS SWE;

CREATE TABLE SWE (
    Altersgruppe INT
  , Fallzahl BIGINT
  , ICU BIGINT 
  , Todesfallzahl BIGINT
  , PRIMARY KEY (Altersgruppe)
  );

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/SWE2019.csv' 
    INTO TABLE SWE 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
