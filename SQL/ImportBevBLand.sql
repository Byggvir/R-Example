USE Bevoelkerung;

DROP TABLE IF EXISTS BevBLand;

CREATE TABLE BevBLand (
    Id INT PRIMARY KEY
  , Name VARCHAR(32)
  , Anzahl BIGINT
  , Deusche BIGINT
  , Nichtdeutsche BIGINT
  , INDEX (Name)
  );

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/BevBLand.csv' 
    INTO TABLE BevBLand 
    FIELDS TERMINATED BY ','
    ENCLOSED BY '"'
    IGNORE 2 ROWS;
