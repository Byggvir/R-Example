USE COVID19;

DROP TABLE IF EXISTS DeathsPerDay;

CREATE TABLE IF NOT EXISTS DeathsPerDay (
    Year INT 
  , JD INT
  , Deaths INT
  , PRIMARY KEY (Year, JD)
  );

DROP TABLE IF EXISTS DeathsPerCW;

CREATE TABLE IF NOT EXISTS DeathsPerCW (
    Year INT 
  , calweek INT
  , AGlow INT
  , AGhigh INT
  , Deaths INT
  , PRIMARY KEY (Year, calweek, AGlow, AGhigh)
  );
