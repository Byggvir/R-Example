USE COVID19;

DROP TABLE IF EXISTS us_cases;

CREATE TABLE us_cases (UID BIGINT, date INT, count INT , INDEX (UID, date));

DROP TABLE IF EXISTS us_deaths;

CREATE TABLE us_deaths (UID BIGINT, date INT, count INT , INDEX (UID, date));

LOCK TABLES `us_cases` WRITE;
LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/US-confirmed.csv' 
    INTO TABLE us_cases 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
UNLOCK TABLES;

LOCK TABLES `us_deaths` WRITE;

LOAD DATA LOCAL INFILE '/home/thomas/git/R-Example/data/US-deaths.csv' 
    INTO TABLE us_deaths 
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
UNLOCK TABLES;

SELECT "Date","Kw","WTag","Cases","Deaths"  
UNION all ( 
    SELECT  us_cases.date as date , 
            ((us_cases.date + 2) DIV 7 + 4) as Kw ,
            ((us_cases.date + 2) % 7) as WTag ,
            sum(us_cases.count) as Cases ,
            sum(us_deaths.count) as Deaths
            FROM us_cases 
            INNER JOIN us_deaths 
                ON  (   us_cases.UID = us_deaths.UID 
                    AND us_cases.date=us_deaths.date
                ) 
            GROUP BY ( us_cases.date ) 
            INTO OUTFILE '/tmp/us.csv' 
                FIELDS TERMINATED BY ',' );
