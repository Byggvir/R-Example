USE COVID19;

CREATE TEMPORARY TABLE DailyCases
      SELECT 
        t1.date AS date
        , t1.cases - t2.cases AS cases
      FROM rki AS t1
      JOIN rki AS t2
      ON t1.date = adddate(t2.date,1);

CREATE TEMPORARY TABLE WeeklyCases 
SELECT 
    week(date,3) AS Kw
    , sum(cases) AS WCases 
FROM DailyCases
GROUP BY Kw ;

select "Tag","WTag","Kw","Cases","WCases","WTagAnteil"
UNION ALL
SELECT 
    date AS Tag
    , weekday(date) as WTag
    , Kw as Kw
    , Cases as Cases
    , WCases as WCases
    , round(Cases/WCases,8) AS WTagAnteil
FROM DailyCases
JOIN WeeklyCases
ON week(date,3) = Kw
INTO OUTFILE '/tmp/week.csv' FIELDS TERMINATED by ','; ;
