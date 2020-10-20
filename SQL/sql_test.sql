USE COVID19;


SELECT weekday(date) as WTag, sum(cases)/(select max(cases) from rki) as Anteil
FROM (
    SELECT t1.date as date, t1.cases - t2.cases as cases
    FROM rki AS t1 
    JOIN rki AS t2 
        ON t1.date = adddate(t2.date,1)
    ) AS t3
GROUP BY WTag;
