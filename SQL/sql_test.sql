USE COVID19;

DROP TABLE IF EXISTS delta;

CREATE TEMPORARY TABLE delta 
    SELECT t1.date as date, t1.cases - t2.cases as cases
    FROM rki AS t1 
    JOIN rki AS t2 
        ON t1.date = adddate(t2.date,1)
    ;
    
select "WTag", "WAnteil"
union all
SELECT weekday(date) as WTag, cases/WSum as WAnteil
FROM delta as d1
INNER JOIN ( SELECT week(d2.date,3) as Kw, sum(cases) as WSum 
       FROM delta as d2
       where week(d2.date,3) > 9 and week(d2.date,3) < 43
       group by Kw ) as d3
on week(d1.date,3) = d3.Kw;
