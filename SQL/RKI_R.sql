USE COVID19;
SELECT week(a.date,3) AS Kw
    , (max(a.cases)-min(a.cases)) AS cases 
    , (max(a.deaths)-min(a.deaths)) AS deaths 
    , (max(a.cases)-min(a.cases))/(max(b.cases)-min(b.cases)) AS Rcases 
    , (max(a.deaths)-min(a.deaths))/(max(b.deaths)-min(b.deaths)) AS Rdeaths 
FROM rki AS a 
INNER JOIN rki AS b 
    ON week(a.date,3) = week(b.date,3) + 1 
GROUP BY week(a.date,3);
