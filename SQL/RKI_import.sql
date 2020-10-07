use COVID19;

select 
    t1.day
    , WEEK(t1.day,1) as Kw
    , WEEKDAY(t1.day) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases-t2.cases as incCases
    , t1.deaths-t2.deaths as incDeaths
from rki as t1 
inner join rki as t2 
on t1.day=adddate(t2.day,1)
into OUTFILE '/tmp/RKI_nach_Tag.csv' FIELDS TERMINATED by ',';


select * from 
( select 
    week(t1.day) as Kalenderwoche
    , count(week(t1.day)) as Tage
    , sum(t1.cases-t2.cases) as Cases
    , sum(t1.deaths-t2.deaths) as Deaths
from rki as t1
inner join rki as t2 
on t1.day=adddate(t2.day,1) 
group by week(t1.day)
) as t0
where t0.Tage = 7
into OUTFILE '/tmp/RKI_nach_Kw.csv' FIELDS TERMINATED by ',';