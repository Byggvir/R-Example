use COVID19;

set @i:=0;

select "Date","Day","Kw","WTag","Cases","Deaths","incCases","incDeaths"
union all
select 
    t1.day as Date
    , (@i:=@i+1) as Day
    , WEEK(t1.day,3) as Kw
    , WEEKDAY(t1.day) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases-t2.cases as incCases
    , t1.deaths-t2.deaths as incDeaths
from rki as t1 
inner join rki as t2 
on t1.day=adddate(t2.day,1)
into OUTFILE '/tmp/RKI_nach_Tag.csv' FIELDS TERMINATED by ',';

set @i=0;

select "Woche","Kw","Tage","Cases","Deaths","WMin","WMax"
union all
select 
    (@i:=@i+1) as Woche
    , week(t1.day,3) as Kalenderwoche
    , count(week(t1.day,3)) as Tage
    , sum(t1.cases-t2.cases) as Cases
    , sum(t1.deaths-t2.deaths) as Deaths
    , min(t1.cases-t2.cases) as WMin
    , max(t1.cases-t2.cases) as WMax 
    from rki as t1
    inner join rki as t2 
    on t1.day=adddate(t2.day,1)
    group by week(t1.day,3)
into OUTFILE '/tmp/RKI_nach_Kw.csv' FIELDS TERMINATED by ',';
