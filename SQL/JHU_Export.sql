use COVID19;

set @i:=0;

select "Name","Date","Day","Kw","WTag","Cases","Deaths","incCases","incDeaths"
union all
select
      t1.name
    , t1.date as Date
    , (@i:=@i+1) as Day
    , WEEK(t1.date,3) as Kw
    , WEEKDAY(t1.date) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases-t2.cases as incCases
    , t1.deaths-t2.deaths as incDeaths
from jhu as t1 
inner join jhu as t2 
on t1.date=adddate(t2.date,1)
into OUTFILE '/tmp/JHU_nach_Tag.csv' FIELDS TERMINATED by ',';
