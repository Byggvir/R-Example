use COVID19;
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
    group by week(t1.day,3);
