use COVID19;

delimiter //

drop procedure if exists CasesPerDay //

create procedure CasesPerDay ()
begin
   set @i := 1 ;
   select 
    t1.date as Date
    , (@i:=@i+1) as Day
    , WEEK(t1.date,3) as Kw
    , WEEKDAY(t1.date) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases - t2.cases as incCases
    , t1.deaths - t2.deaths as incDeaths
    from rki as t1 
    inner join rki as t2 
    on t1.date=adddate(t2.date,1)
    where t1.cases > t2.cases;
end
//
delimiter ;
 
