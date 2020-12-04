use COVID19;

delimiter //

drop procedure if exists CasesPerWeek //

create procedure CasesPerWeek ()
begin
   set @i := 1 ;
   create temporary table cpd ( Date DATE, Kw INT, Cases INT, Deaths INT )
   select 
    t1.date as Date
    , t1.cases - t2.cases as Cases
    , t1.deaths - t2.deaths as Deaths
    from rki as t1 
    inner join rki as t2 
    on t1.date=adddate(t2.date,1)
    where t1.cases > t2.cases;
    
   set @i := 1 ;
   select 
    (@i:=@i+1) as NrWeek
    , week(date,3) as Kw
    , sum(Cases) as Cases
    , sum(Deaths) as Deaths
    from cpd
    group by week(date,3)
    ;
end
//
delimiter ;
 
