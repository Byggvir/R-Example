use COVID19;

delimiter //

drop procedure if exists CasesPerWeek //

create procedure CasesPerWeek ()
begin
   set @i := 1 ;
   select 
    (@i:=@i+1) as NrWeek
    , week(date,3) as Kw
    , max(cases) - min(cases) as Cases
    , max(deaths) - min(deaths) as Deaths
    from rki
    group by week(date,3)
    ;
end
//
delimiter ;
 
