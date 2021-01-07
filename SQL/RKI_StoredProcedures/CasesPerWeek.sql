use COVID19;

delimiter //

drop function if exists is_westbl //

create FUNCTION is_westbl (n INT , we BOOL) returns BOOL DETERMINISTIC
begin
  
  set @r := 0;
  if we then set @r := (n < 12);
  else set @r := (n > 11);
  end if;
  return @r;
  
end
//

drop function if exists BLWestEast //

create function BLWestEast (n INT )
RETURNS varchar(20) DETERMINISTIC

BEGIN

  DECLARE dir varchar(20);
  
  if n < 12 then set dir = 'West';
  else set dir = 'Ost';
  end if;
  return dir;
  
end
//

drop procedure if exists CasesPerWeek //

create procedure CasesPerWeek ()
begin

  drop table if exists cpd;
  
  create temporary table cpd ( Date DATE PRIMARY KEY, Kw INT, Cases INT, Deaths INT )
  select 
    t1.date as Date
    , ( case when t1.date > "2021-01-03" then 53+week(t1.date,3) else week(t1.date,3) end ) as Kw
    , t1.cases - t2.cases as Cases
    , t1.deaths - t2.deaths as Deaths
    from rki as t1 
    inner join rki as t2 
    on t1.date=adddate(t2.date,1)
    where t1.cases > t2.cases
  ;
    
  select 
      Kw as Kw
    , sum(Cases) as Cases
    , sum(Deaths) as Deaths
    from cpd
    group by Kw
    order by Kw
  ;

end
//

drop procedure if exists CasesPerWeekBL //

create procedure CasesPerWeekBL (IdBL INT)
begin

   select 
      IdBundesland as BL
    , ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
    , sum(AnzahlFall) as Cases
    , sum(AnzahlTodesfall) as Deaths
    from RKIFaelle
    where IdBundesland = IdBL
    and ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 8
    group by IdBundesland, ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) ;
end
//

drop procedure if exists CasesPerWeekBLWE //

create procedure CasesPerWeekBLWE (West BOOL)
begin

   select 
       ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
    , sum(AnzahlFall) as Cases
    , sum(AnzahlTodesfall) as Deaths
    from RKIFaelle
    where is_westbl(IdBundesland,West)
    and  ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 8
    group by  ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) 
    ;
end
//

drop procedure if exists CasesPerWeekWE //

create procedure CasesPerWeekWE ()
begin

  select 
    ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
    , BLWestEast(IdBundesland) as Bundesland
    , sum(AnzahlFall) as Cases
    , sum(AnzahlTodesfall) as Deaths
    from RKIFaelle
    where  ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 8
    group by BLWestEast(IdBundesland), ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) 
  ;
end
//

delimiter ;
 
