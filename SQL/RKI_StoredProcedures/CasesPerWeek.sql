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
    , round(sum(AnzahlTodesfall) /sum(AnzahlFall)*100,1) as CFR
    from RKIFaelle
    where ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 9
    group by BLWestEast(IdBundesland), Kw 
  ;
end
//
drop procedure if exists MinMaxCasesPerWeek //

create procedure MinMaxCasesPerWeek (minW INT, maxW INT)
begin
    
    drop table if exists cpw;
    
    create temporary table cpw ( IdBundesland INT, Kw INT, Cases BIGINT, Deaths BIGINT, PRIMARY KEY (IdBundesland,Kw) )
        select 
            IdBundesland as IdBundesland
            , ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
            , sum(AnzahlFall) as Cases
            , sum(AnzahlTodesfall) as Deaths
        from RKIFaelle
        where ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end )
        group by IdBundesland, Kw 
    ;
    
    drop table if exists minmaxcpw;
    
    create temporary table minmaxcpw ( 
        IdBundesland INT
        , minCases BIGINT
        , minDeaths BIGINT
        , maxCases BIGINT
        , maxDeaths BIGINT
        , PRIMARY KEY (IdBundesland) )
    select 
        IdBundesland as IdBundesland
        , min(Cases) as minCases
        , min(Deaths) as minDeaths
        , max(Cases) as maxCases
        , max(Deaths) as maxDeaths
    from cpw
    where Kw >= minW and Kw <= maxW
    group by IdBundesland
    ;
    
    select 
        M.IdBundesland as BL
        , Bundesland as Bundesland
        , M.minCases as 'Min'
        , max(W1.Kw) as 'minKw'
        , M.maxCases as 'Max'
        , max(W2.Kw) as 'maxKw'
        , max(W2.Kw)-max(W1.Kw) as 'Wochen'
        , round(exp(log(M.maxCases / M.minCases) / (max(W2.Kw)-max(W1.Kw))),4)  as Ratio
    from minmaxcpw as M
    join cpw as W1
        on M.IdBundesland = W1.IdBundesland
        and M.minCases = W1.Cases
    join cpw as W2
        on M.IdBundesland = W2.IdBundesland
        and M.maxCases = W2.Cases
    join Bundesland as B
        on
            M.IdBundesland = B.IdBundesland
    group by M.IdBundesland
    ;
end
//

drop procedure if exists MinMaxCasesPerWeekAgeGroup //

create procedure MinMaxCasesPerWeekAgeGroup (minW INT, maxW INT)
begin
    
    drop table if exists minmaxAG;
    
    create temporary table minmaxAG ( 
        AgeGroup INT
        , minCount INT
        , maxCount INT
        , PRIMARY KEY (AgeGroup) )
    select 
        AgeGroup as AgeGroup
        , min(Count) as minCount
        , max(Count) as maxCount
    from RKI_CasesByAge
    where (Jahr-2020)*53 + Kw >= minW and (Jahr-2020)*53 + Kw  <= maxW
    group by AgeGroup
    ;
    
    select 
        M.AgeGroup as AgeGroup
        , M.minCount as 'Min'
        , max(W1.Kw) as 'minKw'
        , M.maxCount as 'Max'
        , max(W2.Kw) as 'maxKw'
        , max(W2.Kw)-max(W1.Kw) as 'Wochen'
        , round(exp(log(M.maxCount / M.minCount) / (max(W2.Kw)-max(W1.Kw))),4)  as Ratio
    from minmaxAG as M
    join RKI_CasesByAge as W1
        on M.AgeGroup = W1.AgeGroup
        and M.minCount = W1.Count
    join RKI_CasesByAge as W2
        on M.AgeGroup = W2.AgeGroup
        and M.maxCount = W2.Count
    group by M.AgeGroup
    ;
end
//
delimiter ;
