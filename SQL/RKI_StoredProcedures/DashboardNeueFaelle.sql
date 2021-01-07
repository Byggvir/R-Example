use COVID19;

delimiter //

drop procedure if exists Neuefaelle //

create procedure Neuefaelle ()
begin

    SELECT distinct A.Meldedatum, dayofweek(A.Meldedatum),B.Infizierte
    FROM RKIFaelle as A
    LEFT JOIN (SELECT Meldedatum, sum(AnzahlFall) AS Infizierte
    FROM RKIFaelle as C WHERE NeuerFall <> 0 GROUP BY Meldedatum) AS B
    ON A.Meldedatum=B.Meldedatum where A.Meldedatum > CURRENT_DATE() - 14 ;

end
//

drop procedure if exists FallzahlenGestern //

create procedure FallzahlenGestern ()
begin

  set @MD := (select max(Meldedatum) from RKIFaelle where NeuerFall <> 0);
  SELECT
    @MD as Meldedatum  
    ,  max(case when F.Outcome ='Cases' then F.Counts else 0 end ) as Cases
    , max(case when F.Outcome ='Deaths' then F.Counts else 0 end ) as Deaths    
  from (
    SELECT 'Cases' as Outcome, sum(AnzahlFall) AS Counts
    FROM RKIFaelle as C WHERE NeuerFall <> 0
    UNION ALL
    SELECT 'Deaths' as Outcome, sum(AnzahlTodesfall) AS Counts
    FROM RKIFaelle as C WHERE NeuerTodesfall <> 0
  ) as F
  ;

end
//
drop procedure if exists UpdateYesterday //

create procedure UpdateYesterday ()
begin

  set @MD := (select max(Meldedatum) from RKIFaelle);
  INSERT INTO rki SELECT
    @MD 
    ,  max(case when F.Outcome ='Cases' then F.Counts else 0 end )
    , max(case when F.Outcome ='Deaths' then F.Counts else 0 end )    
  from (
    SELECT 'Cases' as Outcome, sum(AnzahlFall) AS Counts
    FROM RKIFaelle as C WHERE NeuerFall > -1
    UNION ALL
    SELECT 'Deaths' as Outcome, sum(AnzahlTodesfall) AS Counts
    FROM RKIFaelle as C WHERE NeuerTodesfall > -1
  ) as F
  ;

end
//

delimiter ;
 
