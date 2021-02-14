USE COVID19;

delimiter //

DROP PROCEDURE IF EXISTS LandkreisTable //

CREATE PROCEDURE LandkreisTable (IdLK INT)
BEGIN

select 
      Kw
    , max( case when Altersgruppe = 'A00-A04' then Anzahl else 0 end ) as 'A00-A04'
    , max( case when Altersgruppe = 'A05-A14' then Anzahl else 0 end ) as 'A05-A14'
    , max( case when Altersgruppe = 'A15-A34' then Anzahl else 0 end ) as 'A15-A34'
    , max( case when Altersgruppe = 'A35-A59' then Anzahl else 0 end ) as 'A35-A59'
    , max( case when Altersgruppe = 'A60-A79' then Anzahl else 0 end ) as 'A60-A79'
    , max( case when Altersgruppe = 'A80+'    then Anzahl else 0 end ) as 'A80+'
    , max( case when Altersgruppe = 'unbekan' then Anzahl else 0 end ) as 'unbekan'
    , sum(Anzahl) as Summe
from (
    select 
        ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
        , Altersgruppe as Altersgruppe
        , sum(AnzahlFall) as Anzahl 
    from RKIFaelle
    where 
        IdLandkreis = IdLK
    group by 
          Kw
        , Altersgruppe
    ) as A
group by
    A.Kw
having Kw >= 35
;
end
//

DROP PROCEDURE IF EXISTS LandkreisAltersgruppen //

CREATE PROCEDURE LandkreisAltersgruppen (IdLK INT)
BEGIN

    select 
        ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
        , Altersgruppe as Altersgruppe
        , sum(AnzahlFall) as Anzahl 
    from RKIFaelle
    where 
        IdLandkreis = IdLK
    group by 
          Kw
        , Altersgruppe

;
end
//


DROP PROCEDURE IF EXISTS Landkreis //

CREATE PROCEDURE Landkreis (IdLK INT)
BEGIN

    select 
        ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
        , sum(AnzahlFall) as Anzahl 
    from RKIFaelle
    where 
        IdLandkreis = IdLK
    group by 
          Kw

;
end
//

delimiter ;
