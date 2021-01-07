USE COVID19;

set @Kw := 52;
set @delta :=  2;

select AgeGroup, Deaths,Cases, Deaths / Cases
from (
select (C.AgeGroup div 10 * 10) as AgeGroup
    , sum(C.Count) as Cases
    , D.Deaths as Deaths
from RKIAlter C
join (  
        select AgeGroup 
            , sum(Count) as Deaths
        from SterbeFaelleKw 
        where CalWeek = @Kw
        group by AgeGroup 
    ) as D
on D.AgeGroup = (C.AgeGroup div 10 * 10)
where C.Kw <= @Kw - @delta
group by C.AgeGroup div 10 
order by C.AgeGroup
) as CFR
;

select Deaths,Cases, Deaths / Cases
from (
select 
      ( select sum(Count) from RKIAlter where Kw <= @Kw - @delta ) as Cases
    , ( select sum(Count) from SterbeFaelleKw where CalWeek = @Kw) as Deaths 
) as CFR
;

select CFR.Altersgruppe 'Altersgruppe'
  ,  max(case when CFR.Geschlecht= "M" then CFR else 0 end) Male
  ,  max(case when CFR.Geschlecht= "W" then CFR else 0 end) Female
  ,  max(case when CFR.Geschlecht= "u" then CFR else 0 end) Unknown
from (
    select 
          Altersgruppe
        , Geschlecht
        , sum(AnzahlTodesfall) / sum(AnzahlFall) CFR
    from RKIFaelle
    where IdBundesland=5
    group by Altersgruppe,Geschlecht
) as CFR 
group by CFR.Altersgruppe;
