USE COVID19;

select CFR.Altersgruppe 'Altersgruppe'
  ,  max(case when CFR.Geschlecht= "M" then Deaths else 0 end) DeathsMale
  ,  max(case when CFR.Geschlecht= "M" then Cases else 0 end) CasesMale
  ,  max(case when CFR.Geschlecht= "W" then Deaths else 0 end) DeathsFemale
  ,  max(case when CFR.Geschlecht= "W" then Cases else 0 end) CasesFemale
  ,  max(case when CFR.Geschlecht= "u" then Deaths else 0 end) DeathsUnknown
  ,  max(case when CFR.Geschlecht= "u" then Cases else 0 end) CasesUnknown
from (
    select 
          Altersgruppe
        , Geschlecht
        , sum(AnzahlTodesfall) Deaths
        , sum(AnzahlFall) Cases
    from RKIFaelle
    where IdBundesland=5
    group by Altersgruppe,Geschlecht
) as CFR 
group by CFR.Altersgruppe;


select Altersgruppe, round(Male/Female,6) from (
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
group by CFR.Altersgruppe
) as RR;
