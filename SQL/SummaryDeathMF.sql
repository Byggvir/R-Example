USE COVID19;

set @m:= ( select sum(Count) from SterbeFaelleKw where Jahr=2021 and Kw=1 and Sex=0 );
set @f:= ( select sum(Count) from SterbeFaelleKw where Jahr=2021 and Kw=1 and Sex=1 );

select 
    AgeGroup as Altersgruppe
    , round(max(case when Sex=0 then Count else 0 end ) /@m *100,1) as 'Männer'
    , round(max(case when Sex=1 then Count else 0 end ) /  @f * 100,1) as 'Frauen'
from ( 
    select 
        AgeGroup
        , Sex
        , Count 
    from SterbeFaelleKw 
    where 
        Jahr=2021 
        and Kw=1 
    group by AgeGroup, Sex
    ) 
    as T 
group by AgeGroup;
