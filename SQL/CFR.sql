USE COVID19;

delimiter //

drop procedure if exists PredictCases //

create procedure PredictCases ()
begin

set @Jahr := ( select max(Jahr) from RKI_CasesByAge);
set @Kw := ( select max(Kw) from RKI_CasesByAge where Jahr = @Jahr);

select 
    C.AgeGroup
    , max(CFR)
    , F.CumulatedCount
    , round(max(CFR) * F.CumulatedCount,0)
    
from (
    select 
        D.Jahr
        , D.Kw
        , D.AgeGroup
        , sum(D.count) / F.Cases as CFR
    from RKI_DeathsByAgeKw as D
    join ( 
        select 
            Jahr
            , Kw
            , AgeGroup div 10 * 10 as AgeGroup
            , sum(CumulatedCount) as Cases 
        from RKI_CasesByAge
        group by 
            Jahr,Kw, AgeGroup div 10 * 10 
        ) as F
    on 
        D.Jahr = F.Jahr
        and D.Kw = F.Kw
        and D.AgeGroup = F.AgeGroup
    group by 
        D.Jahr
        , D.Kw
        , D.AgeGroup
    ) as C 
join ( 
    select 
        AgeGroup div 10 * 10 as AgeGroup
        , sum(CumulatedCount) as CumulatedCount
    from RKI_CasesByAge
    where
        Jahr = @Jahr
        and Kw = @Kw
    group by 
        Jahr,Kw, AgeGroup div 10 * 10
    ) as F
on
    F.AgeGroup = C.AgeGroup
where 
    C.Kw > 15 or C.Jahr=2021 
group by 
    C.AgeGroup;

end
//
delimiter ;

call PredictCases();
