use COVID19;

delimiter //

drop procedure if exists CompareCountries //

create procedure CompareCountries (TCC INT)
begin

set @TBEV := (select
        sum(Anzahl) as Anzahl
    from 
        WPP.WPP20191
    where
        CountryCode = TCC
        and Jahr = 2019
        ) 
;


select
      WD.CountryCode as CountryCode
    , C.Region as Region
    , (case when WD.Altersgruppe = 100 then 90 else WD.Altersgruppe div 10 * 10 end ) as Altersgruppe
    , round(sum(WD.Anzahl),0) as OrigTodesfallzahl
    , round(sum(WD.Anzahl / PSCC.Anzahl * PTCC.Anzahl),0) as VergleichsTodesfallzahl
    , round(sum(WDT.Anzahl),0) as Todesfallzahl
from 
    WorldDeaths as WD
join 
    WorldDeaths as WDT
on
    WD.Altersgruppe = WDT.Altersgruppe
    and WD.Sex = WDT.Sex
join
    WPP.WPP20191 as PSCC
on
    WD.CountryCode = PSCC.CountryCode
    and WD.Altersgruppe = PSCC.Altersgruppe
join 
    WPP.WPP20191C as C
on 
    WD.CountryCode = C.CountryCode
join 
    WPP.WPP20191 as PTCC
on
    WD.Altersgruppe = PTCC.Altersgruppe
where
    
    PTCC.CountryCode = TCC
    and PTCC.Jahr = 2019
    and PSCC.Jahr = 2019
    and WDT.CountryCode = TCC
    and WD.Sex = 'B'
group by
    WD.CountryCode
   , Altersgruppe
    ;

end //

drop procedure if exists CompareCountriesSum //

create procedure CompareCountriesSum (TCC INT)
begin

set @TBEV := (select
        sum(Anzahl) as Anzahl
    from 
        WPP.WPP20191
    where
        CountryCode = TCC
        and Jahr = 2019
        ) 
;


select
      WD.CountryCode as CountryCode
    , C.Region as Region
    , round(sum(WD.Anzahl),0) as OrigTodesfallzahl
    , round(sum(WD.Anzahl / PSCC.Anzahl * PTCC.Anzahl),0) as VergleichsTodesfallzahl
    , round(sum(WDT.Anzahl),0) as Todesfallzahl
from 
    WorldDeaths as WD
join 
    WorldDeaths as WDT
on
    WD.Altersgruppe = WDT.Altersgruppe
    and WD.Sex = WDT.Sex
join
    WPP.WPP20191 as PSCC
on
    WD.CountryCode = PSCC.CountryCode
    and WD.Altersgruppe = PSCC.Altersgruppe
join 
    WPP.WPP20191C as C
on 
    WD.CountryCode = C.CountryCode
join 
    WPP.WPP20191 as PTCC
on
    WD.Altersgruppe = PTCC.Altersgruppe
where
    
    PTCC.CountryCode = TCC
    and PTCC.Jahr = 2019
    and PSCC.Jahr = 2019
    and WDT.CountryCode = TCC
    and WD.Sex = 'B'
group by
    WD.CountryCode
    ;

end //

delimiter ;

call CompareCountries (276);
call CompareCountriesSum(276);
;
