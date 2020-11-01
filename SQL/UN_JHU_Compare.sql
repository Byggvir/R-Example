use COVID19;

delimiter //

drop procedure if exists PopulationByCountry //

create procedure PopulationByCountry ( Country CHAR(255))
begin
    
    select C.CountryCode,C.CountryArea, sum(P.Count) 
    from UNCountry as C 
    join UNPopulation as P 
    on P.CountryCode = C.CountryCode 
    where C.CountryArea = Country
        and P.Year = 2020 P.Sex = 'B' 
    group by Agegroup


    ;
    
end
//
delimiter ;
 
