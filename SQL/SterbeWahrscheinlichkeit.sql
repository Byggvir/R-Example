USE Bevoelkerung;

set @Alter = 62;
set @i=0;

delimiter //

drop procedure if exists AgeTab //

create procedure AgeTab (n INT)
begin

    create temporary table if not exists Age 
        (Age int NOT NULL PRIMARY KEY)
        ;
    repeat
    insert into Age values (@i);
    set @i = @i + 1;
    until @i>n end repeat;
end
//

delimiter ;

create temporary table if not exists SumAge 
    select 
        Age
        , sum(Male) as SMale
        , sum(Female) as SFemale
    from DEU
    where year(Stichtag) >2009
    group by Age
;


select 
    D1.Age
    , 1 - D1.SMale
    / D2.SMale as FRMale
    , 1 - D1.SFemale 
    / D2.SFemale FRFemale
from SumAge as D1
join SumAge as D2
on D1.Age=D2.Age + 1
group by D1.Age;

select * from SumAge;

select D1.Age,D2.Age,1-D1.Male/D2.Male,1-D1.Female/D2.Female 
from DEU as D1 
join (
    select Age,Male,Female 
    from DEU as D2 
    where 
        year(Stichtag)-Age=2019-@Alter 
        and year(Stichtag)>=1990
) as D2
on D1.Age=D2.Age + 1

where 
    year(D1.Stichtag)-D1.Age=2019-@Alter 
    and year(D1.Stichtag)>1990
;
