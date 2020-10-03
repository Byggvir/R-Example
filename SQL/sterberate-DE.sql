use Bevoelkerung;

set @P := 1;

select DEU1.Stichtag
    , DEU1.Age
    , (DEU1.Male-DEU2.Male)/DEU2.Male * 100 as 'FR Male'
    , (DEU1.Female-DEU2.Female)/DEU2.Female * 100 as 'FR Female'
#    , FORMAT( @P := @P *(1 + (DEU1.Male-DEU2.Male)/DEU2.Male),4)
from DEU as DEU1 join DEU as DEU2 
on ( year(DEU1.Stichtag) = year(DEU2.Stichtag)+1
     and DEU1.Age = DEU2.Age+1
     ) 
where ( year(DEU1.Stichtag) = 2019 
        and DEU1.Age < 85
    )
;
