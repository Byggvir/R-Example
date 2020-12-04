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
delimiter ;
 
