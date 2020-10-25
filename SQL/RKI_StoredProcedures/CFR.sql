use COVID19;

delimiter //

drop procedure if exists CFR //

create procedure CFR ()
begin
    set @f := 0 ;
    set @t := 0 ;
    select 
        Kw
        , (@t:=@t+Tote)
        , (@f:=@f+Faelle)
        , @t/@f*100 as CFR 
        from (
            select 
                week(Refdatum,3) as Kw
                , sum(AnzahlTodesfall) as Tote
                , sum(AnzahlFall) as Faelle
            from RKIFaelle 
            group by week(Refdatum,3)) as CFR
    ;
end
//
delimiter ;
