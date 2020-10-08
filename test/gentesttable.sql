use test;

delimiter //

drop procedure if exists createtesttable //

create procedure createtesttable (n INT)
begin
  drop table if exists test;
  create table if not exists test
	( nr BIGINT PRIMARY KEY AUTO_INCREMENT,
	  zahl BIGINT 
	) ;
  set @i=1;
  repeat
    insert into test set zahl=@i*@i;
    set @i=@i+1; 
  until @i > n end repeat; 
end
//
delimiter ;

call createtesttable (10000);
