USE COVID19;
    
select S.AgeGroup 'Age group'
  ,  max(case when S.Sex = 0 then Summe else 0 end) Male
  , max(case when S.Sex = 1 then Summe else 0 end) Female
from (
    select AgeGroup, Sex, sum(count) as Summe
    from SterbeFaelleKw where CalWeek=51 group by AgeGroup,Sex having AgeGroup <110
) as S group by S.AgeGroup;
