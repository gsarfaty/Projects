library(readxl)
library(tidyverse)

#PLHIV
epi<-read_xlsx("2019-03-01 DataPack_Zambia_V1.12.xlsx", sheet="Epi Cascade I",skip=4, col_names=TRUE)


epi_clean <-epi %>%
separate(PSNU, c("psnu","psnuuid"), sep = "\\(") %>%
mutate(psnuuid = str_remove(psnuuid, "\\)")) %>%
select(psnu,psnuuid,Age,AgeCoarse,Sex,`PLHIV.NA.Age/Sex/HIVStatus.20T`) %>%
rename(plhiv=`PLHIV.NA.Age/Sex/HIVStatus.20T`) %>%
group_by(psnu,psnuuid) %>%
summarise_at(vars(plhiv),funs(sum(.,na.rm=TRUE)))
View(epi_clean)

#FY15
FY15 <-read_tsv("MER_Structured_Dataset_PSNU_IM_FY15-16_20190215_v1_1_Zambia.txt") 


TX_CURR15<-FY15%>% 
  filter(indicator=="TX_CURR" & standardizedDisaggregate=="Total Numerator") %>% 
  group_by(PSNUuid,PSNU,indicator) %>% 
  summarise_at(vars(FY2015Q4),funs(sum(.,na.rm=TRUE))) %>%
  unite(indicator_unite,indicator,sep="_") %>% 
  spread(indicator_unite, FY2015Q4) %>% 
  rename(TX_CURR15=TX_CURR)


#FY19Q1
FY19 <-read_tsv("MER_Structured_Dataset_PSNU_IM_FY17-19_20190215_v1_2_Zambia.txt")

TX_CURR19 <-FY19 %>%
  filter(indicator=="TX_CURR" & standardizedDisaggregate=="Total Numerator") %>% 
  group_by(PSNUuid,PSNU,indicator) %>% 
  summarise_at(vars(FY2019Q1),funs(sum(.,na.rm=TRUE))) %>%
  unite(indicator_unite,indicator,sep="_") %>% 
  spread(indicator_unite, FY2019Q1) %>% 
  rename(TX_CURR19=TX_CURR)

#merge
Final_1 <-merge(epi_clean,TX_CURR15, by.x="psnuuid", by.y="PSNUuid", all=TRUE)
Final_2 <-merge(Final_1,TX_CURR19, by.x="psnuuid", by.y="PSNUuid", all=TRUE)

write_tsv(Final_2, "Zambia_ARTCov_FY15to19.txt")
