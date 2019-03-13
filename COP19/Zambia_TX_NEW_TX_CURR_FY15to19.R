library(readxl)
library(tidyverse)

#PLHIV
epi<-read_xlsx("2019-03-01 DataPack_Zambia_V1.12.xlsx", sheet="Epi Cascade I",skip=4, col_names=TRUE)


epi_clean <-epi %>%
separate(PSNU, c("psnu","psnuuid"), sep = "\\(") %>%
mutate(psnuuid = str_remove(psnuuid, "\\)")) %>%
select(psnu,psnuuid,Age,AgeCoarse,Sex,`PLHIV.NA.Age/Sex/HIVStatus.20T`,`TX_CURR_SUBNAT.N.Age/Sex/HIVStatus.20T`) %>%
rename(plhiv=`PLHIV.NA.Age/Sex/HIVStatus.20T`,
      tx_curr_20=`TX_CURR_SUBNAT.N.Age/Sex/HIVStatus.20T`)%>%
group_by(psnu,psnuuid) %>%
summarise_at(vars(plhiv:tx_curr_20),funs(sum(.,na.rm=TRUE)))



#FY15/16
FY15_16 <-read_tsv("MER_Structured_Dataset_PSNU_IM_FY15-16_20190215_v1_1_Zambia.txt") 


TX_15_16<-FY15_16%>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW") & standardizedDisaggregate=="Total Numerator") %>% 
  group_by(PSNUuid,PSNU,SNUPrioritization,indicator) %>% 
  summarise_at(vars(FY2015Q4:FY2016Q2),funs(sum(.,na.rm=TRUE))) %>%
  select(PSNUuid,PSNU,SNUPrioritization,indicator,FY2015Q4,FY2016Q2) %>% 
  gather(qtr, value, starts_with("fy2")) %>% 
  unite(indicator_unite,indicator,qtr,sep = "_") %>% 
  spread(indicator_unite,value) %>% 
  rename(fy16priority=SNUPrioritization)



#FY19Q1
FY19 <-read_tsv("MER_Structured_Dataset_PSNU_IM_FY17-19_20190215_v1_2_Zambia.txt")

TX_CURR19 <-FY19 %>%
  filter(indicator %in% c("TX_CURR", "TX_NEW") & standardizedDisaggregate=="Total Numerator") %>% 
  group_by(PSNUuid,PSNU,indicator) %>% 
  summarise_at(vars(FY2019Q1),funs(sum(.,na.rm=TRUE))) %>%
  spread(indicator, FY2019Q1) %>% 
  rename(TX_CURR_FY2019Q1=TX_CURR,
         TX_NEW_FY2019Q1=TX_NEW)


#merge
Final_0 <-merge(epi_clean,TX_15_16, by.x="psnuuid", by.y="PSNUuid", all=TRUE)
Final <-merge(Final_0,TX_CURR19, by.x="psnuuid", by.y="PSNUuid", all=TRUE) %>% 
  select(psnuuid,psnu,PSNU.x,fy16priority,TX_CURR_FY2015Q4,TX_NEW_FY2015Q4,TX_CURR_FY2016Q2,TX_NEW_FY2016Q2,PSNU.y,TX_CURR_FY2019Q1,TX_NEW_FY2019Q1,tx_curr_20,plhiv)


write_tsv(Final, "Zambia_TX_NEW and TX_CURR_FY15to19.txt")
