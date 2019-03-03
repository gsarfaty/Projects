library(readxl)
library(tidyverse)


#Read in PLHIV from DP
epi<-read_xlsx("COP19_Burundi_DataPack_Feb 27 Reviewed_nmbAJ.nmb.xlsx", sheet="Epi Cascade I", 
                 skip=4, col_names=TRUE)

epi <-epi %>% 
  separate(PSNU, c("psnu","psnuuid"), sep = "\\(") %>% 
  mutate(psnuuid = str_remove(psnuuid, "\\)")) %>% 
  select(psnu,psnuuid,Age,AgeCoarse,Sex,`PLHIV.NA.Age/Sex/HIVStatus.20T`)

#Read in TX from DP
TX<-read_xlsx("COP19_Burundi_DataPack_Feb 27 Reviewed_nmbAJ.nmb.xlsx", sheet="TX", 
               skip=4, col_names=TRUE)

TX<-TX %>% 
  separate(PSNU, c("psnu","psnuuid"), sep = "\\(") %>% 
  mutate(psnuuid = str_remove(psnuuid, "\\)")) %>% 
  select(psnu,psnuuid,Age,AgeCoarse,Sex,`TX_CURR.N.Age/Sex/HIVStatus.20T`,TX_CURR_SUBNAT.targeted,TX_CURR_SUBNAT.N.coverageTarget)

#join data from epi & TX tabs
df_long <-full_join(epi,TX)


#Sex/Age Coarse by PSNU Long
GIS <- df_long %>% 
  group_by(psnu,psnuuid,AgeCoarse,Sex,add=TRUE) %>% 
  summarize_at(vars(`PLHIV.NA.Age/Sex/HIVStatus.20T`:TX_CURR_SUBNAT.targeted),funs(sum(.,na.rm=TRUE))) %>% 
  rename(plhiv=`PLHIV.NA.Age/Sex/HIVStatus.20T`,
         tx_curr_20t=`TX_CURR.N.Age/Sex/HIVStatus.20T`,
         tx_curr_targeted=TX_CURR_SUBNAT.targeted)

#GIS_Peds
GIS_Peds <-GIS %>% 
  group_by(psnu,psnuuid,AgeCoarse,add=TRUE) %>% 
  filter(AgeCoarse=="<15") %>% 
  summarise_at(vars(plhiv:tx_curr_targeted),funs(sum(.,na.rm=TRUE))) %>% 
  mutate(tx_coverage=tx_curr_targeted/plhiv)


#GIS_Adult_Male
GIS_Over15_Male<-GIS %>% 
  group_by(psnu,psnuuid,AgeCoarse,Sex,add=TRUE) %>% 
  filter(AgeCoarse=="15+" & Sex=="Male") %>% 
  summarise_at(vars(plhiv:tx_curr_targeted),funs(sum(.,na.rm=TRUE))) %>% 
  mutate(tx_coverage=tx_curr_targeted/plhiv)


#GIS_GIS_Adult_Female
GIS_Over15_Female<-GIS %>% 
  group_by(psnu,psnuuid,AgeCoarse,Sex,add=TRUE) %>% 
  filter(AgeCoarse=="15+" & Sex=="Female") %>% 
  summarise_at(vars(plhiv:tx_curr_targeted),funs(sum(.,na.rm=TRUE))) %>% 
  mutate(tx_coverage=tx_curr_targeted/plhiv)


  
write_tsv(GIS_Peds, "Burudni_plhiv_ArtCov_Peds_DP_20180227.txt")
write_tsv(GIS_Over15_Male, "Burudni_plhiv_ArtCov_AdultMale_DP_20180227.txt")
write_tsv(GIS_Over15_Female, "Burudni_plhiv_ArtCov_AdultFemale_DP_20180227.txt")
