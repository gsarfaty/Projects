library(readxl)
library(tidyverse)


df <-read_tsv("MER_Structured_Dataset_Site_IM_FY17-19_20190215_v1_2_Eswatini.txt",
              col_types = cols(FY2019Q1 = "d"))



#FY19 VLS
VLS<- df %>% 
  filter(indicator %in% c("TX_PVLS") & standardizedDisaggregate %in% c("Total Numerator", "Total Denominator"))%>%
  group_by(CommunityUID,Community,indicator,standardizedDisaggregate) %>% 
  summarise_at(vars(FY2019Q1),funs(sum(.,na.rm=TRUE))) %>% 
  unite(ND,standardizedDisaggregate,sep="_") %>% 
  spread(ND,FY2019Q1) %>% 
  mutate(VLS=`Total Numerator`/`Total Denominator`)
  
  
write_tsv(VLS, "Eswatini_VLS_FY19Q1.txt")
  
