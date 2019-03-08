library(readxl)
library(tidyverse)

df <-read_tsv("MER_Structured_Dataset_PSNU_IM_FY17-19_20190215_v1_1_South Sudan.txt")



#Female Testing
HTSFY18_Female <- df %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_NEG", "HTS_TST_POS") & standardizedDisaggregate=="Modality/Age/Sex/Result" & Sex=="Female") %>%
  group_by(PSNUuid,PSNU,indicator) %>% 
  summarise_at(vars(FY2018APR),funs(sum(.,na.rm=TRUE))) %>%
  unite(indicator_unite,indicator,sep="_") %>% 
  spread(indicator_unite, FY2018APR)
  

write_tsv(HTSFY18_Female, "SSudan_FemaleTesting_FY18.txt")



#Male Testing
HTSFY18_Male <- df %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_NEG", "HTS_TST_POS") & standardizedDisaggregate=="Modality/Age/Sex/Result" & Sex=="Male") %>%
  group_by(PSNUuid,PSNU,indicator) %>% 
  summarise_at(vars(FY2018APR),funs(sum(.,na.rm=TRUE))) %>%
  unite(indicator_unite,indicator,sep="_") %>% 
  spread(indicator_unite, FY2018APR)


write_tsv(HTSFY18_Male, "SSudan_MaleTesting_FY18.txt")


#total testing
HTSFY18 <- df %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_NEG", "HTS_TST_POS") & standardizedDisaggregate=="Total Numerator") %>%
  group_by(PSNUuid,PSNU,indicator) %>% 
  summarise_at(vars(FY2018APR),funs(sum(.,na.rm=TRUE))) %>%
  unite(indicator_unite,indicator,sep="_") %>% 
  spread(indicator_unite, FY2018APR)


write_tsv(HTSFY18, "SSudan_Total_Testing_FY18.txt")
