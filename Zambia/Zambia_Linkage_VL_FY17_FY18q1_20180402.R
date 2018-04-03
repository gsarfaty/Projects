#load packages
pacman::p_load(tidyverse, stringr, lubridate, tidyr, dplyr, readr)


#set WD
setwd("C:/Users/gsarfaty/Documents/Zambia")


#import data & read in values as double
linkage_data <- read_tsv("ICPI_FactView_Site_IM_Zambia_20180215_v1_3.txt", 
                         col_types = cols(FY2017Q1 = "d",
                                          FY2017Q2 = "d",
                                          FY20173 = "d",
                                          FY2017Q4 = "d",
                                          FY2017APR = "d",
                                          FY2018Q1 ="d"))



# change all header names to lower case to make it easier to use
names(linkage_data) <- tolower(names(linkage_data))


#filter and subset for key variables and disaggs
linkage_data2 <-linkage_data %>%
  filter(indicator %in% c("HTS_TST_POS", "HTS_TST_NEG", "TX_NEW", "TX_PVLS"),
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")) %>%
  select(snu1,snu1uid,psnu,psnuuid,facilityuid,facility,fundingagency,implementingmechanismname,indicator,standardizeddisaggregate,numeratordenom,age,sex,fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017apr,fy2018q1) %>% 
  group_by(snu1,snu1uid,psnu,psnuuid,facilityuid,facility,fundingagency,implementingmechanismname,indicator,numeratordenom,age,sex,add=TRUE) %>% 
  summarize_at(vars(starts_with("fy2")),funs(sum(.,na.rm=TRUE))) %>% 
  ungroup() %>% 
  unite(indicator_unite,indicator,numeratordenom,sep="_") %>% 
  gather(qtr, value, starts_with("fy2")) %>% 
  spread(indicator_unite,value) 


#create HTS_TST as sum of post and neg disaggs
linkage_data2$HTS_TST_N <-rowSums(linkage_data2[,12:13], na.rm=TRUE)

#reorder columns
linkage_data2<-linkage_data2[c(1,2,3,4,5,6,7,8,9,10,11,17,12,13,14,16,15)]

#get total numerator for our variables of interest & read in values as double
totalN <- read_tsv("ICPI_FactView_Site_IM_Zambia_20180215_v1_3.txt", 
                   col_types = cols(FY2017Q1 = "d",
                                    FY2017Q2 = "d",
                                    FY2017Q3 = "d",
                                    FY2017Q4 = "d",
                                    FY2017APR = "d",
                                    FY2018Q1 = "d"))

names(totalN) <-tolower(names(totalN))

totalN2 <- totalN %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "TX_NEW", "TX_PVLS"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  select(snu1,snu1uid,psnu,psnuuid,facilityuid,facility,fundingagency,implementingmechanismname,indicator,standardizeddisaggregate,numeratordenom,fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017apr,fy2018q1) %>% 
  group_by(snu1,snu1uid,psnu,psnuuid,facilityuid,facility,fundingagency,implementingmechanismname,indicator,standardizeddisaggregate,numeratordenom,add=TRUE) %>% 
  summarize_at(vars(starts_with("fy2")),funs(sum(.,na.rm=TRUE))) %>% 
  ungroup() %>% 
  unite(indicator_unite,indicator,numeratordenom,sep="_") %>% 
  gather(qtr, value, starts_with("fy2")) %>% 
  spread(indicator_unite,value) 
 

#create age and sex columns and reorder to align with age/sex df
totalN2$age <-totalN2$standardizeddisaggregate
totalN2$sex <-totalN2$standardizeddisaggregate

totalN2 <- totalN2 %>% 
  select(-standardizeddisaggregate) %>% 
  select(snu1,snu1uid,psnu,psnuuid,facilityuid,facility,fundingagency,implementingmechanismname,age,sex,qtr,HTS_TST_N,HTS_TST_NEG_N,HTS_TST_POS_N,TX_NEW_N,TX_PVLS_N,TX_PVLS_D)

  
#combine disagg df with total numerator
Data_Final <-rbind.data.frame(linkage_data2,totalN2)

#export
write_tsv(Data_Final, "Zambia_Linkage_VL_20180402.txt", na="")
