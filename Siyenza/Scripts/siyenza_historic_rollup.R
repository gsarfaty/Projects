#load packages
library(tidyverse)
library(here)
library(readxl)
library(here)
library(glamr)



#GLOBALS -----------------------------------------------------------------------------
raw<-here("Data")


# RAW DATA IN ------------------------------------------------------------------------

raw_files<-list.files(raw,pattern="Siyenza")

raw_df<-here("Data",raw_files) %>% 
  map(~ read_excel(.x, sheet = which(str_detect(excel_sheets(.x), 'RAW')))) %>%
  reduce(rbind)

# ROLL UP ----------------------------------------------------------------------------

df_base<-raw_df %>% 
  mutate(mon_yr= format(Week_End, "%Y-%m"),
         MechanismID=as.character(MechanismID)) %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric))))
                                

df_snapshot<-df_base%>%
  filter(indicator=="TX_CURR_28", !is.na(val)) %>% 
  group_by(mon_yr,Facility,MechanismID) %>% 
  filter(Week_End==max(Week_End)) %>% 
  ungroup() %>% 
  select(-c(Week_Start,Week_End))


df_cumulative<-df_base %>% 
  filter(!is.na(val),
         !indicator=="TX_CURR_28") %>% 
  select(-c(Week_Start,Week_End)) %>% 
  group_by(FundingAgency,PrimePartner,MechanismID,SNU1,PSNU,Community,Facility,
           Siyenza_StartDate,Siyenza_EndDate,mon_yr,indicator) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE)
  
final<-bind_rows(df_snapshot,df_cumulative) %>% 
  spread(indicator,val)
