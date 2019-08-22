library(tidyverse)
library(readxl)


site1<-read_tsv("fy19q3\\SA_sitexim_20190821_pt1.txt")
site2<-read_tsv("fy19q3\\SA_sitexim_20190821_pt2.txt")
sitexim<-bind_rows(site1,site2)
rm(site1,site2)



####for now, keep to cascade total n/totla d [tst,pos,new,curr,pvls]
sitexim_sub<-sitexim %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS","TX_NEW","TX_CURR","TX_PVLS"),
         standardizedDisaggregate %in% c("Total Numerator", "Total Denominator"))


#Chatsworth facility (in eThekwini), move all targets to CDC
chatsworthtargets<-sitexim_sub %>% 
  filter(Fiscal_Year=="2019",
         FacilityUID=="HD8l3wLgl0l",
         !TARGETS=="na") %>% 
  mutate(FundingAgency= "HHS/CDC",
         PrimePartner = "Health Systems Trust",
         mech_code = as.numeric(18481),
         mech_name="Health Systems Trust Comprehensive (CDC GH001980)")

##Tshwane transition - HTS, TX_NEW = USAID: 25% (For Q1 contribution) and CDC: 75% (for Q2-4 contribution)
tshwanetargets_USAID<-sitexim_sub %>% 
  filter(Fiscal_Year=="2019",
         PSNUuid=="jP6YHtU87iQ",
         !TARGETS=="na",
         indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW"),
         FundingAgency=="USAID") %>% 
  mutate(TARGET_NEW=case_when(
    FundingAgency=="USAID" ~ as.numeric(TARGETS*0.25),
    TRUE ~ as.numeric(TARGETS)
     )) %>% 
  select(-TARGETS) %>% 
  rename(TARGETS=TARGET_NEW)
  
  
tshwanetargets_CDCadditions<-sitexim_sub %>% 
    filter(Fiscal_Year=="2019",
           PSNUuid=="jP6YHtU87iQ",
           !TARGETS=="na",
           indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW"),
           FundingAgency=="USAID") %>% 
  mutate(TARGET_NEW=case_when(
    FundingAgency=="USAID" ~ as.numeric(TARGETS*0.75),
    TRUE ~ as.numeric(TARGETS)
  )) %>% 
    select(-TARGETS) %>% 
    rename(TARGETS=TARGET_NEW) %>% 
  mutate(FundingAgency= "HHS/CDC",
         PrimePartner = "Aurum Health Research",
         mech_code = as.numeric(18484),
         mech_name="Aurum Comprehensive (CDC GH001981)")

#Tshwane transition for TX_CURR: Cumulative results moved to CDC, therefore, cumulative targets have been moved to CDC
tshwanetargets_snapshot<-sitexim_sub %>% 
  filter(Fiscal_Year=="2019",
         PSNUuid=="jP6YHtU87iQ",
         !TARGETS=="na",
         indicator %in% c("TX_CURR","TX_PVLS"))%>% 
  mutate(FundingAgency= "HHS/CDC",
         PrimePartner = "Aurum Health Research",
         mech_code = as.numeric(18484),
         mech_name="Aurum Comprehensive (CDC GH001981)")


#City of Cape Town targets are under Kheth’Impilo and results are under ANOVA;
#Move 1/2 targets to ANOVA for HTS_TST, POS, NEW
CTtargets_1<-sitexim_sub %>% 
  filter(Fiscal_Year=="2019",
         PSNUuid=="ROKgB9sl4NS",
         mech_code=="70288",
         !TARGETS=="na",
         indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW"))%>% 
  mutate(TARGET_NEW=case_when(
    FundingAgency=="USAID" ~ as.numeric(TARGETS*0.50),
    TRUE ~ as.numeric(TARGETS)
  )) %>% 
  select(-TARGETS) %>% 
  rename(TARGETS=TARGET_NEW)
  
  
CT_Targets2<-sitexim_sub %>% 
  filter(Fiscal_Year=="2019",
           PSNUuid=="ROKgB9sl4NS",
           mech_code=="70288",
           !TARGETS=="na",
           indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW")) %>% 
  mutate(TARGET_NEW=case_when(
      FundingAgency=="USAID" ~ as.numeric(TARGETS*0.50),
      TRUE ~ as.numeric(TARGETS)
    )) %>% 
    select(-TARGETS) %>% 
    rename(TARGETS=TARGET_NEW) %>% 
    mutate(PrimePartner="Anova Health Institute",
           mech_code=as.numeric(70310),
           mech_name="Anova: Accelerating Program Achievement to Control the Epidemic - Guateng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)")

  
###Move all for CT targets for TX_CURR & PVLS from KI to ANOVA
  CT_Targets3<-sitexim_sub %>% 
    filter(Fiscal_Year=="2019",
           PSNUuid=="ROKgB9sl4NS",
           mech_code=="70288",
           !TARGETS=="na",
           indicator %in% c("TX_CURR","TX_PVLS")) %>% 
    mutate(PrimePartner="Anova Health Institute",
           mech_code=as.numeric(70310),
           mech_name="Anova: Accelerating Program Achievement to Control the Epidemic - Guateng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)")


###create site x im sub that subtracts the above, then binds together new pieces
sitexim_sub_foundation<-sitexim_sub %>% 
  filter(!Fiscal_Year=="2019" | !FacilityUID=="HD8l3wLgl0l" | is.na(TARGETS)) %>% 
  filter(!Fiscal_Year=="2019" | !PSNUuid=="jP6YHtU87iQ" | is.na(TARGETS) | !indicator %in% c("TX_CURR","TX_PVLS")) %>%
  filter(!Fiscal_Year=="2019" | !PSNUuid=="jP6YHtU87iQ" | is.na(TARGETS) | !indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW") | !FundingAgency=="USAID") %>% 
  filter(!Fiscal_Year=="2019" | !PSNUuid=="ROKgB9sl4NS" | is.na(TARGETS) | !indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW") | !mech_code=="70288") %>% 
  filter(!Fiscal_Year=="2019" | !PSNUuid=="ROKgB9sl4NS" | is.na(TARGETS) | !indicator %in% c("TX_CURR","TX_PVLS") | !mech_code %in% c("70288")) 
  


site_im_revised<-bind_rows(sitexim_sub_foundation,chatsworthtargets,CT_Targets2,CTtargets_1,CT_Targets3,tshwanetargets_USAID,tshwanetargets_CDCadditions,tshwanetargets_snapshot)


write_tsv(site_im_revised,"fy19q3\\SA_sitexim_fy19q3i_shifts_20190822.txt",na="")
