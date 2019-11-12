library(tidyverse)
library(readxl)


#read in dsp district level shift file
dsp_shifts<-read_excel("ContextFiles\\FY18_19_DistrictShifts.xlsx",sheet="Agency")

#read in genie
genie<-read_tsv("SA_fy19q4inprocess_20191112_cascadeonly.txt",
                col_types = c(.default = "c")) %>%
  mutate_at(vars(TARGETS,Qtr1,Qtr2,Qtr3,Qtr4,Cumulative), as.double)


#merge
genie_dspshifts<-merge(genie,dsp_shifts,by.x="PSNU",by.y="PSNU",all.x = TRUE)


#read in ethekwini fy19q1 41 site shifts
ethk<-read_excel("ContextFiles\\eThekwiniSiteShifts.xlsx") %>% 
  filter(Transitionstat=="USAIDtoCDC") %>% 
  rename(fy19q1_sitetransition=Transitionstat)

#merge in etk attributes
genie_dspandsiteshifts<-merge(genie_dspshifts,ethk,by.x="orgunituid",by.y="Facilityuid",all.x=TRUE)

#make wide
wide<-genie_dspandsiteshifts %>% 
  select(-c("TARGETS")) %>% 
  filter(indicator %in% c("TX_CURR","TX_NEW"),
         Fiscal_Year %in% c("2018", "2019")) %>% 
  gather(period,value,Qtr1:Cumulative) %>% 
  filter(!value=="na") %>%
  unite(indicator_unite,Fiscal_Year,indicator,period,sep="_",remove=FALSE) %>%
  spread(indicator_unite,value)

            

#filter out TX_CURR FY18Q4 for districts & mechs with shifts
wide_sub<-wide %>%
  select(-c(`2018_TX_CURR_Qtr1`,`2018_TX_CURR_Qtr2`,`2018_TX_CURR_Qtr3`,`2018_TX_CURR_Qtr4`,
            `2018_TX_NEW_Qtr1`,`2018_TX_NEW_Qtr2`,`2018_TX_NEW_Qtr3`,`2018_TX_NEW_Qtr4`,`2018_TX_NEW_Cumulative`,
            prime_partner_duns,pre_rgnlztn_hq_mech_code,award_number)) %>%
  mutate(FundingAgency_modified=FundingAgency,
         mech_code_modified=mech_code,
         mech_name_modified=mech_name,
         PrimePartner_modified=PrimePartner)
  
#base we will bind back to
wide_sub_foundation<-wide_sub %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="gp City of Johannesburg Metropolitan Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="gp Sedibeng District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="lp Capricorn District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="mp Nkangala District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="wc City of Cape Town Metropolitan Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="fs Lejweleputswa District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="nw Dr Kenneth Kaunda District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="kz eThekwini Metropolitan Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="kz uMgungundlovu District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="gp City of Tshwane Metropolitan Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="ec Alfred Nzo District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="ec Buffalo City Metropolitan Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="ec Chris Hani District Municipality") %>% 
  filter(!Fiscal_Year=="2018" | !indicator=="TX_CURR" | !PSNU=="ec Oliver Tambo District Municipality") 
  


#COJ FY18Q4 CURR
coj<-wide_sub %>%
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="gp City of Johannesburg Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17020" ~ "70310",
    mech_code=="17037" ~ "70310",
    mech_code=="17021" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17020" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    mech_code=="17037" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    mech_code=="17021" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17020" ~ "ANOVA HEALTH INSTITUTE",
    mech_code=="17037" ~ "ANOVA HEALTH INSTITUTE",
    mech_code=="17021" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ PrimePartner
  ))


#sedibeng
sedibeng<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="gp Sedibeng District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17023" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17023" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17023" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ PrimePartner
  ))
  
#capricorn
capricorn<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="lp Capricorn District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17036" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ PrimePartner
  ))

#Nkangala
Nkangala<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="mp Nkangala District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "70287",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "BRHC: Accelerating Program Achievements to Control the epidemic- KZN: King Cetshwayo and Ugu Mpumalanga: Gert Sibande and Nkangala",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17036" ~ "Broadreach Healthcare, LLC",
    TRUE ~ PrimePartner
  ))

#CT
CT<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="wc City of Cape Town Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17046" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17046" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17046" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ PrimePartner
  ))

#Lej
Lej<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="fs Lejweleputswa District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="18481" ~ "70301",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="18481" ~ "WRHI: Accelerating Program Achievements to Control the epidemic- Gauteng (Tshwane) and Free State (Lejwekleputswa)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="18481" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ PrimePartner
  )) %>% 
  mutate(FundingAgency_modified=case_when(
    mech_code=="18481" ~ "USAID",
    TRUE ~ FundingAgency
  ))


#dkk
dkk<-wide_sub %>%
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="nw Dr Kenneth Kaunda District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17037" ~ "18484",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17037" ~ "Aurum Comprehensive (CDC GH001981)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17037" ~ "THE AURUM INSTITUTE",
    TRUE ~ PrimePartner
  )) %>% 
  mutate(FundingAgency_modified=case_when(
    mech_code=="17037" ~ "HHS/CDC",
    TRUE ~ FundingAgency
  ))

#ethekwini
ethekwini<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="kz eThekwini Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "18481",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "Health Systems Trust Comprehensive (CDC GH001980)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "TRUST FOR HEALTH SYSTEM PLANNING & DEVELOPMENT IT1098/92",
    TRUE ~ PrimePartner
  )) %>% 
  mutate(FundingAgency_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "HHS/CDC",
    TRUE ~ FundingAgency
  )) %>% 
  mutate(fy19q1_sitetransition=case_when(
    Fiscal_Year=="2018" & indicator=="TX_CURR" & SiteName=="kz Cato Manor CHC" & FundingAgency=="HHS/CDC" ~ "CDC Exited in fy19q1",
    TRUE ~ fy19q1_sitetransition
  ))
  
#umgung
umgung<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="kz uMgungundlovu District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17046" ~ "70289",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17046" ~ "MatCH: Accelerating Program Achievements to Control the epidemic Eastern Cape (Alfred Nzo), KZN (Harry Gwala, eThekwini, uMgungundlovu)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17046" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ PrimePartner
  ))

#tshwane
tshwane<-wide_sub %>%
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="gp City of Tshwane Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "18484",
    mech_code=="17021" ~ "18484",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "Aurum Comprehensive (CDC GH001981)",
    mech_code=="17021" ~ "Aurum Comprehensive (CDC GH001981)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17036" ~ "THE AURUM INSTITUTE",
    mech_code=="17021" ~ "THE AURUM INSTITUTE",
    TRUE ~ PrimePartner
  )) %>% 
  mutate(FundingAgency_modified=case_when(
    mech_code=="17036" ~ "HHS/CDC",
    mech_code=="17021" ~ "HHS/CDC",
    TRUE ~ FundingAgency
  ))

#alfred Nzo
alfrednzo<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="ec Alfred Nzo District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17023" ~ "70289",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17023" ~ "MatCH: Accelerating Program Achievements to Control the epidemic Eastern Cape (Alfred Nzo), KZN (Harry Gwala, eThekwini, uMgungundlovu)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17023" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ PrimePartner
  ))

#buffalo city
buffalo<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="ec Buffalo City Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "70288",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "KI: Accelerating Program Achievements to Control the epidemicEastern Cape: Buffalo City Western Cape: Cape Town",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="17036" ~ "KHETHIMPILO AIDS FREE LIVING",
    TRUE ~ PrimePartner
  ))

#oliver tambo
otambo<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="ec Oliver Tambo District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="18481" ~ "18482",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="18481" ~ "TB/HIV Care Comprehensive (CDC GH001933)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="18481" ~ "TB HIV CARE ASSOCIATION",
    TRUE ~ PrimePartner
  ))


#Chris Hani
chrishani<-wide_sub %>% 
  filter(Fiscal_Year=="2018" & indicator=="TX_CURR" & PSNU=="ec Chris Hani District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="18481" ~ "18482",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="18481" ~ "TB/HIV Care Comprehensive (CDC GH001933)",
    TRUE ~ mech_name
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    mech_code=="18481" ~ "TB HIV CARE ASSOCIATION",
    TRUE ~ PrimePartner
  ))
  
#re-bound
df<-bind_rows(wide_sub_foundation,coj,sedibeng,capricorn,Nkangala,CT,Lej,dkk,ethekwini,umgung,tshwane,alfrednzo,buffalo,otambo,chrishani)


df_final<-df %>% 
  mutate(mech_code_modified=case_when(
    Fiscal_Year=="2019" & indicator=="TX_NEW" & PSNU=="wc City of Cape Town Metropolitan Municipality" & mech_code=="70288" ~ "70310",
    TRUE ~ mech_code_modified
  )) %>% 
  mutate(mech_name_modified=case_when(
  Fiscal_Year=="2019" & indicator=="TX_NEW" & PSNU=="wc City of Cape Town Metropolitan Municipality" & mech_code=="70288" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
  TRUE ~ mech_name_modified
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    Fiscal_Year=="2019" & indicator=="TX_NEW" & PSNU=="wc City of Cape Town Metropolitan Municipality" & mech_code=="70288" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ PrimePartner_modified
  )) %>% 
  mutate(mech_code_modified=case_when(
    Fiscal_Year=="2019" & indicator=="TX_NEW" & PSNU=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "18484",
    TRUE ~ mech_code_modified
  )) %>% 
  mutate(mech_name_modified=case_when(
    Fiscal_Year=="2019" & indicator=="TX_NEW" & PSNU=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "Aurum Comprehensive (CDC GH001981)",
    TRUE ~ mech_name_modified
  )) %>% 
  mutate(PrimePartner_modified=case_when(
    Fiscal_Year=="2019" & indicator=="TX_NEW" & PSNU=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "THE AURUM INSTITUTE",
    TRUE ~ PrimePartner_modified
  )) %>% 
  mutate(FundingAgency_modified=case_when(
    Fiscal_Year=="2019" & indicator=="TX_NEW" & PSNU=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "HHS/CDC",
    TRUE ~ FundingAgency_modified
  ))


write_tsv(df_final,"SA_Retention_PartnerShiftAdjusted_20191112.txt",na="")
