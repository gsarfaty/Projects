df<-bind_rows(wide_sub_foundation,coj,sedibeng,capricorn,Nkangala,CT,Lej,dkk,ethekwini,umgung,tshwane)


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

write_tsv(df_final,"SA_Retention_PartnerShiftAdjusted_20191106.txt",na="")
