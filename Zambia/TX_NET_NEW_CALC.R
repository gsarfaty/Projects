######################Caclulated NN for CRS shifts #######
df<-read_tsv("FY20\\Q1\\zam_msd_genie_fy17to20_20200217_att.txt",
             col_types = c(.default = "c"))

NN_calc<- df %>% 
  filter(indicator =="TX_NET_NEW",
         standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% c("2019","2020")) %>% 
  mutate(indicator="TX_NET_NEW_CALC") %>% 
  gather(period,value,targets:cumulative) %>% 
  mutate(mech_code=case_when(
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative") & mech_code=="18327" &
      IM_TransferToFY19=="SAFE" ~ "17413",
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative") & mech_code=="18327" &
      IM_TransferToFY19=="DISCOVER" ~ "17399",
    TRUE ~ mech_code
  )) %>%
  mutate(fundingagency=case_when(
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative")  & mech_code=="17413" ~ "USAID",
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative")  & mech_code=="17399" ~ "USAID",
    TRUE ~ fundingagency
  )) %>%
  mutate(primepartner=case_when(
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative") & mech_code=="17413" &
      IM_TransferToFY19=="SAFE" ~ "John Snow, Incorporated",
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative") & mech_code=="17399" &
      IM_TransferToFY19=="DISCOVER" ~ "JSI Research and Training Institute, INC.",
    TRUE ~ primepartner
  )) %>%
  mutate(mech_name=case_when(
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative") & mech_code=="17413" &
      IM_TransferToFY19=="SAFE" ~ "SAFE",
    Transfer_FY19Q1=="CDC to USAID" & fiscal_year=="2019" & period %in% c("qtr1","cumulative") & mech_code=="17399" &
      IM_TransferToFY19=="DISCOVER" ~ "USAID/District Coverage of Health Services (DISCOVER-H)",
    TRUE ~ mech_name
  )) %>% 
  spread(period,value)

final<-df %>% 
  bind_rows(NN_calc)

write_tsv(final,"Zam_fy17tofy20_att_NNcalc.txt",na="")
