## Zambia Partner/Site data collection
## Gina Sarfaty
## March 26, 2018


#load packages
library(dbplyr)


#set WD
setwd("C:/Users/gsarfaty/Documents/Zambia")


#import data 
zambia_sites <- read_tsv("ICPI_FactView_Site_IM_Zambia_20180215_v1_3.txt")

#change variables to lower case
names(zambia_sites) <- tolower(names(zambia_sites))

#filter and subset for facility attributes; only those reporting in apr17 or fy18 t/r
zambia_site_template <-zambia_sites %>%
  filter(fundingagency %in% c("USAID"),
         typefacility %in% c("Y"), 
         !is.na(fy2017apr) | !is.na(fy2018_targets) | !is.na(fy2018q1)) %>%
  select(operatingunit,snu1,snu1uid,psnuuid,psnu,facilityuid,facility,currentsnuprioritization,mechanismuid,primepartner,fundingagency,mechanismid,implementingmechanismname) %>% 
  distinct(facilityuid,mechanismuid,primepartner,implementingmechanismname, .keep_all=TRUE)

#add column for date/qtr
zambia_site_template$qtr <-"FY2018Q2"

#write file
write_tsv(zambia_site_template, "Zambia_Site_Template_FY2018Q2.tsv")

