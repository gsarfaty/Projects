## Burundi SDS
## Gina Sarfaty
## March 19, 2018


#load packages
pacman::p_load(plyr, tidyr, dplyr, readr, tidyverse)


#set WD
setwd("C:/Users/gsarfaty/Documents/GIS/COP18/Burundi")


#import data 
sds_data <- read_tsv("ICPI_FactView_PSNU_IM_20180215_v1_3_Burundi.txt")

#change variables to lower case
names(sds_data) <- tolower(names(sds_data))

#filter and subset for key variables and disaggs
sds_data <-sds_data %>%
  filter(indicator %in% c("TX_CURR", "TX_PVLS"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  select(psnuuid,psnu,indicator,standardizeddisaggregate,fy2017apr) %>% 
  group_by(psnu,psnuuid,indicator,standardizeddisaggregate,add=TRUE) %>% 
  summarize_at(vars(starts_with("fy2017")),funs(sum(.,na.rm=TRUE))) %>%
  ungroup() %>% 
  unite(indicator_unite_,indicator,standardizeddisaggregate, sep="_") %>% 
  spread(indicator_unite_,fy2017apr)  
  
#rename columns to be appropriate for use in ArcGIS
names(sds_data)[3] <-"TX_CURR_N"
names(sds_data)[4] <-"TX_PVLS_D"
names(sds_data)[5] <-"TX_PVLS_N"


#create cluster column & sum values by clusters
sds_data$cluster <-sds_data$psnu

sds_data <-sds_data %>% 
  mutate(cluster = str_replace(cluster, "Bujumbura Rural", "Bujumbura")) %>%
  mutate(cluster = str_replace(cluster, "Bujumbura Mairie", "Bujumbura")) %>% 
  group_by(cluster) %>% 
  summarize_at(vars(starts_with("TX")),funs(sum(.,na.rm=TRUE)))

  

#merge MER with PLHIV figures from country team
plhiv <-read_tsv("Burundi_PLHIV_Clustered.txt")
SDS_GIS<-merge(sds_data,plhiv,by.x="cluster",by.y="Province")

#calculate ART Coverage and VL coverage
SDS_GIS$art_cov <- round((SDS_GIS$TX_CURR_N/SDS_GIS$PLHIV_17)*100,1)
SDS_GIS$vl_cov <-round((SDS_GIS$TX_PVLS_D/SDS_GIS$TX_CURR_N)*100,1)

#export
write_tsv(SDS_GIS, "Burundi_SDS_GIS_format.txt", na="")


