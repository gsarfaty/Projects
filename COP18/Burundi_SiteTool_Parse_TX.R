#library
pacman::p_load(tidyverse, readxl)

#set WD
setwd("C:/Users/gsarfaty/Documents/GIS/Burundi/Pivot")

#########################import TXCURR as df################################################
TX_CURR <- read_excel("BurundiCOP18SiteTool_TX_CURR.xlsx")

#add an NA where facility/community is for not yet distributed rows
TX_CURR <- TX_CURR %>% 
  mutate(Site = str_replace(Site, "NOT YET DISTRIBUTED", "NOT YET DISTRIBUTED {NA}"))

#replace end brackets with blanks and then replace opening brackets with common delimitor ">"
TX_CURR <- TX_CURR %>% 
  mutate(Site = str_replace_all(Site, "[})]", ""),
         Site = str_replace_all(Site, "[{())]", "> ")) %>%
  #parse into distinct columns
  separate(Site, c("psnu", "site_name", "site_type", "site_uid"), sep = ">") %>% 
  filter(!is.na(site_uid)) %>% 
  #remove leading/trailing spaces
  mutate_all(~ trimws(.))

#####################import site list as DF################################################
sitelist <- read_excel("BurundiCOP18SiteTool_sitelist.xlsx")

#add an NA where facility/community is for not yet distributed rows
sitelist <- sitelist %>% 
  mutate(siteID = str_replace(siteID, "NOT YET DISTRIBUTED", "NOT YET DISTRIBUTED {NA}"))

#replace end brackets with blanks and then replace opening brackets with common delimitor ">"
sitelist <- sitelist %>% 
  mutate(siteID = str_replace_all(siteID, "[})]", ""),
         siteID = str_replace_all(siteID, "[{())]", "> ")) %>%
  #parse into distinct columns
  separate(siteID, c("psnu", "site_name", "site_type", "site_uid"), sep = ">") %>% 
  filter(!is.na(site_uid)) %>% 
  #remove leading/trailing spaces
  mutate_all(~ trimws(.))


#########################import coord as DF################################################
gps <- read_tsv("Burundi_OrgUnits_20180412.csv")

gps <- gps %>% 
  mutate(coordinates = str_replace_all(coordinates, "\\[", ""),
         coordinates = str_replace_all(coordinates, "\\]", "")) %>% 
  #parse into distinct columns
  separate(coordinates, c("longitude", "latitude"), sep = ",") %>% 
  select(id,longitude,latitude)

COP18 <-merge(TX_CURR,sitelist,by.x="site_uid",by.y="site_uid",all=TRUE)

COP18_GIS <-merge(COP18,gps,by.x="site_uid",by.y="id",all=TRUE)

COP18_GIS <- COP18_GIS %>% 
  filter(site_type.y %in% c("Facility")) %>% 
  select(site_uid,psnu.y,site_name.y,Inactive,tx_curr_fy19,longitude,latitude)

write_csv(COP18_GIS, "COP18_TX_CURR_Sites.csv", na="")


#########################fy17 txcurr#######################################################
fy17tx_curr <-read_tsv("ICPI_MER_Structured_Dataset_Site_IM_Burundi_20180323_v2_1.txt")

# change all header names to lower case to make it easier to use
names(fy17tx_curr) <- tolower(names(fy17tx_curr))


#filter and subset for key variables and disaggs
fy17tx_curr2 <-fy17tx_curr %>%
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator")) %>%
  select(facilityuid,facility,indicator,standardizeddisaggregate,fy2017apr) %>% 
  group_by(facilityuid,facility,add=TRUE) %>% 
  summarize_at(vars(starts_with("fy2017")),funs(sum(.,na.rm=TRUE))) %>% 
  ungroup()

###################join##################################################################

COP18_GIS_final <-merge(COP18_GIS,fy17tx_curr2,by.x = "site_uid", by.y="facilityuid",all=TRUE)

write.csv(COP18_GIS_final, "COP18_TX_Sites_final.csv", na="")