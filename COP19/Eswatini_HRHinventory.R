library(readxl)
library(tidyverse)


#Read in HRH
HRH<-read_xlsx("Eswatini-Dashboard 2019 GKoE 12_02_19 .xlsx", sheet="Facility List", 
               skip=0, col_names=TRUE)

HRH_data<-read_xlsx("Eswatini-Dashboard 2019 GKoE 12_02_19 .xlsx", sheet="HRH Data", 
                    skip=0, col_names=TRUE)

HRH_community<-HRH_data %>% 
  filter(`SITE LEVEL` %in% c("Community"))


HRH_facility<-HRH_data %>%
  filter(`SITE LEVEL` %in% c("Facility") & `FUNDING ENTITY` %in% c("USAID","CDC")) %>% 
  select(`FUNDING ENTITY`:FTE)

# #Read in org unit list w/MOH ids
# OU_mohid <-read_csv("OU sites.csv")
# 
# #filter to level 6
# OU_mohid <- OU_mohid %>% 
#   filter(orgunit_level==6)

#HRH facility list w/moh id
HRH_list <-read.csv("HRH_PEPFAR_Merge_FacilityList.csv")

#merge USAID HRH facility data w/HRH facility list w/moh id
merged_hrhdata <-merge(HRH_facility,HRH_list,by.x="FACILITY",by.y="Facility",all.x=TRUE)

#GIS
GIS <-read_csv("organisationUnits_GPS.csv") %>% 
  mutate(coordinates = str_replace_all(coordinates, "\\[", ""),
         coordinates = str_replace_all(coordinates, "\\]", "")) %>% 
  separate(coordinates, c("longitude", "latitude"), sep = ",") 

#merge facilitygis (hrh+pepfar) w/facility hrh data
facilityfinal<-merge(merged_hrhdata,GIS,by.x="orgunit_internal_id",by.y="id", all.x=TRUE)


write_tsv(facilityfinal, "HRH_facilitydata_MergeGIS_20190327.txt")

write_tsv(merged_hrhdata, "HRH_facilitydata_Merge_20190327.txt")
