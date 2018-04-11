## Burundi SDS revision 
## prepping estimates prepared by N. Bartlett using DHS data
## Gina Sarfaty
## April 11, 2018


#load packages
pacman::p_load(plyr, tidyr, dplyr, readr, tidyverse)


#set WD
setwd("C:/Users/gsarfaty/Documents/GIS/COP18/Burundi")


#import data 
sds_data <- read_excel("Burundi_SDS_Revised_DHSnumbers.xlsx")

#change variables to lower case
names(sds_data) <- tolower(names(sds_data))


#create column to cluster Bujumbura & group Rumonge with Bururi
sds_data$psnu_align <-sds_data$psnu

sds_data <-sds_data %>% 
  mutate(psnu_align = str_replace(psnu_align, "Bujumbura Rural", "Bujumbura")) %>%
  mutate(psnu_align = str_replace(psnu_align, "Bujumbura Mairie", "Bujumbura")) %>% 
  mutate(psnu_align = str_replace(psnu_align, "Rumonge", "Bururi")) %>% 
  group_by(psnu_align) %>% 
  summarize_at(vars(plhiv:coverage),funs(sum(.,na.rm=TRUE)))

#calculate coverage based on new psnu groupings
sds_data$coverage <-round((sds_data$`tx_curr (moh)`/sds_data$plhiv)*100,1)

write.csv(sds_data, file="Burundi_SDS_DHSrevision.txt", na="")


