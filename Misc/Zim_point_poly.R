# Project: Zim OVC DB
# Author: G. Sarfaty

library(sp)
library(sf)
library(rgdal)
library(maps)
library(tidyverse)
library(here)



#load data -----------------------------------------------------------------------------
sites<-here("Data", "org_coord.csv")
shp<-here("GIS","ZimWards2002.shp")
d_shp<-here("GIS","Zimbabwe_PROD_5_District_DistrictLsib_2020_May.shp")
org<-here("Data","Data Exchange Organisation Units.csv")

facility<-read_csv(sites,na="") 
wards<-readOGR(dsn=shp)
dist<-readOGR(dsn=d_shp)
org<-read_csv(org)


#clean lat/long file---------------------------------------------------------------------
facility<- facility%>% 
  mutate(geometry = str_replace_all(geometry, "\\(", ""),
         geometry= str_replace_all(geometry, "\\)", ""),
         geometry= str_replace_all(geometry, "POINT ", "")) %>% 
  separate(geometry, c("longitude", "latitude"), sep = " ") %>% 

#Separate facilities w no lat/long
facility_nocoord<-facility %>% 
  filter(is.na(longitude))

facility_coord<-facility %>% 
  filter(!is.na(longitude)) %>% 
  mutate(longitude=as.numeric(longitude),
         latitude=as.numeric(latitude))


#make lat/long spatial objc
coordinates(facility_coord) <- c("longitude", "latitude")
as(facility_coord,"SpatialPoints")



#check proj & set proj where needed -------------------------------------------------------
proj4string(wards)<-CRS("+proj=longlat +datum=WGS84")
proj4string(facility_coord)<-CRS("+proj=longlat +datum=WGS84")
wards@proj4string
facility_coord@proj4string
dist@proj4string


#run over points in wards -----------------------------------------------------------------
pointsinpoly<-over(facility_coord,wards)


#append findings to faciliy df ------------------------------------------------------------
output<-cbind(facility_coord@data,facility_coord@coords,pointsinpoly$DISTRICT,
              pointsinpoly$WARDCODE,pointsinpoly$WARDNUMBER,pointsinpoly$WARDKEY,
              pointsinpoly$WARDNAME1)

#clean up and append sites w no coords
output<-output %>% 
  rename(DISTRICT=`pointsinpoly$DISTRICT`,
         WARDCODE=`pointsinpoly$WARDCODE`,
         WARDNUMBER=`pointsinpoly$WARDNUMBER`,
         WARDKEY=`pointsinpoly$WARDKEY`,
         WARDNAME1=`pointsinpoly$WARDNAME1`) %>% 
  bind_rows(facility_nocoord)
  


#filter org units to level 5 & join to output --------------------------------------------
org6<-org %>% 
  filter(orgunit_level=="6") %>% 
  select(orgunit_parent,orgunit_parent_internal_id,orgunit_internal_id)

final<-output %>% 
  left_join(org6,by=c("id"="orgunit_internal_id")) %>% 
  select(id,name,orgunit_parent_internal_id,orgunit_parent,
         DISTRICT,WARDCODE,WARDNUMBER,WARDKEY,WARDNAME1)



filename<-paste("Zim_FacilidtytoWard", Sys.Date(), ".csv",sep="_")
write_csv(final,file.path("Dataout",filename,na=""),na="")


##check urban rural names
wards_urban_rural<-wards_dist %>% 
  filter(str_detect(DISTRICT,"urban") | str_detect(DISTRICT,"Urban")| str_detect(DISTRICT,"Rural"))

