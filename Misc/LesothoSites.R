library(tidyverse)
library(gisr)
library(glamr)
library(glitr)
library(Wavelength)
library(sf)
library(here)


#globals & directories

cntry <- "Lesotho"
user <- "gsarfaty"
key <- mypwd(user)
dir_terr<-"../..//Documents/GIS/DATA/topo"
dir_geo<-"../..//Documents/GIS/DATA/Boundaries"
file_districts<-"LesothoDistrictLsib2016Dec.shp"


#get coords
df<-extract_locations("Lesotho", user, mypwd(user)) 

facility<- df %>% 
  filter(label=="facility") %>% 
  mutate(coordinates = str_replace_all(coordinates, "\\(", ""),	
         coordinates = str_replace_all(coordinates, "\\)", ""),
         coordinates = str_replace_all(coordinates, "c", ""),	) %>% 
  separate(coordinates, c("longitude", "latitude"), sep = ",") %>% 
  filter(!is.na(latitude)) %>% 
  mutate(longitude=as.numeric(longitude),
         latitude=as.numeric(latitude)) %>% 
  st_as_sf(crs=qgs84)

macha<-facility %>% 
  filter(name=="Machabeng Hospital")

#get psnu
ls_districts <- (list.files(path =dir_geo,
                             pattern = file_districts,
                             recursive = TRUE,
                             full.names = TRUE) %>%
                    map(read_sf))[[1]]



#mappy 
ggplot() +
 geom_sf(data = ls_districts, colour = "black", fill = "light gray", linetype = "dotted") +
  geom_point(data = macha, aes(longitude, latitude), shape = 21, size = 3, fill = "red", colour = "red")+
  geom_sf_text(data = ls_districts, aes(label = NAME), color = grey80k, size = 4)+
  labs(
    title = "Lesotho",
    subtitle = "Machabeng Hospital is shown in red",
    caption = paste0(
      "OHA/SIEI/SI, ", Sys.Date(),
      "\nNote: names and boundaries are not necessarily authoritative."
    )
  ) +
  si_style_map()


ggsave(here("Lesotho_MachabengHospital_v1.png"),
       plot = last_plot(), scale = 1.2,
       dpi = 310, width = 10, height = 7, units = "in")

gis_sites<-facility %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 84)


st_write(gis_sites,
         "LesothoSites.shp", driver = "ESRI Shapefile")
