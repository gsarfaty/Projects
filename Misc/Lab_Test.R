##  PROJECT: Geospatal Analytics
##  AUTHOR:  G.Sarfaty
##  PURPOSE: lab testing volume
##  LICENCE: MIT
##  DATE:    2020-10-02



# Libraries
library(tidyverse)
library(sf)
library(glamr) #if needed, need to install devtools then use devtools::install_github("USAID-OHA-SI/glamr")
library(gisr) #if needed, need to install devtools then use devtools::install_github("USAID-OHA-SI/gisr")
library(here)
library(ICPIutilities) #if needed, need to install devtools then use devtools::install_github("ICPI/ICPIutilities")


# Globals
dir_data <- here("Data")
dir_dataout <- here("Dataout")

dir_geo <- "../../Boundaries"
dir_terr <- "../../Topography"
dir_merdata <- "../../DATIM"

dir_img <- here("Images")
dir_graphs <- here("Graphics")

cntry <- "Mozambique" #country of interest
user <- "yourusername" #your username entered in keyringr
key <- mypwd(user)


# MER Data
file_site_im <- (list.files(path =dir_merdata,
                            pattern = "Moz",
                            recursive = TRUE,
                            full.names = TRUE))

df<-read_msd(file_site_im)


# MER MUNGE - filter to testing volume for Viral Load
VL_Labs<-df %>%
    filter(indicator =="LAB_PTCQI",
           standardizeddisaggregate %in% c("POCT/TestVolume", "Lab/TestVolume"),
           fiscal_year=="2019") %>%
    group_by(orgunituid,facilityprioritization,indicator,standardizeddisaggregate,otherdisaggregate_sub) %>%
    summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
    ungroup() %>%
    select(-c(targets:qtr4)) %>%
    mutate(standardizeddisaggregate=str_remove_all(standardizeddisaggregate,"\\/"),
           standardizeddisaggregate=str_remove_all(standardizeddisaggregate,"TestVolume")) %>%
    filter(otherdisaggregate_sub=="HIV Viral Load")

prinf(VL_Labs)


# GEO
facility<-extract_locations(cntry, user, mypwd(user)) %>%
    filter(label=="facility",
           !coordinates=="NULL") %>%
    mutate(coordinates = str_replace_all(coordinates, "c", ""),
           coordinates = str_replace_all(coordinates, "\\(", ""),
           coordinates = str_replace_all(coordinates, "\\)", "")) %>%
    separate(coordinates, c("longitude", "latitude"), sep = ",") %>%
    select(operatingunit,name,id,longitude,latitude)

# EXPORT
filename<-paste(Sys.Date(),cntry,"coords_SBU.csv",sep="_")

write.csv(facility, file.path(here("Dataout"),filename,na=""))

