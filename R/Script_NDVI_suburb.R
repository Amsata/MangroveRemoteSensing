memory.limit()
memory.limit(9999999999)

#### Import packages
library(rgdal)
library(raster)
library(dplyr)
library(RStoolbox)
library(plyr)
#library(keras)
#library(tfruns)
#library(tfestimators)
library(rgdal)
library(sf)
library(sp)
library(dplyr)
library(RStoolbox)
library(ggplot2)
library(tmap)
library(rasterVis)
library(data.table)
library(ggpubr)
library(randomForest)

rasterOptions(tmpdir='Temp')

source("R/fonctions.R")
shp<-readRDS('data/gadm36_SEN_4_sf.rds')
senegal<- readOGR('data/SEN_adm_shp/SEN_adm4.shp')
polyg <- senegal[senegal$NAME_2%in% c("Pikine"),]
landuse<-readOGR("data/occ_sol/Occ_sol.shp")
# limite<-readOGR("data/occ_sol/limite.shp")
# polyg<- readOGR('data/studied_area/studied_area.shp')
# polyg<-limite

fichier<-list.files(file.path("data","data_final"))
sous_fichier<-gsub(".tar","",fichier)
chemin<-file.path("data","data_final",fichier,sous_fichier)
metadonne<-paste0(chemin,"/",sous_fichier,"_MTL.txt")

metaD<-lapply(metadonne, readTML)

SPACECRAFT_ID<-unlist(lapply(metaD, function(x){tt(x,"SPACECRAFT_ID")}))

DATE_ACQUIRED<-unlist(lapply(metaD, function(x){tt(x,"DATE_ACQUIRED")}))
SENSOR_ID<-unlist(lapply(metaD, function(x){tt(x,"SENSOR_ID")}))
WRS_PATH<-unlist(lapply(metaD, function(x){tt(x,"WRS_PATH")}))
WRS_ROW<-unlist(lapply(metaD, function(x){tt(x,"WRS_ROW")}))
CLOUD_COVER<-unlist(lapply(metaD, function(x){tt(x,"CLOUD_COVER")}))

medaDf<-data.frame(cbind(
  FICHIER=as.character(chemin),
  SPACECRAFT_ID=SPACECRAFT_ID,
  DATE_ACQUIRED=DATE_ACQUIRED,
  SENSOR_ID=SENSOR_ID,
  WRS_PATH=WRS_PATH,
  WRS_ROW=WRS_ROW,
  CLOUD_COVER=CLOUD_COVER
),stringsAsFactors = FALSE)

medaDf<-medaDf %>% mutate(YEAR=substr(DATE_ACQUIRED,1,5),
                          MONTH=substr(DATE_ACQUIRED,7,8),
                          CLOUD_COVER=as.numeric(CLOUD_COVER),
                          WRS_ROW=as.numeric(WRS_ROW)
)

touse<-medaDf %>% group_by(SPACECRAFT_ID,YEAR,MONTH) %>%
  # filter(CLOUD_COVER==min(CLOUD_COVER)) %>% 
  # ungroup() %>% 
  #filter(DATE_ACQUIRED %in% c("2018-12-18")) %>% 
  filter(SPACECRAFT_ID %in% c(" LANDSAT_8")) %>% 
  filter(CLOUD_COVER<=2)

touse$SPACECRAFT_ID<-gsub(" ","",touse$SPACECRAFT_ID)
touse$YEAR<-as.numeric(gsub(" ","",touse$YEAR))

saloum<-touse %>% filter(WRS_ROW==50 & WRS_PATH==" 205" & YEAR>2013)

data_ldst_8<-lapply(saloum$FICHIER,readLandsat2)

data<-stack(data_ldst_8)

NDVI<-lapply(data_ldst_8, function(x){NDVI_func(x,"LANDSAT_8")})

data_ndvi<-stack(NDVI)
