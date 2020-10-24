library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(RStoolbox)

shp<-readRDS('data/gadm36_SEN_4_sf.rds')
sedhiou<- readOGR('data/SEN_adm_shp/SEN_adm4.shp')
# bignona <- sedhiou[sedhiou$NAME_4%in% c("Kafountine"),]
bignona <- sedhiou[sedhiou$NAME_4%in% c("Yeumbeul Nord"),]

modis<-raster("data/MOD_NDVI_M_2000-03.FLOAT.tiff")
cod<-raster("data/MOP_CO_M_2000-03.FLOAT.tiff")


dfn<-crop(stack(modis), sedhiou)
dfn<-mask(dfn,sedhiou)

dfo<-crop(stack(cod), sedhiou)
dfo<-mask(dfo,sedhiou)
