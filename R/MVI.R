
tensorflow::install_tensorflow()
memory.limit()
memory.limit(9999999999)
set.seed(120293)
library(rgdal)
library(raster)
library(dplyr)
library(RStoolbox)
library(plyr)
library(keras)
library(tfruns)
library(tfestimators)
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
polyg <- senegal[senegal$NAME_3%in% c("Niodior"),]
landuse<-readOGR("data/occ_sol/Occ_sol.shp")
limite<-readOGR("data/occ_sol/limite.shp")
polyg<- readOGR('data/studied_area/studied_area.shp')
polyg<-limite

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

saloum<-touse %>% filter(WRS_ROW==50 & WRS_PATH==" 205")
casa1<-touse %>% filter(WRS_ROW==51 & WRS_PATH==" 205")
casa2<-touse %>% filter(WRS_ROW==51 & WRS_PATH==" 204")



satellite<-c(saloum$SPACECRAFT_ID[20],
             casa1$SPACECRAFT_ID[10],
             casa2$SPACECRAFT_ID[1])
ldst_8<-c(saloum$FICHIER[20],
          casa1$FICHIER[10],
          casa2$FICHIER[1])

ldst_8_date<-c(saloum$DATE_ACQUIRED[20],
               casa1$DATE_ACQUIRED[10],
               casa2$DATE_ACQUIRED[1])

data_ldst_8<-lapply(ldst_8,readLandsat)
#data_ldst_8<-readLandsat(ldst_8[2])
#Mosaic-ing

data_mosaic<-vector(mode="list",length = 7)
pairs <- mapply(list, as.list(data_ldst_8[[1]]),
                as.list(data_ldst_8[[2]]),
                as.list(data_ldst_8[[3]]), SIMPLIFY=F)

mosaic<-lapply(pairs, function(lst){
  lst$fun <- mean
  do.call("mosaic",lst)}
)
data_ldst_8<-stack(mosaic)

for (i in 1:nlayers(data_ldst_8)) {
  
  filename<-paste0("data\\bands\\Band",i,".tif")
  
  writeRaster(data_ldst_8[[i]],filename =filename ,overwrite=TRUE)
  
  
}

# data_ldst_8<-list()
# 
# for (i in 1:8) {
#   
#   filename<-paste0("data\\bands\\Band",i,".tif")
#   
# data_ldst_8[[i]]<-raster(filename)
#   
# }

# path_bands<-"data/bands/"
# data_ldst_8<-stack(list.files(path_bands))

#Nrmalized DIfference Vegetation Index (NDVI)----
NDVI<-NDVI_func(data_ldst_8,"LANDSAT_8")
#Nrmalized Difference Water Index (NDWI)----
NDWI<-NDWI_func(data_ldst_8,"LANDSAT_8")
#Normalized Difference Drought Index
NDDI<-(NDVI-NDWI)/(NDVI+NDWI)
#Vegetation Condition Index
MRI<-NDVI-NDWI
writeRaster(NDVI,"indices/NDVI.tif",overwrite=TRUE)
writeRaster(NDWI,"indices/NDWI.tif",overwrite=TRUE)
# writeRaster(NDDI,"indices/NDDI.tif",overwrite=TRUE)
# writeRaster(VCI,"indices/VCI.tif",overwrite=TRUE)

NDVI<-normImage(NDVI)
NDWI<-normImage(NDWI)
MRI<-normImage(MRI)
# NDDI<-normImage(NDDI)
#-----

AMSA<-AMSA_func(data_ldst_8,"LANDSAT_8")
writeRaster(AMSA,"indices/AMSA.tif",overwrite=TRUE)
AMSA<-normImage(AMSA)

MVI<-MVI_func(data_ldst_8)
writeRaster(MVI,"indices/AMSA.tif",overwrite=TRUE)
MVI<-normImage(MVI)

#Bare Soil Index (BSI)----
# BSI<-BSI_func(data_ldst_8,"LANDSAT_8")
# writeRaster(BSI,"indices/BSI.tif",overwrite=TRUE)
# BSI<-normImage(BSI)
#-------

# #Advance Vagetation Index (AVI)-----
# AVI<-AVI_func(data_ldst_8)
# writeRaster(AVI,"indices/AVI.tif",overwrite=TRUE)
# AVI<-normImage(AVI)
#-----

#Soil Adjusted Total Vegetation Index (SATVI)------
SATVI<-SATVI_func(data_ldst_8)
writeRaster(SATVI,"indices/SATVI.tif",overwrite=TRUE)
SATTVI<-normImage(SATVI)


# #Modified Soil Adjusted Vegetation index (MSAVI)---------
# MSAVI<-MSAVI_func(data_ldst_8,"LANDSAT_8")
# writeRaster(MSAVI,"indices/MSAVI.tif",overwrite=TRUE)
# MSAVI<-normImage(MSAVI)
#-----------------------

# #Soil Adjusted Vegetation Index (SAVI)------------
# SAVI<-SAVI_func(data_ldst_8,"LANDSAT_8")
# writeRaster(SAVI,"indices/SAVI.tif",overwrite=TRUE)
# DAVI<-normImage(SAVI)

#---------------------
# 
# #Normalized difference built-up index (NDBI)----------
# NDBI<-NDBI_func(data_ldst_8,"LANDSAT_8")
# writeRaster(NDBI,"indices/NDBI.tif",overwrite=TRUE)
# NDBI<-normImage(NDBI)
# #-----------------------

# #Normalized Difference Moisture Index(NDMI)--------
# NDMI<-NDMI_func(data_ldst_8,"LANDSAT_8")
# writeRaster(NDMI,"indices/NDMI.tif",overwrite=TRUE)
# NDMI<-normImage(NDMI)
#---------------------------

# #Canopy Shadow Index (CSI)------------
# CSI<-CSI_func(data_ldst_8)
# writeRaster(CSI,"indices/CSI.tif",overwrite=TRUE)
# CSI<-normImage(CSI)
# # #---------------------------

# Nitrogen Reflectance Index (NRI)-----
NRI<-NRI_func(data_ldst_8)
writeRaster(NRI,"indices/NRI.tif",overwrite=TRUE)
NRI<-normImage(NRI)
#---------------------------

# Plant Senescence Reflectance Index (PSRI)----------
PSRI<-PSRI_func(data_ldst_8)
writeRaster(PSRI,"indices/PSRI.tif",overwrite=TRUE)
PSRI<-normImage(PSRI)
#-------------------

# # Brightness index (BI)----------------
# BI<-BI_func(data_ldst_8)
# writeRaster(BI,"indices/BI.tif",overwrite=TRUE)
# BI<-normImage(BI)

#-------------------------

# # Renormalized diference vegetation index(RDVI)---------------
# RDVI<-RDVI_func(data_ldst_8)
# writeRaster(RDVI,"indices/RDVI.tif",overwrite=TRUE)
# RDVI<-normImage(RDVI)
# #--------------------

# Simple ratio pigment index(SRPI)------
SRPI<-SRPI_func(data_ldst_8)
writeRaster(SRPI,"indices/SRPI.tif",overwrite=TRUE)
SRPI<-normImage(SRPI)
#----------------------

#The Normalized Difference Soil Moisture Index, NDSMI--------------
NDSMI<-NDSMI_func(data_ldst_8)
writeRaster(NDSMI,"indices/NDSMI.tif",overwrite=TRUE)
NDSMI<-normImage(NDSMI)
#------------------------------

##Salinity index - SI1
SI1<-SI1_func(data_ldst_8)
writeRaster(SI1,"indices/SI1.tif",overwrite=TRUE)
SI1<-normImage(SI1)

# # ##Salinity index - SI2
# SI2<-SI2_func(data_ldst_8)
# writeRaster(SI2,"indices/SI2.tif",overwrite=TRUE)
# SI2<-normImage(SI2)
# 
# 
# # ##Salinity index - SI3
# SI3<-SI3_func(data_ldst_8)
# writeRaster(SI3,"indices/SI3.tif",overwrite=TRUE)
# SI3<-normImage(SI3)
# # 
# SI4<-SI4_func(data_ldst_8)
# writeRaster(SI4,"indices/SI4.tif",overwrite=TRUE)
# SI4<-normImage(SI4)

SI5<-SI5_func(data_ldst_8)
writeRaster(SI5,"indices/SI5.tif",overwrite=TRUE)
SI5<-normImage(SI5)

# # #Vegetation Soil Salinity Index   - VSSI
# VSSI<-VSSI_func(data_ldst_8)
# writeRaster(VSSI,"indices/VSSI.tif",overwrite=TRUE)
# VSSI<-normImage(VSSI)
# 
# 
# # #Vegetation Soil Salinity Index   - VSSI
# MSI<-Ratio_func(data_ldst_8,7,5)
# writeRaster(MSI,"indices/MSI.tif",overwrite=TRUE)
# MSI<-normImage(MSI)

# #Soil Moisture Index
# SMI<-Ratio_func(data_ldst_8,5,2)
# writeRaster(SMI,"indices/SMI.tif",overwrite=TRUE)
# SMI<-normImage(SMI)
# 
# # #Visible and Shortwave infrared Drought Index
# VSIDI<-VSIDI_func(data_ldst_8)
# writeRaster(VSIDI,"indices/VSIDI.tif",overwrite=TRUE)
# VSIDI<-normImage(VSIDI)

#Normalized Difference infrared Index
NDII<-NDII_func(data_ldst_8)
writeRaster(NDII,"indices/NDII.tif",overwrite=TRUE)
NDII<-normImage(NDII)


# writeRaster(data_ldst_8[[10]],"indices/band10.tif",overwrite=TRUE)
# writeRaster(data_ldst_8[[11]],"indices/band11.tif",overwrite=TRUE)


# R62<-Ratio_func(data_ldst_8,6,2)
# writeRaster(R62,"indices/R62.tif",overwrite=TRUE)
# R62<-normImage(R62)

# writeRaster(data_ldst_8[[5]],"indices/band5.tif",overwrite=TRUE)

#data_ldst_8<-normImage(data_ldst_8)

#covs<-vector(mode="list",length = length(NDVI))

#for (i in 1:length(covs)) {
covs<- raster::stack(NDVI,NDWI,MVI,AMSA,NRI,SI1,SI5,NDSMI,SRPI,NDII)
names(covs) <- c('NDVI', 'NDWI','MVI','AMSA','NRI','SI1','SI5', 'NDSMI','SRPI','NDII')


# names(covs) <- c('ndvi', 'ndwi','bsi','avi','csi',
#                  'savi','msavi','nri','psri','bi',
#                  'rdvi','r62','si1','si2','si3',
#                  'si4','si5','vssi','smi','srpi',
#                  'ndsmi','vsidi','ndii','band1','band2',
#                  'band3','band4','band5','band6','band7',
#                  'band8','band9','band10','band11')
#}

# occ_sol <- readOGR('data/occ_sol_4326/occ_sol4326.shp')
# occ_sol <- spTransform(occ_sol, CRS(proj4string(NDVI)))
# 
# saveRDS(occ_sol,"occ_sol.rds")
# 
# occ_sol<-readRDS("occ_sol.rds") 
# 
# occ_sol@data<-occ_sol@data %>% 
#   mutate(dummy=ifelse(NATURE %in% c("Savane arborÃ©e ou arbustive","ForÃªt"),1,0),
#          dummy1=ifelse(NATURE %in% c("Zone humide"),1,0))

# Dummy <- rasterize(occ_sol, NDVI, field='dummy')
# Dummy[is.na(Dummy)]<-0
# Dummy1 <- rasterize(occ_sol, NDVI, field='dummy')
# Dummy1[is.na(Dummy1)]<-0


samp <- readOGR('data/Niodior_shp/testing/test.shp')
samp <- spTransform(samp, CRS(proj4string(NDVI)))

# occ_sol <- readOGR('data/occ_sol_4326/occ_sol_4326.shp')
# occ_sol <- spTransform(samp, CRS(proj4string(NDVI)))


saveRDS(samp,"samp.rds")

testing<-readRDS("samp.rds")

testing@data$dummy<-0

testing@data<-testing@data %>% 
  mutate(dummy=ifelse(NATURE %in% c("Savane arborÃ©e ou arbustive","ForÃªt"),1,0))


testing@data$Code<-as.numeric(testing@data$id)
r=covs

classes <- rasterize(testing, NDVI, field='Code')

# classes[classes==4]<-3
# classes[classes==5]<-4
# classes[classes==6]<-5

#subset_vect<-c(8,2,3,4,5)
covmasked <- mask(r, classes)

## Combine this new brick with the classes layer to make our input training dataset
names(classes) <- "class"
#names(classesID) <- "classID"

trainingbrick <- addLayer(covmasked, classes)
#plot(trainingbrick)

## Extract all values into a matrix
valuetable <- getValues(trainingbrick)
#valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)
valuetable<-valuetable[complete.cases(valuetable),]


write.csv(valuetable,"donnee_28062020.csv")


for (col in names(valuetable)[1:length(names(valuetable))-1]) {
  valuetable<-rm_outlier(valuetable,col)
}



ggplot(valuetable,aes(y=(MVI-NSDMI),fill=as.factor(class)))+
   geom_boxplot()
# 
# g2<-ggplot(valuetable,aes(y=avi,fill=as.factor(class)))+
#   geom_boxplot()
# g3<-ggplot(valuetable,aes(y=bsi,fill=as.factor(class)))+
#   geom_boxplot()
# g4<-ggplot(valuetable,aes(y=ndwi,fill=as.factor(class)))+
#   geom_boxplot()
# g5<-ggplot(valuetable,aes(y=amsa,fill=as.factor(class)))+
#   geom_boxplot()
# g6<-ggplot(valuetable,aes(y=nri,fill=as.factor(class)))+
#   geom_boxplot()
# 
# ggarrange(g1,g2,g3,g4,g5,g6,nrow = 3,ncol = 2,
#           common.legend = TRUE)

valuetable$class <- factor(valuetable$class, levels = c(1:length(unique(valuetable$class))))

subset_vect<-1:(length(names(valuetable))-1)
#<-setdiff(subset_vect,6)

train <- sample(nrow(valuetable), 0.7*nrow(valuetable), replace = FALSE)
TrainSet <- valuetable[train,c(subset_vect,length(names(valuetable)))]
ValidSet <- valuetable[-train,c(subset_vect,length(names(valuetable)))]
#descriptive analysis
#pairs(r)

#Photointerpretation quality check

modelRF <- randomForest(x=TrainSet[ ,1:length(subset_vect)], y=TrainSet$class,
                        importance = TRUE)


# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg", "class.error")
rownames(modelRF$confusion) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")
modelRF$confusion

training_ac<-sum(diag(modelRF$confusion[,1:6]))/sum(modelRF$confusion[,1:6])
training_ac

sensi_mang<-modelRF$confusion[1:1]/sum(modelRF$confusion[,1])
sensi_mang

d=as.data.frame(modelRF$confusion)

varImpPlot(modelRF)
#testing in the testing set
predValid <- predict(modelRF, ValidSet[,1:length(subset_vect)], type = "class")
table(predValid,ValidSet$class)

e=as.matrix(table(predValid,ValidSet$class))

colnames(e) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")
rownames(e) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")

test_ac<-sum(diag(e))/sum(e)
test_ac

sensi_mangt<-e[1,1]/sum(e[,1])
sensi_mangt



predLC <- raster::predict(r, model=modelRF, na.rm=TRUE)

writeRaster(predLC,"indices/Random_forest_All_new.tif",overwrite=TRUE)


SEN<-  occ_sol <- readOGR('data/occ_sol_4326/occ_sol4326.shp')
SEN <- spTransform(SEN, CRS(proj4string(predLC)))

resultSN<-mask(predLC,SEN)
#pairs <- mapply(list, resultat, ldst_8_date, SIMPLIFY=F)
rsToDf2(resultSN)
#df8<-do.call("rbind",df8)

saloum<- readOGR('data/Saloum/saloum.shp')
saloum <- spTransform(saloum, CRS(proj4string(predLC)))

raster_saloum<-crop(resultSN,saloum)
raster_saloum<-mask(resultSN,saloum)

rsToDf2(raster_saloum)


casamance<- readOGR('data/Casamance/casamance.shp')
casamance <- spTransform(saloum, CRS(proj4string(predLC)))
raster_casa<-crop(resultSN,casamance)
raster_casa<-mask(resultSN,casamance)
rsToDf2(raster_casa)


#extracting NDVI of mangrove
NDVI_real<-raster("indices/NDVI.tif")

NDVI_mangrove<-NDVI_real
## Assign 1 to formask to all cells corresponding to the forest class
NDVI_mangrove[predLC!=1] <- NA

hist(NDVI_mangrove,breaks=150)

MVI_real<-raster("indices/MVI.tif")

MVI_mangrove<-MVI_real
## Assign 1 to formask to all cells corresponding to the forest class
MVI_mangrove[predLC!=1] <- NA


ggplot(melt(as.data.frame(NDVI_mangrove)),aes(value))+geom_histogram(color="white",bins = 100, fill="blue")+
  geom_vline(xintercept = 0.18)+
  xlim(0,0.4)
  theme_classic2()

# hist(MVI_mangrove,breaks=150)
ggplot(melt(as.data.frame(MVI_mangrove)),aes(value))+geom_histogram(color="white",bins = 100, fill="blue")+
  theme_classic2()


NDVI_salinity<-SI1
## Assign 1 to formask to all cells corresponding to the forest class
NDVI_salinity[predLC!=1] <- NA


tan_salinity<-SI1
## Assign 1 to formask to all cells corresponding to the forest class
tan_salinity[predLC!=3 & predLC!=4] <- NA

par(mfrow=c(2,1))
hist(NDVI_salinity,col="blue",xlim=c(-2,2),bins=12)
hist(tan_salinity,col="blue",xlim=c(-2,2))


NDVI_salinity<-SI1
## Assign 1 to formask to all cells corresponding to the forest class
NDVI_salinity[predLC!=1] <- NA

BSI_man<-BSI
## Assign 1 to formask to all cells corresponding to the forest class
BSI_man[predLC!=1] <- NA


NDWI_man<-NRI
## Assign 1 to formask to all cells corresponding to the forest class
NDWI_man[predLC!=1] <- NA

SMI_man<-SMI
## Assign 1 to formask to all cells corresponding to the forest class
SMI_man[predLC!=1] <- NA

SRPI_man<-SRPI
## Assign 1 to formask to all cells corresponding to the forest class
SRPI_man[predLC!=1] <- NA

par(mfrow=c(4,2))
hist(SRPI_man)
boxplot(SRPI_man)

hist(NDVI_mangrove,col="blue")
boxplot(NDVI_mangrove,col="blue")

hist(NDVI_salinity,col="blue")
boxplot(NDVI_salinity,col="blue")

hist(BSI_man,col="blue")
boxplot(BSI_man,col="blue")










#################################################################
# NEUREUL NETWORK
###############################################################
#### Convert Class to dummay variables
vv=length(subset_vect)

# valuetable[,length(names(valuetable))] <- as.numeric(valuetable[,length(names(valuetable))]) -1
point.df<- data.matrix(TrainSet, rownames.force = NA)
point.dft<- data.matrix(ValidSet, rownames.force = NA)


#### Set  `dimnames` to `NULL`
dimnames(point.df) <- NULL
dimnames(point.dft) <- NULL

#dimnames(grid.df) <- NULL

#### Standardize_the data ((x-mean(x))/sd(x))
#point.df[, 1:3] = scale(point.df[, 1:3])
#grid.df[, 1:10] = scale(grid.df[, 1:10])

### Split data 
##  Determine sample size
# ind <- sample(2, nrow(point.df), replace=TRUE, prob=c(0.70, 0.30))
# Split the `Split data
training <- point.df[, 1:vv]
test <- point.dft[, 1:vv]
# Split the class attribute
trainingtarget <- as.numeric(point.df[, (vv+1)])-1
testtarget <- as.numeric(point.dft[, (vv+1)])-1

#### Hyperparameter flag
FLAGS <- flags(
  flag_numeric('dropout_1', 0.2, 'First dropout'),
  flag_numeric('dropout_2', 0.2, 'Second dropout'),
  flag_numeric('dropout_3', 0.1, 'Third dropout'),
  flag_numeric('dropout_4', 0.1, 'Forth dropout'),
  flag_numeric('dropout_5', 0.1, 'Fifth dropout'),
  flag_numeric('dropout_6', 0.1, 'sixth dropout')
  
)

### Define model parameters with 4 hidden layers with 200 neuron
model <- keras_model_sequential()
model %>% 
  # Imput layer
  layer_dense(units = 1000, activation = 'relu', 
              kernel_regularizer =regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001),input_shape = c(vv)) %>% 
  layer_dropout(rate = FLAGS$dropout_1,seed = 1) %>% 
  # Hidden layers
  layer_dense(units =1000, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_2,seed = 1) %>%
  layer_dense(units = 1000, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_3,seed = 1) %>%
  layer_dense(units =1000, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.0001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_4) %>%
  layer_dense(units =350, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.0001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_5) %>%
  layer_dense(units =1000, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.0001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_6) %>%
  # Output layer
  layer_dense(units = 6, activation = 'softmax')
summary(model)

#### Define an optimizer (Stochastic gradient descent optimizer)
optimizer <- optimizer_sgd(lr=0.01, decay=1e-6, momentum=0.9)

#### Compile the model
model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer,
  metrics = 'accuracy'
)

####  Fit the model to the data 
history<-model %>% fit(
  training, trainingtarget, 
  epochs = 100, 
  batch_size = 100, 
  shuffle = TRUE,
  validation_split = 0.2,
  callbacks = callback_tensorboard()
)

### Plot history

#plot(history)

#### Evaluate the model
score <- model %>% evaluate(test, testtarget, batch_size = 100)
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')

#### Prediction & confusion matrix - test data
class.test <- model %>%
  predict_classes(test, batch_size = 100)
CF_DNN<-table(testtarget,class.test)

