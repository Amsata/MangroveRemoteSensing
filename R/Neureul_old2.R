#### Import packages
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

source("R/fonctions.R")
shp<-readRDS('data/gadm36_SEN_4_sf.rds')
senegal<- readOGR('data/SEN_adm_shp/SEN_adm4.shp')
#polyg <- senegal[senegal$NAME_3%in% c("Niodior"),]
# polyg <- senegal[senegal$NAME_3%in% c("Niodior",
#                                       "Toubacouta",
#                                       "Fimela",
#                                       "Sessene"),]
# polyg <- senegal[senegal$NAME_2%in% c("Fatick",
#                                       "Foundiougne"),]

polyg<- readOGR('data/studied_area/studied_area.shp')


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
  filter(CLOUD_COVER==min(CLOUD_COVER)) %>% 
  ungroup() %>% 
  filter(WRS_ROW==50) %>% 
  filter(SPACECRAFT_ID %in% c(" LANDSAT_8")) %>% 
  filter(CLOUD_COVER<=2)

touse$SPACECRAFT_ID<-gsub(" ","",touse$SPACECRAFT_ID)

satellite<-touse$SPACECRAFT_ID[c(7,11)]
ldst_8<-unique(touse$FICHIER)[c(7,11)]

ldst_8_date<-unique(touse$DATE_ACQUIRED)[c(7,11)]

data_ldst_8<-lapply(ldst_8,readLandsat)

NDVI<-mapply(NDVI_func,data_ldst_8,satellite)
NDWI<-mapply(NDWI_func,data_ldst_8,satellite)
AMSA<-mapply(AMSA_func,data_ldst_8,satellite)
BSI<-mapply(BSI_func,data_ldst_8,satellite)
AVI<-lapply(data_ldst_8, AVI_func)
ADD<-lapply(data_ldst_8,ADD_func)

B6<-lapply(data_ldst_8, function(img){img[[6]]})
B7<-lapply(data_ldst_8, function(img){img[[7]]})

PCA67=mapply(PCA_raster,B6,B7)
# Graphicx
# trellis.device('png', file = 'myPic%02d.png')
# rasterVis::levelplot(stack(NDVI),scales=list(draw=FALSE)) 
# # levelplot(s, layout = c(3, 4))
# dev.off()

AVI<-lapply(AVI,normImage)
BSI<-lapply(BSI,normImage)
NDWI<-lapply(NDWI,normImage)
AMSA<-lapply(AMSA,normImage)
NDVI<-lapply(NDVI,normImage)
data_ldst_8<-lapply(data_ldst_8,normImage)
ADD<-lapply(ADD67,normImage)
PCA67<-lapply(PCA67,normImage)



writeRaster(AVI[[2]],"indices/AVI_NEW.tif",overwrite=TRUE)
writeRaster(ADD67[[2]],"indices/ADD67_NEW.tif",overwrite=TRUE)
writeRaster(NDVI[[2]],"indices/NDVI_NEW.tif",overwrite=TRUE)
writeRaster(AMSA[[2]],"indices/AMSA_NEW.tif",overwrite=TRUE)
writeRaster(NDWI[[2]],"indices/NDWI_NEW.tif",overwrite=TRUE)
writeRaster(BSI[[2]],"indices/BSI_NEW.tif",overwrite=TRUE)
writeRaster(PCA67[[2]],"indices/PCA67_NEW.tif",overwrite=TRUE)

covs<-vector(mode="list",length = length(NDVI))

for (i in 1:length(covs)) {
  covs[[i]] <- raster::stack(NDVI[[i]],NDWI[[i]],BSI[[i]],AVI[[i]],AMSA[[i]],ADD[[i]],
                             data_ldst_8[[i]][[1]],data_ldst_8[[i]][[2]],
                             data_ldst_8[[i]][[3]],data_ldst_8[[i]][[4]],
                             data_ldst_8[[i]][[5]],data_ldst_8[[i]][[6]],
                             data_ldst_8[[i]][[7]],PCA67[[i]])
  
  names(covs[[i]]) <- c('ndvi', 'ndwi','bsi','avi','amsa','add',
                        'band1','band2','band3','band4',
                        'band5','band6','band7','pca67')
}

samp <- readOGR('data/Niodior_shp/testing/test.shp')
samp <- spTransform(samp, CRS(proj4string(NDVI[[1]])))

saveRDS(samp,"samp.rds")

testing<-readRDS("samp.rds")

testing@data$Code<-as.numeric(testing@data$id)

classes <- rasterize(testing, NDVI[[1]], field='Code')
#classesID <- rasterize(testing, NDVI[[1]], field='ID2')

classes[classes==4]<-3
classes[classes==5]<-4
classes[classes==6]<-5

r=covs[[2]]
subset_vect<-c(1,2,3,4,5)
covmasked <- mask(r, classes)

## Combine this new brick with the classes layer to make our input training dataset
names(classes) <- "class"
#names(classesID) <- "classID"

trainingbrick <- addLayer(covmasked, classes)
#plot(trainingbrick)

## Extract all values into a matrix
valuetable <- getValues(trainingbrick)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)

for (col in names(valuetable)[1:length(names(valuetable))-1]) {
  valuetable<-rm_outlier(valuetable,col)
}

head(valuetable, n = 10)
valuetable$class <- factor(valuetable$class, levels = c(1:5))

# ggplot(valuetable,aes(y=bsi,fill=as.factor(class)))+
#   geom_boxplot()

#pairs(r)

# graph<-function(df,xx,yy){
#   
#   df1<-df %>% 
#     mutate(Classes=case_when(class=="1" ~ "Mangrove",
#                              class=="2" ~ "Water",
#                              class=="3" ~ "Tanne",
#                              class=="4" ~ "Soil/Built Up",
#                              class=="5" ~ "Other vegetation"))
#   
#   
#   ggplot(df1,aes_string(x=xx,y=yy,color="Classes"))+
#     geom_point()+theme_bw()+xlim(-2,2)
# }
# 
# plt1<-graph(valuetable,"ndvi","ndwi")
# plt2<-graph(valuetable,"ndvi","bsi")
# plt3<-graph(valuetable,"bsi","ndwi")
# plt4<-graph(valuetable,"ndvi","avi")
# plt5<-graph(valuetable,"ndwi","avi")
# plt6<-graph(valuetable,"bsi","avi")
# 
# ggarrange(plt1,plt2,plt3,plt4,plt5,plt6,
#           nrow = 3,ncol = 2,common.legend=TRUE)

#### Convert Class to dummay variables
valuetable[,length(names(valuetable))] <- as.numeric(valuetable[,length(names(valuetable))]) -1
point.df<- data.matrix(valuetable[,c(subset_vect,length(names(valuetable)))], rownames.force = NA)
grid.df <- data.matrix(valuetable[,subset_vect], rownames.force = NA)
#### Set  `dimnames` to `NULL`
dimnames(point.df) <- NULL
#dimnames(grid.df) <- NULL

#### Standardize_the data ((x-mean(x))/sd(x))
#point.df[, 1:3] = scale(point.df[, 1:3])
#grid.df[, 1:10] = scale(grid.df[, 1:10])

### Split data 
##  Determine sample size
ind <- sample(2, nrow(point.df), replace=TRUE, prob=c(0.70, 0.30))
# Split the `Split data
training <- point.df[ind==1, 1:5]
test <- point.df[ind==2, 1:5]
# Split the class attribute
trainingtarget <- as.numeric(point.df[ind==1, 6])
testtarget <- point.df[ind==2, 6]

#### Hyperparameter flag
FLAGS <- flags(
  flag_numeric('dropout_1', 0.2, 'First dropout'),
  flag_numeric('dropout_2', 0.2, 'Second dropout'),
  flag_numeric('dropout_3', 0.1, 'Third dropout'),
  flag_numeric('dropout_4', 0.1, 'Forth dropout')
)

### Define model parameters with 4 hidden layers with 200 neuron
model <- keras_model_sequential()
model %>% 
  # Imput layer
  layer_dense(units = 100, activation = 'relu', 
              kernel_regularizer =regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001),input_shape = c(5)) %>% 
  layer_dropout(rate = FLAGS$dropout_1,seed = 1) %>% 
  # Hidden layers
  layer_dense(units =100, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_2,seed = 1) %>%
  layer_dense(units = 100, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_3,seed = 1) %>%
  layer_dense(units =100, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.0001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_4) %>%
  # Output layer
  layer_dense(units = 5, activation = 'softmax')
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
table(testtarget,class.test)

#### Predicted Class Probability

prob.test <- model %>%
  predict_proba(test, batch_size = 100)

#### Prediction at grid data

Predict_func<-function(ras) {
  ras<- model %>%
    predict_classes(as.matrix(ras), batch_size = 100)
  ras <- setValues(NDVI[[1]], as.numeric(ras))
  return(ras)
}


#clear some space
#rm(list = c("BSI","MSAVI","data_ldst_8",
#            "NDWI","NDMI","NDBI","CSI","AVI"))
a=lapply(covs, function(x){subset(x,subset_vect)})

resultat<-lapply(a,Predict_func)

pairs <- mapply(list, resultat, ldst_8_date, SIMPLIFY=F)
df8<-lapply(pairs, rsToDf)
df8<-do.call("rbind",df8)

#after <- setValues(NDVI[[1]], as.numeric(after))

myPalette <- c("#228B22","blue","yellow", "#FF8C00","#00FF00")
class.name=c("Mangrove", "Eaux", "Tanne","sol","Other vegetations")

# for (i in 1:length(a)) {
#   levels(a[[i]])=data.frame(ID=0:4, code=class.name)
#   
# }
# 
# a=stack(a)
# rasterVis::levelplot(a,col.regions=myPalette,
#                      colorkey=list(space="bottom"))

before<-resultat[[1]]
after<-resultat[[2]]

formask <- setValues(NDVI[[1]], NA)
## Assign 1 to formask to all cells corresponding to the forest class
formask[before==0 & after>0] <- -1
formask[before==0 & after==0] <- 0
formask[before>0 & after==0] <- 1
#plot(formask,col=c("red","yellow"," dark green"))
levels(formask)=data.frame(ID=-1:1, code=c("Loss","Stable","Gain"))

rasterVis::levelplot(formask,col.regions=c("red","yellow"," dark green"))

writeRaster(formask,"indices/change_New.tif",overwrite=TRUE)
writeRaster(before,"indices/before.tif",overwrite=TRUE)
writeRaster(after,"indices/after.tif",overwrite=TRUE)

crosstab(before,after)*0.09

#writeRaster(,"Neureul_nectwork2013.tif",overwrite=TRUE)
#### Detach keras, tfruns, tftestimators

detach(package:keras, unload=TRUE)
detach(package:tfruns, unload=TRUE)
detach(package:tfestimators, unload=TRUE)

#### Convert to raster


#### Clean everyrhing

gc()

