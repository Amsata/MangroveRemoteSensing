memory.limit()
memory.limit(9999999999)

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

tensorflow::install_tensorflow()

rasterOptions(tmpdir='E:\\remote sensing with R\\RemoteSensing\\Temp')


source("R/fonctions.R")

list_bands<-list.files("data/bands")
list_bands_cmplt<-paste0("data/bands/",list_bands)
data_ldst<-stack(list_bands_cmplt)

vect<-c("AMSA","AVI","NDII","NDSMI","NDVI","NDWI","NRI","PSRI","R62","BSI","MSAVI","MSI","SI1","SI5")

#list_indice<-list.files("indices/touse")

# list_complete<-paste0("indices/touse/",list_indice)
list_complete<-paste0("indices/touse/",vect,".tif")

NDVI<-raster("indices/NDVI.tif")

data_list<-lapply(list_complete,raster)

data_list<-lapply(data_list,normImage)

data_list_1<-stack(data_list)
names(data_list_1)<-vect
covs<-stack(data_ldst,data_list_1)
#names(covs)<-gsub(".tif","",list_indice)

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
rm(covs)
classes <- rasterize(testing, NDVI, field='Code')

# classes[classes==1 & NDVI<0.2]<-NA
# classes[classes==4]<-3
# classes[classes==5]<-4
# classes[classes==6]<-5
# 
# #subset_vect<-c(8,2,3,4,5)
# ndviMant <- NDVI
# ndviMant[classes!=1]<-NA
# ndviMant[is.na(classes)]<-NA
# 
# 
# #NDVI analysis
# #ndviMask <- mask(NDVI, classes)
# writeRaster(ndviMant,"indices/NDVI_mangrove_class.tif",overwrite=TRUE)
rm(data_list_1)
covmasked <- mask(r, classes)
#covmasked1 <- mask(data_ldst, classes)

## Combine this new brick with the classes layer to make our input training dataset
names(classes) <- "class"
#names(classesID) <- "classID"

trainingbrick <- addLayer(covmasked, classes)
#trainingbrick1 <- addLayer(covmasked1, classes)

#plot(trainingbrick)
#subvec<-c(9,11:16,3,5,6,20,24,30)
rm(covmasked)
rm(classes)
#subset1<-c(7,19,1,25,27)
xx<-trainingbrick
#xx<-subset(trainingbrick,subvec)
## Extract all values into a matrix
valuetable0 <- getValues(xx)
#valuetable <- na.omit(valuetable)
valuetable0 <- as.data.frame(valuetable0)
valuetable0<-valuetable0[complete.cases(valuetable0),]



# yy<-subset(trainingbrick,subset1)
# valuetable1 <- getValues(yy)
# valuetable1 <- as.data.frame(valuetable1)
# valuetable1<-valuetable1[complete.cases(valuetable1),]
# valuetable1<-valuetable1[,c("NDBI", "SAVI", "AMSA", "SMI","VCI")]

valuetable=valuetable0
write.csv(valuetable,"data/valuetable_final.csv")


d<-read.csv("data/valuetable_final.csv")
#valuetable<-read.csv("data/valuetable_final.csv")
# valuetable<-valuetable %>% select(-X) %>% mutate(class=case_when(class==1~1,
#                                                                  class==2~2,
#                                                                  class==3~3,
#                                                                  class==4~3,
#                                                                  class==5~4,
#                                                                  class==6~5))

# valuetable2 <- getValues(trainingbrick1)
# valuetable2 <- as.data.frame(valuetable2)
# valuetable2<-valuetable2[complete.cases(valuetable2),]
# 
# write.csv(valuetable2,"data/valuetable_bands.csv")

#valuetable2<-read.csv("data/valuetable_bands.csv")

# for (col in names(valuetable0)[1:length(names(valuetable0))-1]) {
#   valuetable0<-rm_outlier(valuetable0,col)
# }

# valuetable<-valuetable2 %>% select(-class) %>% left_join(valuetable) %>% 
#   select(-X)


for (col in names(valuetable)[1:length(names(valuetable))-1]) {
  valuetable<-rm_outlier(valuetable,col)
}

# write.csv(valuetable1,"data/valuetable1.csv")


for (col in names(valuetable2)[1:length(names(valuetable2))-1]) {
  valuetable2<-rm_outlier(valuetable2,col)
}


# to keep for valuetable 1

# NDBI, SAVI, SI2, SMI,VCI,
# valuetable1<-valuetable1[,c("NDBI", "SAVI", "SI2", "SMI","VCI")]

ggplot(valuetable,aes(y=Band8,fill=as.factor(class)))+
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

# valuetable<-valuetable2
valuetable$class <- factor(valuetable$class)

subset_vect<-1:(length(names(valuetable))-1)

# subset_vect<-setdiff(subset_vect,c(14,15))

#subset_vect<-c(6,7,8,9,10,11,12,13,16,17)


#<-setdiff(subset_vect,6)

train <- sample(nrow(valuetable), 0.7*nrow(valuetable), replace = FALSE)
TrainSet <- valuetable[train,]
ValidSet <- valuetable[-train,]
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
CM_RF<-table(predValid,ValidSet$class)
CM_RF
e=as.matrix(table(predValid,ValidSet$class))

colnames(e) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")
rownames(e) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")

test_ac<-sum(diag(e))/sum(e)
test_ac

sensi_mangt<-e[1,1]/sum(e[,1])
sensi_mangt
#############################################################################################
#######################SUPPORT VECTOR MACHINE
########################################################################################
#SVM

library(e1071)

x <- subset(TrainSet, select = -class)
xx <- subset(ValidSet, select = -class)
y <- TrainSet$class
model <- svm(x, y) 

pred <- predict(model, xx)
# (same as:)
#pred <- fitted(model)

# Check accuracy:
CM_SVM<-table(pred, ValidSet$class)


#naive bayes
Naive_Bayes_Model=naiveBayes(class ~., data=TrainSet)
NB_Predictions=predict(Naive_Bayes_Model,ValidSet)
#Confusion matrix to check accuracy
table(NB_Predictions,ValidSet$class)
#################################################################
# NEUREUL NETWORK
###############################################################
#### Convert Class to dummay variables
vv=length(subset_vect)

valuetable[,length(names(valuetable))] <- as.numeric(valuetable[,length(names(valuetable))]) -1
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


CF_DNN
CF_ML_DNN
CM_SVM
CM_RF
df_list<-list(CF_DNN,CF_ML_DNN,CM_SVM,CM_RF)

df1<-CMtoDf(CF_DNN,"DNN")
df2<-CMtoDf(CF_ML_DNN,"ML DNN")
df3<-CMtoDf(CM_SVM,"SVM")
df4<-CMtoDf(CM_RF,"RF")

df_final<-rbind(df1,df2,df3,df4)


df_final<-df_final %>% spread(key = Method,value = CF)






v=setdiff(names(TrainSet),"class")
# r=subset(trainingbrick,v)
r=covs
xxx=subset(r,v)
rm(covs)
rm(r)
predLC <- raster::predict(xxx, model=modelRF, na.rm=TRUE)

writeRaster(predLC,"indices/Random_forest_All_new_3004.tif",overwrite=TRUE)


SEN<-  occ_sol <- readOGR('data/occ_sol_4326/occ_sol4326.shp')
SEN <- spTransform(SEN, CRS(proj4string(predLC)))

resultSN<-mask(predLC,SEN)
#pairs <- mapply(list, resultat, ldst_8_date, SIMPLIFY=F)
writeRaster(resultSN,"indices/Random_forest_SN.tif",overwrite=TRUE)






rsToDf2(resultSN)
#df8<-do.call("rbind",df8)

resSN<-resultSN
resSN[NDVI<0.2]<-NA

saloum<- readOGR('data/Saloum/saloum.shp')
saloum <- spTransform(saloum, CRS(proj4string(predLC)))

raster_saloum<-crop(resultSN,saloum)
raster_saloum<-mask(resultSN,saloum)
writeRaster(raster_saloum,"indices/Random_forest_Saloum.tif",overwrite=TRUE)

rsToDf2(raster_saloum)


casamance<- readOGR('data/Casamance/casamance.shp')
casamance <- spTransform(casamance, CRS(proj4string(predLC)))
raster_casa<-crop(resultSN,casamance)
raster_casa<-mask(resultSN,casamance)
rsToDf2(raster_casa)

writeRaster(raster_casa,"indices/Random_forest_casa.tif",overwrite=TRUE)

#extracting NDVI of mangrove

NDVI_mangrove<-NDVI
NDVI_mangrove[predLC!=1] <- NA
writeRaster(NDVI_mangrove,"indices/NDVI_mangrove.tif",overwrite=TRUE)

tan_NDVI<-NDVI
tan_NDVI[predLC!=3 & predLC!=4] <- NA

par(mfrow=c(2,1))
hist(NDVI_mangrove,col="blue",xlim=c(-0.2,0.5),bins=12)
hist(tan_NDVI,col="blue",xlim=c(-0.2,0.5))

tan_ndvi_vec<-getValues(tan_NDVI)
ub_tan<-quantile(tan_ndvi_vec,0.75,na.rm=T)+1.5*(quantile(tan_ndvi_vec,0.75,na.rm=T)-quantile(tan_ndvi_vec,0.25,na.rm=T))

man_ndvi_vec<-getValues(NDVI_mangrove)
lb_man<-quantile(man_ndvi_vec,0.25,na.rm=T)-1.5*(quantile(man_ndvi_vec,0.75,na.rm=T)-quantile(man_ndvi_vec,0.25,na.rm=T))


SI1<-raster("indices/SI1.tif")

mang_salinity<-SI1
mang_salinity[predLC!=1] <- NA

tan_salinity<-SI1
tan_salinity[predLC!=4] <- NA

par(mfrow=c(2,1))
hist(mang_salinity,col="blue")
hist(tan_salinity,col="blue")


NDVI_salinity<-SI1
## Assign 1 to formask to all cells corresponding to the forest class
NDVI_salinity[predLC!=1] <- NA

BSI_man<-BSI
## Assign 1 to formask to all cells corresponding to the forest class
BSI_man[predLC!=1] <- NA


