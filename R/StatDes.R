
library(dplyr)
library(tidyr)
library(ggplot2)
data<-read.csv("donnee_bis_outcorr.csv")

valuetable<-read.csv('donnee_bis_outcorr.csv')
valuetable<-valuetable %>% dplyr::select(-X)
#Spectral signature of different classes
data<-data %>% dplyr::select(-X) %>% 
  mutate(Classes=case_when(class=="1" ~ "Mangrove",
                           class=="2" ~ "Water",
                           class=="3" ~ "Tanne claire",
                           class=="4" ~ "Tanne foncée",
                           class=="5" ~ "Soil/Built Up",
                           class=="6" ~ "Other vegetation"))

df<-read.csv("data_bands_outcorr.csv")

names(df)<-c("X",paste0("band",1:8),"class")

df<-df %>% dplyr::select(-X) %>% 
  mutate(Classes=case_when(class=="1" ~ "Mangrove",
                           class=="2" ~ "Water",
                           class=="3" ~ "Tanne claire",
                           class=="4" ~ "Tanne foncée",
                           class=="5" ~ "Soil/Built Up",
                           class=="6" ~ "Other vegetation")) %>% 
  dplyr::select(band1,band2,band3,band4,band5,band6,band7,class,Classes) %>%
  group_by(class,Classes) %>% summarise_at(vars(-group_cols()),mean) %>%
  tidyr::gather(key="band",value="Value",-c("Classes","class"))
df$bandNum<-as.numeric(substr(df$band,5,5))
ggplot(df,aes(x=bandNum,y=Value,color=Classes))+geom_line(size=1)+
  geom_point(shape=21,fill="white",size=3)+theme_classic()+
  scale_x_continuous(
    breaks = 1:length(unique(df$band)),
    labels = unique(df$band)
  )+
  labs(title="Sprectral signature of different classes",
       y="mean surface reflectance",x="")


ggplot(data,aes(y=r62,x=Classes,fill=Classes))+
  geom_boxplot(show.legend = F)+theme_classic()

p1=ggplot(data %>% filter(class!=2),aes(x=avi,color=Classes))+
  geom_density()+theme_classic()

p2=ggplot(data %>% filter(class!=2),aes(x=ndvi,color=Classes))+
  geom_density()+theme_classic()

p3=ggplot(data %>% filter(class!=2),aes(x=si1,color=Classes))+
  geom_density()+theme_classic()
p4=ggplot(data %>% filter(class!=2),aes(x=si5,color=Classes))+
  geom_density()+theme_classic()

library(ggpubr)

plot<-ggarrange(p1,p2,p3,p4,nrow = 2,ncol = 2,common.legend = TRUE)
plot

ggsave(file="desity.png",dpi = 600,width = 6,height = 4)

library(GGally)

a=ggpairs(data,columns = 1:14,upper = list(continuous = wrap("cor", size = 3)),
          ggplot2::aes(color=Classes))+theme_bw()+
  theme(axis.ticks=element_blank(), 
          axis.line=element_blank(), 
          axis.text=element_blank(), 
          panel.grid.major= element_blank())

b=ggpairs(data,columns = 1:14,upper = list(continuous = wrap("cor", size = 3)))+theme_bw()+
  theme(axis.ticks=element_blank(), 
          axis.line=element_blank(), 
          axis.text=element_blank(), 
          panel.grid.major= element_blank())



for(i in 1:a$nrow) {
  for(j in 1:a$ncol){
    if(i<j){
      a[i,j]<-b[i,j]
    }
    
  }
}
a


ggsave(file="cor_mat.png",dpi=600,width = 8,height = 8)

data$class <- factor(data$class, levels = c(1:length(unique(data$class))))

subset_vect<-1:4
#<-setdiff(subset_vect,6)

train <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
TrainSet <- data[train,c(subset_vect,length(names(data)))]
ValidSet <- data[-train,c(subset_vect,length(names(data)))]
#descriptive analysis
#pairs(r)

#Photointerpretation quality check

modelRF <- randomForest(x=TrainSet[ ,1:length(subset_vect)], y=TrainSet$class,
                        importance = TRUE)


# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg", "class.error")
rownames(modelRF$confusion) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")
modelRF$confusion
