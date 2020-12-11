library(tidyverse)
library(plot3D)
library(rgl)
#Read datas
X.Train<-read.table(file="uWaveGestureLibrary_X_TRAIN") %>% as_tibble()
Y.Train<-read.table(file="uWaveGestureLibrary_Y_TRAIN") %>% as_tibble()
Z.Train<-read.table(file="uWaveGestureLibrary_Z_TRAIN") %>% as_tibble()

##Question a

sort(unique(X.Train$V1)) # Find unique classes

Random.Instance<-function(class.no){
  #first row number(instance) from each class will be chosen. we can also select randomly.
  row.no<-first(which(X.Train$V1==class.no)) 
  Gesture<-as.data.frame(cbind(t(X.Train[row.no,]),t(Y.Train[row.no,]),t(Z.Train[row.no,]))) #For 3D purpose we bind columns ıf 3 axis
  colnames(Gesture)<-c("X","Y","Z")
  Gesture<-Gesture[-1,]#First row is class info. We do not need it
  rownames(Gesture)<-NULL
  Gesture.v<-as.data.frame(apply(Gesture,2,cumsum))#Acceleration to velocity transformation
  return(Gesture.v)
}

par(mar=c(0,0,0,0))
#1st Class
res<-Random.Instance(1)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one

#2nd Class
res<-Random.Instance(2)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one

#3rd Class
res<-Random.Instance(3)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one

#4th Class
res<-Random.Instance(4)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one

#5th Class
res<-Random.Instance(5)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one

#6th Class
res<-Random.Instance(6)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one

#7th Class
res<-Random.Instance(7)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one

#8th Class
res<-Random.Instance(8)
rgl::plot3d(x = res$X,y = res$Y,z = res$Z,type = "l" ,col = "blue",xlab = "x",ylab = "y",zlab = "z") #Helpful tool. Make rotations to visualize
plot3D::scatter3D(x = res$X,y = res$Y,z = res$Z,col = "blue",type="l",xlab = "x",ylab = "y",zlab = "z") #Hard to visualize with this one



##Question B
new.X.Train<-X.Train %>% rename(class=V1) %>% 
  mutate(time.series.id=row_number()) %>% 
  gather(key = "time.index",value = "X",-c(1,ncol(.)))

new.Y.Train<-Y.Train %>% rename(class=V1) %>% 
  mutate(time.series.id=row_number()) %>% 
  gather(key = "time.index",value = "Y",-c(1,ncol(.)))

new.Z.Train<-Z.Train %>% rename(class=V1) %>% 
  mutate(time.series.id=row_number()) %>% 
  gather(key = "time.index",value = "Z",-c(1,ncol(.)))

Long.Data<-full_join(new.X.Train,new.Y.Train,by=c("class","time.series.id","time.index")) %>% 
  full_join(new.Z.Train,by=c("class","time.series.id","time.index")) %>% 
  separate(time.index,c("V","time.index"),sep = "(?<=V) ?(?=[0-9])") %>% select(-V) %>% 
  mutate(time.index=as.numeric(time.index)-1) %>% 
  arrange(time.series.id,class,time.index) %>% 
  select(time.series.id,time.index,X,Y,Z,class)

pca.data<-Long.Data %>% select(X,Y,Z) %>% as.matrix()

pca.res<-prcomp(pca.data,center = T,scale. = T)$rotation %>% as_tibble()

PC1<-pca.data%*%pca.res$PC1 %>% as_tibble(.name_repair = "minimal")
colnames(PC1)<-"PC1"

pca.final<-Long.Data %>% select(time.series.id,time.index,class) %>% bind_cols(PC1) 

#Randomly select 2 time series from each class
random.sample<-2
pca.test<-pca.final %>% select(class,time.series.id) %>% unique() %>% 
  group_by(class) %>% 
  mutate(row.no=row_number(),
         maxrow.no=max(row.no)) %>% 
  filter(row.no %in% sample(1:maxrow.no[1],random.sample)) %>% 
  ungroup()

pca.final %>% filter(time.series.id %in% pca.test$time.series.id) %>% 
  mutate(class=as.character(class)) %>% 
  ggplot(aes(x=time.index, y=PC1,group=time.series.id, colour=class)) +
  geom_line()
#This is a bit confusing so we remove time.series.id info 

pca.final %>% filter(time.series.id %in% pca.test$time.series.id) %>% 
  mutate(class=as.character(class)) %>% 
  ggplot(aes(x=time.index, y=PC1, colour=class)) +
  geom_line()

#I think with geom_smooth it is easy to visualize
#I think classes can be separated with PCA as can be seen from the plots
pca.final %>% filter(time.series.id %in% pca.test$time.series.id) %>% 
  mutate(class=as.character(class)) %>% 
  ggplot(aes(x=time.index, y=PC1, colour=class)) +
  geom_smooth()



##Question C
#1st Class
pca.tmp<-Long.Data %>% filter(class==1) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#2nd Class
pca.tmp<-Long.Data %>% filter(class==2) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#3rd Class
pca.tmp<-Long.Data %>% filter(class==3) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#4th Class
pca.tmp<-Long.Data %>% filter(class==4) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#5th Class
pca.tmp<-Long.Data %>% filter(class==5) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#6th Class
pca.tmp<-Long.Data %>% filter(class==6) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#7th Class
pca.tmp<-Long.Data %>% filter(class==7) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#8th Class
pca.tmp<-Long.Data %>% filter(class==8) %>% 
  select(X,Y,Z) %>% as.matrix()
pca.res<-prcomp(pca.tmp,center = T,scale.=T)
pca.res$rotation
summary(pca.res)

#With first component more than %50 of Variance is covered for each class.
#There are some patterns in the values of first components. 
#For example, first components of 3rd and 4th classes; 2nd, 5th and 7th classes; 1st and 8th classes are similar to each other.

##Question D
X.Train.dist<-as.matrix(X.Train[,-1])
Y.Train.dist<-as.matrix(Y.Train[,-1])
Z.Train.dist<-as.matrix(Z.Train[,-1])
classes<-X.Train[,1]
colnames(classes)<-"class"

colnames(X.Train.dist)<-paste0("V",1:ncol(X.Train.dist))
colnames(Y.Train.dist)<-paste0("V",1:ncol(Y.Train.dist))
colnames(Z.Train.dist)<-paste0("V",1:ncol(Z.Train.dist))

#Calculate distances between time series (rows)
dist.X<-dist(x=X.Train.dist,method = "euclidean",diag = T,upper = T) %>% as.matrix()
dist.Y<-dist(x=Y.Train.dist,method = "euclidean",diag = T,upper = T) %>% as.matrix()
dist.Z<-dist(x=Z.Train.dist,method = "euclidean",diag = T,upper = T) %>% as.matrix()

dist.all<-dist.X+dist.Y+dist.Z #Sum them up

#Multidimensional scaling to represent on 2d space
Scaled2d<-cmdscale(dist.all,k = 2) %>% as_tibble()
colnames(Scaled2d)<-c("X","Y")

#Visualization
Scaled2d %>%
  bind_cols(classes) %>% 
  mutate(class=as.character(class)) %>% 
  ggplot(aes(x=X,y=Y,colour=class))+
  geom_point(size=4)

#As can be seen from the plot, classes are grouped together meaning that samples within classes are close.
#In other words, multidimensional scaling works for separating classes.
























