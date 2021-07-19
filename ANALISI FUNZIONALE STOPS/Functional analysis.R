#####################################
#### CLUSTER WEEKEND AND WEEKDAY ####
#####################################


library(fda)
library(ggplot2)
library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(raster)
library(rgdal)

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/River_Dataset.RData")

#--------------------------------------------------------------------
# Build dataset 

New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]
attach(New_York_County_no_river)

stops<-matrix(nrow = 1092, ncol=30)

for (i in 1:dim(New_York_County_no_river)[1]) {
  stops[i,]<-stops_by_day[[i]]
}

detach(New_York_County_no_river)

stops<-t(stops)
colnames(stops)<-New_York_County_no_river$area
x11()
matplot(stops,type='l' , main = "Stops by day")


#B-SPLINES
# Set parameters
nbasis <- 13
m <- 3+1        # spline order 
# spline degree  #DEGREE = ORDER - 1
basis <- create.bspline.basis(rangeval=c(1,30), nbasis=nbasis, norder=m)


time=1:30
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
x11()
plot.fd(data_W.fd.1, main ="Smoothing stops by day")


# FPCA

arm=5 #numero armoniche
plot.fd(data_W.fd.1)
pca_W.1 <- pca.fd(data_W.fd.1,nharm=arm,centerfns=TRUE) #build a functional object before run it -> smoothing preprocessing

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first are non null
plot(pca_W.1$values[1:5],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

# scatter plot of the scores
par(mfrow=c(1,2))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)

plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2", main="Scatterplot")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)


# outliers
out=c(92,532)
cbg_out=New_York_County_no_river[out,1]
layout(1)
x11()
matplot(stops,type='l', main= "Stops by day - outliers")
lines(stops[,92],lwd=4, col=2) 
lines(stops[,532],lwd=4, col=1) 
 
x11()
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill="cornsilk")+
  geom_sf(data = CBG_ny_no_river$geometry[out], fill="chartreuse")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  ggtitle("Influential Points")

# togliamo questi outliers
stops<-stops[,-out]

x11()
matplot(stops,type='l', main = "Stops by Day")

abscissa<-1:30
Xobs0<-stops

nbasis <- 22
basis <- create.fourier.basis(rangeval=c(1,30),nbasis=nbasis) # creates a fourier basis

time=1:30
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
plot.fd(data_W.fd.1)



# FPCA

arm=5 #numero armoniche
pca_W.1 <- pca.fd(data_W.fd.1,nharm=arm,centerfns=TRUE) #build a functional object before run it -> smoothing preprocessing

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first are non null
plot(pca_W.1$values[1:5],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)


x11()
media <- mean.fd(data_W.fd.1)
par(mfrow=c(1,2))
plot(media,lwd=2,ylim=c(-20,300),ylab='Stops by day',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
abline(v=seq(1,30, by=7),lty=2)
title("FPC1")

plot(media,lwd=2,ylim=c(0,170),ylab='Stops by day',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
abline(v=seq(1,30, by=7), lty=2)
title("FPC2")
# temperate climate or not


# scatter plot of the scores
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=New_York_County_no_river$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)


influential<- which(abs(pca_W.1$scores[,2])>500)

x11()
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill="cornsilk")+
  geom_sf(data = CBG_ny_no_river$geometry[out], fill="chartreuse")+
  geom_sf(data = CBG_ny_no_river$geometry[influential], fill="red")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  ggtitle("Influential Points")


#................................................................................
# proviamo a prendere quelli con stops>250

New_York_County_no_river_no_river=New_York_County_no_river_no_river[order(New_York_County_no_river_no_river$area),]
attach(New_York_County_no_river_no_river)

stops<-matrix(ncol=30, nrow = 124)
k=1
index<-c()
for (i in 1:dim(New_York_County_no_river_no_river)[1]) {
  if (max(stops_by_day[[i]])>250) {
  stops[k,]<-stops_by_day[[i]]
  index[k]<-i
  k=k+1
  }
}

detach(New_York_County_no_river_no_river)
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill="black")+
  geom_sf(data = CBG_ny_no_river$geometry[index], fill="red")


stops<-t(stops)
x11()
par(mfrow=(c(2,2)))
for(i in 1:4){
  matplot(stops[(i*7-6):(i*7),],type='l')
}
x11()
matplot(stops, type="l")

#B-SPLINES
# Set parameters
nbasis <- 12
m <- 2+1        # spline order 
# spline degree  #DEGREE = ORDER - 1
basis <- create.bspline.basis(rangeval=c(1,30), nbasis=nbasis, norder=m)

x11()
time=1:30
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
plot.fd(data_W.fd.1)


# FPCA

arm=5 #numero armoniche
plot.fd(data_W.fd.1)
pca_W.1 <- pca.fd(data_W.fd.1,nharm=arm,centerfns=TRUE) #build a functional object before run it -> smoothing preprocessing

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first are non null
plot(pca_W.1$values[1:5],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

# scatter plot of the scores
par(mfrow=c(1,2))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)

plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=New_York_County_no_river$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)


# outliers
out=c(19,75)
cbg_out=New_York_County_no_river[out,1]
layout(1)
x11()
matplot(stops,type='l')
lines(stops[,19],lwd=4, col=2) 
lines(stops[,75],lwd=4, col=1) 
 

# togliamo questi outliers
stops<-stops[,-out]
x11()
matplot(stops,type='l')



x11()
par(mfrow=(c(2,2)))
for(i in 1:4){
  matplot(stops[(i*7-6):(i*7),],type='l', ylim=c(0,1500))
}



nbasis <- 20
basis <- create.fourier.basis(rangeval=c(1,30),nbasis=nbasis) # creates a fourier basis

time=1:30
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
plot.fd(data_W.fd.1)



# FPCA

arm=5 #numero armoniche
pca_W.1 <- pca.fd(data_W.fd.1,nharm=arm,centerfns=TRUE) #build a functional object before run it -> smoothing preprocessing

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first are non null
plot(pca_W.1$values[1:5],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

x11()
par(mfrow=c(1,2))
media <- mean.fd(data_W.fd.1)

plot(media,lwd=2,ylim=c(0,700),ylab='temperature',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
abline(v=seq(1,30, by=7))

plot(media,lwd=2,ylim=c(150,500),lab='temperature',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
abline(v=seq(1,30, by=7))

# scatter plot of the scores
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=New_York_County_no_river$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)

x11()
matplot(stops,type='l')
lines(stops[,58],lwd=4, col=1) 
lines(stops[,3],lwd=4, col=1) 


#--------------------------------------------------------------------
# proviamo a prendere quelli con stops<250

New_York_County_no_river_no_river=New_York_County_no_river_no_river[order(New_York_County_no_river_no_river$area),]
attach(New_York_County_no_river_no_river)

stops<-matrix(ncol=30, nrow = 964)
k=1
index<-c()
for (i in 1:dim(New_York_County_no_river_no_river)[1]) {
  if (max(stops_by_day[[i]])<250) {
    stops[k,]<-stops_by_day[[i]]
    index[k]<-i
    k=k+1
  }
}

detach(New_York_County_no_river_no_river)
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill="black")+
  geom_sf(data = CBG_ny_no_river$geometry[index], fill="red")


stops<-t(stops)
x11()
par(mfrow=(c(2,2)))
for(i in 1:4){
  matplot(stops[(i*7-6):(i*7),],type='l')
}

x11()
matplot(stops,type='l')


#B-SPLINES
# Set parameters
nbasis <- 17
m <- 3+1        # spline order 
# spline degree  #DEGREE = ORDER - 1
basis <- create.bspline.basis(rangeval=c(1,30), nbasis=nbasis, norder=m)


time=1:30
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
x11()
plot.fd(data_W.fd.1)


# FPCA

arm=5 #numero armoniche
plot.fd(data_W.fd.1)
pca_W.1 <- pca.fd(data_W.fd.1,nharm=arm,centerfns=TRUE) #build a functional object before run it -> smoothing preprocessing

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first are non null
plot(pca_W.1$values[1:5],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

# scatter plot of the scores
par(mfrow=c(1,2))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)

plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=New_York_County_no_river$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)


#-------------------------------------------------------------
#plot derivatives

# Evaluate the basis on the grid of abscissa
abscissa<-time
basismat <- eval.basis(abscissa, basis) #
dim(basismat)
head(basismat)

Xobs0<-stops

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis) # easier because it includes also penalization (see later)
Xsp0 <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data
Xsp1 <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative


x11()
matplot(Xsp1, type ="l")


#---------------------------------------------------------------------
# proviamo a fare clustering, magari troviamo i due comportamenti che vediamo
# ALIGNMENT

x<-t(matrix((time)))
Xsp <- smooth.basis(argvals=time, y=stops, fdParobj=basis) # easier because it includes also penalization (see later)
Xsp0 <- eval.fd(time, Xsp$fd) #  the curve smoothing the data
Xsp1 <- eval.fd(time, Xsp$fd, Lfd=1) # first derivative

library(fdakma)

fdakma_example <- kma(
  x=x, 
  y0=t(Xsp0),
  y1=t(Xsp1), 
  n.clust = 2, 
  warping.method =  'NOalignment', # trasformation of an axis in order to do align
  similarity.method = 'd1.pearson',  # similarity computed as the cosine
  # between the first derivatives 
  # (correlation)
  center.method = 'k-means',
  seeds = c(58,3) # you can give a little help to the algorithm...
)

kma.show.results(fdakma_example)


kma.compare_example2 <- kma.compare (
  x=x, y0=t(Xsp0), 
  #y1=t(Xsp1), 
  n.clust = 2:4, 
  warping.method = c('affine', 'dilatation'), 
  similarity.method = 'd0.pearson',
  center.method = 'k-means', 
  #seeds = c(1,21,30),
  plot.graph=1)

