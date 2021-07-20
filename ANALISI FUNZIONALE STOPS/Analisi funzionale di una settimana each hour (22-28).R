########################################################
#### FUNCTIONAL ANALYSIS - ONE WEEK (22/06 - 29/06) ####
########################################################


library(fda)
library(ggplot2)
library(sf)

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/River_Dataset.RData")


#--------------------------------------------------------------------
# Build dataset 

New_York_County_no_river= New_York_County_no_river[order( New_York_County_no_river$area),]
CBG_ny_no_river<-CBG_ny_no_river[order( CBG_ny_no_river$CensusBlockGroup),]
attach( New_York_County_no_river)

stops<-matrix(nrow = 1092, ncol=30*24)

for (i in 1:dim( New_York_County_no_river)[1]) {
  stops[i,]<-stops_by_each_hour[[i]]
}

detach( New_York_County_no_river)

stops<-stops[,504:671]

stops<-t(stops)
colnames(stops)<- New_York_County_no_river$area
x11()
matplot(stops,type='l', main = "22/06 - 28/06")
abline(v=seq(1,168, by=24), lty=2)



nbasis <- 28 # number of basis

# Create the basis
#FOURIER
basis <- create.fourier.basis(rangeval=c(1,168),nbasis=nbasis) # creates a fourier basis

time=1:168
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
x11()
plot.fd(data_W.fd.1)
abline(v=seq(1,168, by=24), lty=2)
title("22/06 - 28/06 (smoothing)")



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

x11()
par(mfrow=c(1,2))
media <- mean.fd(data_W.fd.1)

plot(media,lwd=2,ylim=c(-15,33),ylab='Stops by each hour',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
abline(v=seq(1,168, by=24), lty=2)
title('FPC1')

plot(media,lwd=2,ylim=c(-2,20),lab='Stops by each hour',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
abline(v=seq(1,168, by=24), lty=2)
title('FPC2')

# PC2 -> contrasto tra weekend e weekday
# PC1 -> media


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
out=c(92,532)
cbg_out=New_York_County_no_river[out,1]
layout(1)
x11()
matplot(stops,type='l')
lines(stops[,92],lwd=4, col=2) 
lines(stops[,532],lwd=4, col=1) 


# togliamo questi outliers
stops<-stops[,-out]


x11()
matplot(stops,type='l',main = "22/06 - 28/06")
abline(v=seq(1,168, by=24), lty=2)


abscissa<-1:168
Xobs0<-stops

nbasis <- 80 # number of basis

# Create the basis
#FOURIER
basis <- create.fourier.basis(rangeval=c(1,168),nbasis=nbasis) # creates a fourier basis

time=1:168
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
x11()
plot.fd(data_W.fd.1)
title("22/06 - 28/06 (smoothing)")
abline(v=seq(1,168, by=24), lty=2)

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



x11()
media <- mean.fd(data_W.fd.1)
par(mfrow=c(3,1))
plot(media,lwd=2,ylim=c(-25,31),ylab='Stops by each hour ~ 22/06 - 28/06',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
abline(v=seq(1,168, by=24), lty=2, lwd=3)
abline(v=seq(1,168, by=6), lty=3)
title("FPC1 - 72.5% of variability")

plot(media,lwd=2,ylim=c(-2,17),ylab='Stops by each hour ~ 22/06 - 28/06',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
abline(v=seq(1,168, by=24), lty=2, lwd=3)
abline(v=seq(1,168, by=6), lty=3)
title("FPC2 - 11.8% of variability")

plot(media,lwd=2,ylim=c(-2,17),ylab='Stops by each hour ~ 22/06 - 28/06',main='FPC2')
lines(media+pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=3)
lines(media-pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=2)
abline(v=seq(1,168, by=24), lty=2, lwd=3)
abline(v=seq(1,168, by=6), lty=3)
title("FPC3 - 4.8% of variability")

#PC1>0 cbg che hanno più stops by each hour rispetto alla media, 
#PC1<0 ci sono meno stops by each hour rispetto alla media (magari sono i cbg più affollati dove la gente non si ferma ma cammina e basta)
#PC2>0 cbg che hanno più stops by each hour durante la notte rispetto alla media, meno stops by each hour durante il giorno rispetto alla media e più stops by each hour di sera rispetto alla media
#PC2<0 cbg con più stops by each hour di giorno e meno stops by each hour di sera rispetto alla media

x11()
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



#------------------------------------------------------------------------------
# CBG LAVORATIVI 
# quelli che hanno PC2<0

cbg_work<-which(pca_W.1$scores[,2]<0)
stops_cbg_work<-stops[,cbg_work]

x11()
matplot(stops_cbg_work,type='l',main = "More frequented CBG during 8.00-18.00 (22/06 - 28/06)")
abline(v=seq(1,168, by=24), lty=2)

x11()
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry) +
  geom_sf(data = CBG_ny_no_river$geometry[cbg_work], fill="red") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue") +ggtitle("CBG more frequented during 8.00-18.00")

# CBG residenziali
# quelli che hanno PC2>0

cbg_work<-which(pca_W.1$scores[,2]>0)
stops_cbg_work<-stops[,cbg_work]

x11()
matplot(stops_cbg_work,type='l',main = "More frequented CBG during 8.00-18.00 (22/06 - 28/06)")
abline(v=seq(1,168, by=24), lty=2)

x11()
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry) +
  geom_sf(data = CBG_ny_no_river$geometry[cbg_work], fill="red") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue") +ggtitle("CBG more frequented during 8.00-18.00")



# CBG più frequentati di sera rispetto alla media 
# quelli che hanno PC3>0

cbg_work<-which(pca_W.1$scores[,2]>=0)
stops_cbg_work<-stops[,cbg_work]

x11()
matplot(stops_cbg_work,type='l',main = "More frequented CBG during 21.00 - 24.00 (22/06 - 28/06)")
abline(v=seq(1,168, by=24), lty=2)

x11()
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry) +
  geom_sf(data = CBG_ny_no_river$geometry[cbg_work], fill="red") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue") +ggtitle("CBG more frequented during 21.00-24.00")



