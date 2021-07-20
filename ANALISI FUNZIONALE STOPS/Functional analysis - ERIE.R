##################################################################
#### FUNCTIONAL ANALYSIS - ONE WEEK (22/06 - 28/06) - BUFFALO ####
##################################################################

library(fda)

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
rm(patterns_ny)
rm(census_metadata)
Erie_cbg <- census_blocks_ny[which(census_blocks_ny$County=="Erie County"),]
rm(census_blocks_ny)

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/Erie County.RData")

Erie<-Erie[order(Erie$area),]

Erie_cbg<-Erie_cbg[order(Erie_cbg$CensusBlockGroup),]

river_index<-which(Erie_cbg$BlockGroup=="0")
RIVER <- Erie_cbg[river_index,]
Erie<-Erie[-river_index,]
Erie_cbg<-Erie_cbg[-river_index,]


#-------------------------------------------------------------------------------------------
# Build dataset 

attach( Erie)

stops<-matrix(nrow = dim( Erie)[1], ncol=30)

for (i in 1:dim( Erie)[1]) {
  stops[i,]<-stops_by_day[[i]]
}

detach( Erie)

stops<-t(stops)
colnames(stops)<- Erie$area
x11()
matplot(stops,type='l')

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
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=Erie$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)


# outliers
out=c(92,532)
cbg_out=Erie[out,1]
layout(1)
x11()
matplot(stops,type='l')
lines(stops[,92],lwd=4, col=2) 
lines(stops[,532],lwd=4, col=1) 


# togliamo questi outliers
stops<-stops[,-out]
matplot(stops,type='l')


#B-SPLINES
# Set parameters
nbasis <- 4
m <- 1+1        # spline order 
# spline degree  #DEGREE = ORDER - 1
basis <- create.bspline.basis(rangeval=c(1,7), nbasis=nbasis, norder=m)


time=1:7
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
x11()
plot.fd(data_W.fd.1)


# FPCA

arm=4 #numero armoniche
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
media <- mean.fd(data_W.fd.1)

plot(media,lwd=2,ylim=c(-2,260),ylab='temperature',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)


plot(media,lwd=2,ylim=c(0,150),lab='temperature',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)

# scatter plot of the scores
par(mfrow=c(1,2))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)

plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=Erie$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)
