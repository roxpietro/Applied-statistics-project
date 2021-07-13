#####################################
#### CLUSTER WEEKEND AND WEEKDAY ####
#####################################


library(fda)

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
#--------------------------------------------------------------------
# Build dataset 

attach(New_York_County)

stops<-matrix(nrow = 1168, ncol=30)

for (i in 1:dim(New_York_County)[1]) {
  stops[i,]<-stops_by_day[[i]]
}

detach(New_York_County)

stops<-t(stops)
colnames(stops)<-New_York_County$area
matplot(stops,type='l')


#B-SPLINES
# Set parameters
nbasis <- 13
m <- 3+1        # spline order 
# spline degree  #DEGREE = ORDER - 1
basis <- create.bspline.basis(rangeval=c(1,30), nbasis=nbasis, norder=m)


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
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=New_York_County$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)


# outliers
out=c(228,871,267)
cbg_out=New_York_County[out,1]
layout(1)
matplot(stops,type='l')
lines(stops[,228],lwd=4, col=2) 
lines(stops[,871],lwd=4, col=1) 
lines(stops[,267],lwd=4, col=3) 

# togliamo questi outliers
stops<-stops[,-out]
matplot(stops,type='l')

# dal matplot vediamo che ci sono due comportamenti diversi, proviamo a togliere il fiume..
#...non cambia molto, significa che dobbiamo capire la discriminante

New_York_County_no_river<-New_York_County_no_river[-which(New_York_County_no_river$area %in% cbg_out),]
attach(New_York_County_no_river)

stops<-matrix(nrow = 1090, ncol=30)

for (i in 1:1090) {
    stops[i,]<-stops_by_day[[i]]
}

detach(New_York_County_no_river)

stops<-t(stops)
matplot(stops,type='l')


nbasis <- 29
basis <- create.fourier.basis(rangeval=c(1,30),nbasis=nbasis) # creates a fourier basis

time=1:30
data_W.fd.1 <- Data2fd(y = stops,argvals = time,basisobj = basis) #SMOOTHING
plot.fd(data_W.fd.1, ylim = c(0,2000))



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

# scatter plot of the scores
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
text(pca_W.1$scores[,1],pca_W.1$scores[,2], labels=New_York_County$area, cex=1)
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2")
text(pca_W.1$scores[,1],pca_W.1$scores[,2], cex=1)
