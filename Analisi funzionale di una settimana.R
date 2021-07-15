########################################################
#### FUNCTIONAL ANALYSIS - ONE WEEK (22/06 - 28/06) ####
########################################################

library(fda)

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
#--------------------------------------------------------------------
# Build dataset 

New_York_County_no_river= New_York_County_no_river[order( New_York_County_no_river$area),]
attach( New_York_County_no_river)

stops<-matrix(nrow = 1092, ncol=30)

for (i in 1:dim( New_York_County_no_river)[1]) {
  stops[i,]<-stops_by_day[[i]]
}

detach( New_York_County_no_river)

stops<-stops[,22:28]

stops<-t(stops)
colnames(stops)<- New_York_County_no_river$area
x11()
matplot(stops,type='l')

#B-SPLINES
# Set parameters
nbasis <- 5
m <- 2+1        # spline order 
# spline degree  #DEGREE = ORDER - 1
basis <- create.bspline.basis(rangeval=c(1,7), nbasis=nbasis, norder=m)


time=1:7
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

#---------------------------------------------------------------------
# proviamo a fare clustering, magari troviamo i due comportamenti che vediamo
# ALIGNMENT

x<-t(matrix((time)))
Xsp <- smooth.basis(argvals=time, y=stops, fdParobj=basis) # easier because it includes also penalization (see later)
Xsp0 <- eval.fd(time, Xsp$fd) #  the curve smoothing the data
Xsp1 <- eval.fd(time, Xsp$fd, Lfd=1) # first derivative


x11()
matplot(stops,type='l')
lines(stops[,381],lwd=4, col=2) 
lines(stops[,100],lwd=4, col=1) 

library(fdakma)

fdakma_example <- kma(
  x=x, 
  y0=t(Xsp0),
  #y1=t(Xsp1), 
  n.clust = 2, 
  warping.method = 'affine', # trasformation of an axis in order to do align
  similarity.method = 'd0.pearson',  # similarity computed as the cosine
  # between the first derivatives 
  # (correlation)
  center.method = 'k-medoids',
  seeds = c(100,381) # you can give a little help to the algorithm...
)

kma.show.results(fdakma_example)

