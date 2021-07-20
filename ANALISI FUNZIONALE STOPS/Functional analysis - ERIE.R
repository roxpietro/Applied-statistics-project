##################################################################
#### FUNCTIONAL ANALYSIS - ONE WEEK (22/06 - 28/06) - BUFFALO ####
##################################################################

library(fda)
library(sf)
library(ggplot2)
library(geosphere)

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

stops<-matrix(nrow = dim( Erie)[1], ncol=30*24)

for (i in 1:dim( Erie)[1]) {
  coords=Erie_cbg$geometry[[i]][[1]][[1]]
  area_cbg=areaPolygon(coords)/10^6
  stops[i,]<-stops_by_each_hour[[i]]/area_cbg
}

detach( Erie)

stops<-stops[,504:671]
stops<-t(stops)
colnames(stops)<- Erie$area
x11()
matplot(stops,type='l', main = "Stops by day - Erie County (22/06 - 28/06)")
abline(v=seq(1,168, by=24), lty=2)

track_code <- c("005801", "005802", "005700", "005900", "017100", "006100",
                "007000", "007202", "005600","005000", "005100", "004500", "004601",
                "004602", "004300", "004700", "004800", "004900", "005500", "005400",
                "005201", "004001", "004200", "004300", "004401",
                "003901", "005300", "004402", "004100", "017000",
                "005202", "006302", "006301", "006501", "016900",
                "016800", "003301", "003400", "003600", "003700",
                "003800", "006901", "006601", "006602", "006701", 
                "006901","006902", "007202", "007101", "007102",
                "016500", "002502", "001402", "016400", "000500",
                "000110", "000600", "000700", "001000", "000800",
                "000200", "000900", "001100", "001900", "016700",
                "016300", "001700","001500", "002400", "003000",
                "002702", "002800", "003000", "002900", "003500",
                "003100", "006800", "003302", "016800", "006602",
                "006601","005202", "002300","002400", "001600","001500",
                "016400", "016600", "006201","006702")

buffalo_index<- which(Erie_cbg$TractCode %in% track_code)

x11()
ggplot() +
   geom_sf(data = Erie_cbg$geometry[buffalo_index],fill="darkgoldenrod1" ) +
   geom_sf(data = Erie_cbg$geometry[-buffalo_index], fill="cornsilk" ) +
   geom_sf(data = RIVER$geometry, fill = "lightblue") +ggtitle("Erie County")

buffalo_stops<-stops[,buffalo_index]
country_stops<-stops[,-buffalo_index]

x11()
matplot(buffalo_stops,type='l', main = "Stops by each hour - Buffalo City (22/06 - 28/06)")
abline(v=seq(1,168, by=24), lty=2)

x11()
matplot(country_stops,type='l', main = "Stops by each hour - Country (22/06 - 28/06)")
abline(v=seq(1,168, by=24), lty=2)


nbasis <- 30 # number of basis

# Create the basis
#FOURIER
basis <- create.bspline.basis(rangeval=c(1,168),nbasis=nbasis, norder=4) # creates a fourier basis

time=1:168
data_W.fd.1 <- Data2fd(y = buffalo_stops,argvals = time,basisobj = basis) #SMOOTHING
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
par(mfrow=c(2,1))
media <- mean.fd(data_W.fd.1)

plot(media,lwd=2,ylim=c(0,61),ylab='Stops by each hour',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
abline(v=seq(1,168, by=24), lty=2)
title('FPC1')

plot(media,lwd=2,ylim=c(-10,40),lab='Stops by each hour',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
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
out=c(141,160, 166)
cbg_out=Erie[out,1]
layout(1)
x11()
matplot(stops,type='l')
lines(stops[,141],lwd=4, col=2) 
lines(stops[,160],lwd=4, col=1) 
lines(stops[,166],lwd=4, col=4) 

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
plot(media,lwd=2,ylim=c(0,65),ylab='Stops by each hour ~ 22/06 - 28/06',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
abline(v=seq(1,168, by=24), lty=2, lwd=3)
abline(v=seq(1,168, by=6), lty=3)
title("FPC1 - 71.3% of variability")

plot(media,lwd=2,ylim=c(-10,45),ylab='Stops by each hour ~ 22/06 - 28/06',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
abline(v=seq(1,168, by=24), lty=2, lwd=3)
abline(v=seq(1,168, by=6), lty=3)
title("FPC2 - 9.4% of variability")

plot(media,lwd=2,ylim=c(-5,50),ylab='Stops by each hour ~ 22/06 - 28/06',main='FPC2')
lines(media+pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=3)
lines(media-pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=2)
abline(v=seq(1,168, by=24), lty=2, lwd=3)
abline(v=seq(1,168, by=6), lty=3)
title("FPC3 - 7.3% of variability")

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