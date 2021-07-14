###################################
### SPATIAL ANALYSIS WITH RIVER ###
###################################

library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(ggplot2)
library(raster)
library(rgdal)


# fra
# load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA
# load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
# load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/River_Dataset.RData")
# terri
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Data frame county/New York County.RData") #TERRI
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")

# order patterns_ny and census_block_ny by CBG of New York County
New_York_County=New_York_County[order(New_York_County$area),]
CBG_ny_index = which(census_blocks_ny$County=="New York County")
CBG_ny = census_blocks_ny[CBG_ny_index,]


# make the two datasets equal
remove=c()
k=1
for (i in 1:1170) {
  index=which(New_York_County$area==CBG_ny$CensusBlockGroup[i])
  if (length(index)==0) {
    remove[k]=i
    k=k+1
  }
}

CBG_ny=CBG_ny[-remove,]
CBG_ny=CBG_ny[order(CBG_ny$CensusBlockGroup),]

CBG_ny_index=CBG_ny_index[-remove]

rm (patterns_ny)
rm (census_blocks_ny)
rm(census_metadata)

#------------------------------------------------------------
# TROVIAMO I CBG DEI FIUMI

track_code_river <- c("027500", "025500", "024700", "024100", "023700", "023300", "022900", "022500", "022301", "021900","021100","020500", "019900", "019500",
                      "019100", "018700", "018300", "017900", "017500", "017100","016700", "016300", "015900", "015500", "015100", "013500", "012900", "011700", "009900", "007900", "007500", "006900", "003700",
                      "003900", "031703", "031704","031900", "000500", "000900","000700", "001502", "001501", "002500", "000800","000600","000201", "000202", "001001", "001002", "002000", "002400", "004400",
                      "006000", "006200", "008601", "008602", "023801", "010601","010602","011600", "012400", "013200", "013600", "023802", "015200","017800", "019200", "024200", "021000", "023600", "024302", "031100",
                      "029900", "029700", "028700", "022302", "016200", "024000")

index_river=c()
k=1
for (i in 1:dim(CBG_ny)[1]) {
  if (CBG_ny$TractCode[i] %in% track_code_river ) {
    if (CBG_ny$BlockGroup[i] == "0") {
      index_river[k] = i
      k=k+1
    }
  }
}
CBG_ny_no_river<-CBG_ny[-index_river,]
#save (CBG_ny_no_river, file="CBG_NY_no_river.RData")


#-----------------------------------------------------------
# coordinate in utm

# x11()
# plot(st_geometry(CBG_ny_no_river$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
# par(new=T)
# plot(st_geometry(CBG_RIVER$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = "lightblue")
# par(new=T)
# plot(centroids, xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ", pch='.')

#
centroids_NY <- st_centroid(CBG_ny_no_river$geometry, of_largest_polygon = FALSE)
coord_NY <- as.numeric(unlist(centroids_NY))
coord.x_long <- coord_NY[seq(1,length(coord_NY),by=2)]
coord.y_lat <- coord_NY[seq(2,length(coord_NY),by=2)]

# x11()
# plot(coord.x_long,coord.y_lat,xlab="longitude",ylab="latitude",lwd=2)
# text(coord.x_long,coord.y_lat, labels=CBG_ny_no_river$CensusBlockGroup, cex=1)


coord<-SpatialPoints(cbind(coord.x_long,coord.y_lat),proj4string=CRS("+proj=longlat"))
coord.UTM.NY <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))

coord.x <- coord.UTM.NY@coords[,1]
coord.y <- coord.UTM.NY@coords[,2]



centroids_river <- st_centroid(CBG_RIVER$geometry, of_largest_polygon = FALSE)
coord_river <- as.numeric(unlist(centroids_river))
coord_riv.x_long <- coord_river[seq(1,length(coord_river),by=2)]
coord_riv.y_lat <- coord_river[seq(2,length(coord_river),by=2)]

coord<-SpatialPoints(cbind(coord_riv.x_long,coord_riv.y_lat),proj4string=CRS("+proj=longlat"))
coord.UTM.riv <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))

coord_riv.x <- coord.UTM.riv@coords[,1]
coord_riv.y <- coord.UTM.riv@coords[,2]

k <- which(CBG_ny_no_river$TractCode == '011300')
centroid_TimesSquare <- st_centroid(CBG_ny_no_river$geometry[k,], of_largest_polygon = FALSE)
coord_cTS <- as.numeric(unlist(centroid_TimesSquare))
coord_cTS.x_long <- coord_cTS[1]
coord_cTS.y_lat <- coord_cTS[2]

dist=distm(cbind(coord.x_long, coord.y_lat), cbind(coord_riv.x_long, coord_riv.y_lat), fun = distGeo)
#SCELTA REGRESSORE:f(s_i)
#f(s_i) = distanza dal fiume
distance<-c()
for (i in 1:1092)
  distance[i]<-dist[i,which.min(dist[i,])]

#f(s_i) = distanza da ipotetico centro di times square
distance<-distm(cbind(coord.x_long, coord.y_lat), cbind(coord_cTS.x_long, coord_cTS.y_lat), fun = distGeo)

New_York_County<-New_York_County[-index_river,]
attach(New_York_County)


data_spatial <-data.frame(coord.x,coord.y, median_dwell, distance)
coordinates(data_spatial)<-c('coord.x', 'coord.y')

#box cox transformation --------------------------------------------------------
library(car)
lambda <- powerTransform(median_dwell)
bc.median_dwell <- bcPower(median_dwell, lambda$lambda)

# histogram of median_dwell variable
hist(median_dwell, breaks=16, col="grey", main='Histogram of median dwell', prob = TRUE, xlab = 'median dwell') #asymmetric data
# highly skewed, transform to the log
hist(log(median_dwell), breaks=16, col="grey", main='Histogram of log(median_dwell)', prob = TRUE, xlab = 'log(median_dwell)')

hist(bc.median_dwell, breaks=16, col="grey", main='Histogram of log(median_dwell)', prob = TRUE, xlab = 'log(median_dwell)')

x11()
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, aes(fill=median_dwell))+scale_fill_gradient(low="lightyellow", high="black") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue")


x11()
spplot(data_spatial,'median_dwell')
#serve per vedere se si nota correlazione, dipendenza, tra funzione della posizione scelta e Z(median_dwell)
xyplot(bc.median_dwell ~ sqrt(distance), as.data.frame(data_spatial))

# vediamo una netta differenza tra nord e sud -> dummy variable tra nord e sud

sud <- c(which(CBG_ny_no_river$TractCode<="013900"), 
         which(CBG_ny_no_river$TractCode=='031900'),
         which(CBG_ny_no_river$TractCode=='031704'),
         which(CBG_ny_no_river$TractCode=='031703'),
         which(CBG_ny_no_river$TractCode=='031704'))
# x11()
# plot(st_geometry(CBG_ny_no_river$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
# par(new=T)
# plot(st_geometry(CBG_ny_no_river$geometry[sud,]), col="red",xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")


DUMMY <- rep(0,1092)
DUMMY[sud] <- 1
data <- data.frame(coord.x, coord.y, median_dwell,DUMMY, distance)

detach(New_York_County)

coordinates(data) <- c('coord.x','coord.y')


#@-------------------------------
# stationary variogram

v.1 <- variogram(log(median_dwell) ~ 1, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
fit1=fit.variogram(v.1, vgm(0.4, model='Exp', 5000, nugget=0.2))

x11()
plot(v.1, fit1, pch = 3)

#--------------------------------
# non stationary variogram

v <- variogram(log(median_dwell) ~ DUMMY + distance + distance*DUMMY, data = data_spatial)
plot(v)

# non converge, dato che c'? poca variabilit? tra i dati
v <- variogram(log(median_dwell) ~ DUMMY + distance + distance*DUMMY, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
plot(v)
v.fit <- fit.variogram(v, vgm(0.4, "Exp", 3000, 0.2))
x11()
plot(v, v.fit, pch = 3)
v.fit


#--------------------------------------------
# compare the two variograms
v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}

plot(v$dist,v$gamma,xlab='distance',ylab='semivariance',pch=19,col='red', ylim=c(0.2,0.39))
curve(v.f.est(x, C0=v.fit[2,2]+v.fit[1,2], cov.pars=rbind(c(v.fit[2,2], v.fit[2,3]),c(v.fit[1,2], v.fit[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 6000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='red',lwd=2)

points(v.1$dist,v.1$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue')
curve(v.f.est(x, C0=fit1[2,2]+fit1[1,2], 
              cov.pars=rbind(c(fit1[2,2], fit1[2,3]),c(fit1[1,2], fit1[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 6000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='steelblue',lwd=2)

# scegliamo il modello non stazionario!

#----------------------------------------------
#TROVARE I COEFFICIENTI

# sud
g.tr <- gstat(formula = log(median_dwell) ~ DUMMY + distance + distance*DUMMY, data = data_spatial, model = v.fit)
pnull=0;
pone=1;
snull.new=data.frame(coord.x=514703.8, coord.y=5035569.3, DUMMY=1) # VAN BENE COORDINATE QUALSIASI
snull.new <- as.data.frame(c(snull.new,pnull))
names(snull.new) <- c('coord.x','coord.y', 'DUMMY','distance') #CAMBIARE x, y, population CON LE VARIABILI GIUSTE
coordinates(snull.new) <- c('coord.x','coord.y')

sone.new=data.frame(coord.x=514703.8, coord.y=5035569.3, DUMMY=1) # UTM coordinates
sone.new <- as.data.frame(c(sone.new,pone))
names(sone.new) <- c('coord.x','coord.y', 'DUMMY','distance')  #CAMBIARE x, y, population CON LE VARIABILI GIUSTE
coordinates(sone.new) <- c('coord.x','coord.y')


#COEFF a0 E a1
a0=predict(g.tr, snull.new, BLUE = TRUE)$var1.pred
a0
a1=predict(g.tr, sone.new, BLUE = TRUE)$var1.pred-a0
a1

# nord
g.tr <- gstat(formula = log(median_dwell) ~ DUMMY + distance + distance*DUMMY, data = data_spatial, model = v.fit)
pnull=0;
pone=1;
snull.new=data.frame(coord.x=514703.8, coord.y=5035569.3, DUMMY=0) # VAN BENE COORDINATE QUALSIASI
snull.new <- as.data.frame(c(snull.new,pnull))
names(snull.new) <- c('coord.x','coord.y', 'DUMMY','distance') 
coordinates(snull.new) <- c('coord.x','coord.y')

sone.new=data.frame(coord.x=514703.8, coord.y=5035569.3, DUMMY=0) # UTM coordinates
sone.new <- as.data.frame(c(sone.new,pone))
names(sone.new) <- c('coord.x','coord.y', 'DUMMY','distance')  
coordinates(sone.new) <- c('coord.x','coord.y')


#COEFF a0 E a1
a0=predict(g.tr, snull.new, BLUE = TRUE)$var1.pred
a0
a1=predict(g.tr, sone.new, BLUE = TRUE)$var1.pred-a0
a1

#-----------------------------------------------------------------------
# spatial prediction

g.tr <- gstat(formula = log(median_dwell) ~ DUMMY + distance + distance*DUMMY, data = data_spatial, model = v.fit)
# inserire le coordinate (longitudine, latitudine) che vogliamo
x=-74.3
y=40.5

coord<-SpatialPoints(cbind(x,y),proj4string=CRS("+proj=longlat"))
coord.UTM <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))

stop.new=data.frame(coord.x=coord.UTM@coords[,1], y=coord.UTM@coords[,2]) # UTM coordinates
s0.dist <- min(rowSums(scale(t(rbind(coord.UTM.NY@coords[,1],coord.UTM.NY@coords[,1])),stop.new)^2))
stop.new <- as.data.frame(c(stop.new,s0.dist,1)) #capire se dummy=1 o 0
names(stop.new) <- c('coord.x','coord.y','distance', 'DUMMY') 
coordinates(stop.new) <- c('coord.x','coord.y')

predict(g.tr, stop.new, BLUE = FALSE)
