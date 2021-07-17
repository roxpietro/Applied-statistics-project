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

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/River_Dataset.RData")



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

x11() #controlla se plotta i centroidi
plot(coord.x_long,coord.y_lat,xlab="longitude",ylab="latitude",lwd=2)
text(coord.x_long,coord.y_lat, labels=CBG_ny_no_river$CensusBlockGroup, cex=1)


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


# dist=distm(cbind(coord.x_long, coord.y_lat), cbind(coord_riv.x_long, coord_riv.y_lat), fun = distGeo)
distance<-c()
#SCELTA REGRESSORE:f(s_i)
#f(s_i) = distanza dal fiume
# for (i in 1:1092)
#   distance[i]<-dist[i,which.min(dist[i,])]
# 

attach(New_York_County_no_river)
attach(CBG_ny_no_river)

#f(s_i) = distanza da ipotetico centro di times square
k <- which(CBG_ny_no_river$TractCode == '011300')
centroid_TimesSquare <- st_centroid(CBG_ny_no_river$geometry[k,], of_largest_polygon = FALSE)
coord_cTS <- as.numeric(unlist(centroid_TimesSquare))
coord_cTS.x_long <- coord_cTS[1]
coord_cTS.y_lat <- coord_cTS[2]
distance<-distm(cbind(coord.x_long, coord.y_lat), cbind(coord_cTS.x_long, coord_cTS.y_lat), fun = distGeo)


area_cbg=matrix(nrow = 1, ncol = 1092)
#par(mfrow=c(4,5))
for (i in 1:1092) {
  #points=census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2]
  coords=CBG_ny_no_river$geometry[[i]][[1]][[1]]
  area_cbg[i]=areaPolygon(coords)
}

area_cbg<-area_cbg/10^6

data_spatial <-data.frame(coord.x,coord.y, median_dwell, distance)
coordinates(data_spatial)<-c('coord.x', 'coord.y')

# histogram of med variable
hist(median_dwell, breaks=16, col="grey", main='Histogram of median dwell', prob = TRUE, xlab = 'median dwell') #asymmetric data
# highly skewed, transform to the log
hist(log(median_dwell), breaks=16, col="grey", main='Histogram of log(med)', prob = TRUE, xlab = 'log(med)')

rem <- which(median_dwell > 350)
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-rem], aes(fill=med[-rem]))+scale_fill_gradient(low="lightyellow", high="red") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue")+
  geom_sf(data = CBG_ny_no_river$geometry[which(CBG_ny_no_river$TractCode=="011300"),], fill="blue")

x11()
spplot(data_spatial,'median_dwell')

xyplot(log(median_dwell) ~ sqrt(distance), as.data.frame(data_spatial))

# vediamo una netta differenza tra nord e sud -> dummy variable tra nord e sud

# sud <- c(which(CBG_ny_no_river$TractCode<="013900"), 
#         which(CBG_ny_no_river$TractCode=='031900'),
#         which(CBG_ny_no_river$TractCode=='031704'),
#         which(CBG_ny_no_river$TractCode=='031703'),
#         which(CBG_ny_no_river$TractCode=='031704'))
# # x11()
# plot(st_geometry(CBG_ny_no_river$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
# par(new=T)
# plot(st_geometry(CBG_ny_no_river$geometry[sud,]), col="red",xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")


#  DUMMY <- rep(0,1092)
#  DUMMY[sud] <- 1
#  data <- data.frame(coord.x, coord.y, med,DUMMY, distance)
#  
#  detach(New_York_County)
#  
#  coordinates(data) <- c('coord.x','coord.y')
# 
#  
# #@-------------------------------
#  # stationary variogram v1
#  
#  v.1 <- variogram(log(med) ~ 1, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
#  fit1=fit.variogram(v.1, vgm(0.4, model='Exp', 5000, nugget=0.2))
#  
#  x11()
#  plot(v.1, fit1, pch = 3)
# 
# #--------------------------------
#  # non stationary variogram v
#  
#  v <- variogram(log(med) ~ DUMMY + sqrt(distance) + sqrt(distance)*DUMMY, data = data_spatial)
#  plot(v)
#  
#  # non converge, dato che c'? poca variabilit? tra i dati
#  v <- variogram(log(med) ~ DUMMY + sqrt(distance) + sqrt(distance)*DUMMY, data = data,boundaries = c(0,200,seq(400,6000,450)))
#  plot(v)
#  v.fit <- fit.variogram(v, vgm(0.4, "Exp", 3000, 0.2))
#  x11()
#  plot(v, v.fit, pch = 3)
#  v.fit
#  
#  
#----------------------------------------------
 #TROVARE I COEFFICIENTI
 
 # sud
 g.tr <- gstat(formula = log(med) ~ DUMMY + distance + distance*DUMMY, data = data_spatial, model = v.fit)
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
 g.tr <- gstat(formula = log(med) ~ DUMMY + distance + distance*DUMMY, data = data_spatial, model = v.fit)
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
 
 g.tr <- gstat(formula = log(med) ~ DUMMY + distance + distance*DUMMY, data = data_spatial, model = v.fit)
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
 
 
 
 #----------------------------------------------------
 # rimuovo i picchi
 med_dwell <- med[-rem]
 dist <- distance[-rem]
 data_spat <- data.frame(coord.x[-rem],coord.y[-rem], med_dwell, dist)
 names(data_spat) <- c('x','y','dist home', 'distance')
 coordinates(data_spat)<-c('x', 'y')
 
 x11()
 xyplot(log(med_dwell) ~ sqrt(distance), as.data.frame(data_spat))
 
 library(car)
 lambda <- powerTransform(med_dwell)
 bc.med_dwell <- bcPower(med_dwell, lambda$lambda)
 
 hist(bc.med_dwell, breaks=16, col="grey", main='Histogram of bc dist', prob = TRUE, xlab = 'bc dist from home')
 
 x11()
 xyplot(bc.med_dwell ~ sqrt(dist), as.data.frame(data_spat))
 
 # stationary variogram v1
 
 v.1 <- variogram(log(med_dwell) ~ 1, data = data_spat,boundaries = c(0,seq(10,3000,300)))
 fit1=fit.variogram(v.1, vgm(0.4, model='Exp', 5000, nugget=0.2))
 fit1
 
 x11()
 plot(v.1, fit1, pch = 3)
 

 # non stationary variogram v
 
 v <- variogram(log(med_dwell) ~ sqrt(dist), data = data_spat)
 plot(v)
 
 # non converge, dato che c'? poca variabilit? tra i dati
 v <- variogram(log(med_dwell) ~ sqrt(dist) , data = data_spat,boundaries = c(0,seq(10,3000,300)))
 plot(v)
 v.fit <- fit.variogram(v, vgm(0.4, "Exp", 3000, 0.2))
 x11()
 plot(v, v.fit, pch = 3)
 v.fit
 
 
 # stationary variogram v1
 
 v.1 <- variogram(bc.med_dwell ~ 1, data = data_spat,boundaries = c(0,seq(10,3000,300)))
 fit1=fit.variogram(v.1, vgm(6, model='Exp', 5000, nugget=2))
 fit1
 
 x11()
 plot(v.1, fit1, pch = 3)
 
 
 # non stationary variogram v
 
 v <- variogram(bc.med_dwell ~ sqrt(dist), data = data_spat)
 plot(v)
 
 # non converge, dato che c'? poca variabilit? tra i dati
 v <- variogram(bc.med_dwell ~ sqrt(dist) , data = data_spat,boundaries = c(0,seq(10,3000,300)))
 plot(v)
 v.fit <- fit.variogram(v, vgm(6, "Exp", 3000, 2))
 x11()
 plot(v, v.fit, pch = 3)
 v.fit
 
 # compare the two variograms
 v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}
 
 x11()
 plot(v$dist,v$gamma,xlab='distance',ylab='semivariance',pch=19,col='red', ylim=c(5.5, 7))
 curve(v.f.est(x, C0=v.fit[2,2]+v.fit[1,2], cov.pars=rbind(c(v.fit[2,2], v.fit[2,3]),c(v.fit[1,2], v.fit[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 3000,
       xlab = "distance", ylab = expression(gamma(h)),
       main = "Variogram model",add=TRUE,col='red',lwd=2)
 
 points(v.1$dist,v.1$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue')
 curve(v.f.est(x, C0=fit1[2,2]+fit1[1,2], 
               cov.pars=rbind(c(fit1[2,2], fit1[2,3]),c(fit1[1,2], fit1[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 3000,
       xlab = "distance", ylab = expression(gamma(h)),
       main = "Variogram model",add=TRUE,col='steelblue',lwd=2)
 