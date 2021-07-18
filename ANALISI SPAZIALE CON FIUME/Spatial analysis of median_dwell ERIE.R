#### spatial correlation median dwell

library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(ggplot2)
library(raster)
library(rgdal)

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
i=15
census_blocks_ny=census_blocks_ny[order(census_blocks_ny$CensusBlockGroup),]
print(unique(census_blocks_ny$County)[i])
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/Erie County.RData")
sub_patt<-Erie
sub_patt=sub_patt[order(sub_patt$area),]

geo<-census_blocks_ny[which(census_blocks_ny$County==unique(census_blocks_ny$County)[i]),]
geo=geo[order(geo$CensusBlockGroup),]

rm(Erie)
rm(census_blocks_ny)
rm(census_metadata)
rm(patterns_ny)

centroids_NY <- st_centroid(geo$geometry, of_largest_polygon = FALSE)
coord_NY <- as.numeric(unlist(centroids_NY))
coord.x_long <- coord_NY[seq(1,length(coord_NY),by=2)]
coord.y_lat <- coord_NY[seq(2,length(coord_NY),by=2)]

# x11() #controlla se plotta i centroidi
# plot(coord.x_long,coord.y_lat,xlab="longitude",ylab="latitude",lwd=2)

coord<-SpatialPoints(cbind(coord.x_long,coord.y_lat),proj4string=CRS("+proj=longlat"))
coord.UTM.NY <- spTransform(coord, CRS("+proj=utm +zone=17+datum=WGS84"))

coord.x <- coord.UTM.NY@coords[,1]
coord.y <- coord.UTM.NY@coords[,2]

CBG_RIVER<-geo[which(geo$BlockGroup==0),]

# centroids_river <- st_centroid(CBG_RIVER$geometry, of_largest_polygon = FALSE)
# coord_river <- as.numeric(unlist(centroids_river))
# coord_riv.x_long <- coord_river[seq(1,length(coord_river),by=2)]
# coord_riv.y_lat <- coord_river[seq(2,length(coord_river),by=2)]
# 
# coord<-SpatialPoints(cbind(coord_riv.x_long,coord_riv.y_lat),proj4string=CRS("+proj=longlat"))
# coord.UTM.riv <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))
# 
# coord_riv.x <- coord.UTM.riv@coords[,1]
# coord_riv.y <- coord.UTM.riv@coords[,2]


# dist=distm(cbind(coord.x_long, coord.y_lat), cbind(coord_riv.x_long, coord_riv.y_lat), fun = distGeo)
distance<-c()
#SCELTA REGRESSORE:f(s_i)
#f(s_i) = distanza dal fiume
# for (i in 1:1092)
#   distance[i]<-dist[i,which.min(dist[i,])]
# 

attach(sub_patt)
attach(geo)

#f(s_i) = distanza da ipotetico centro di buffalo
k <- which(geo$TractCode == '016500')
centroid_TimesSquare <- st_centroid(geo$geometry[k,], of_largest_polygon = FALSE)
coord_cTS <- as.numeric(unlist(centroid_TimesSquare))
coord_cbuffalo.x_long <- coord_cTS[1]
coord_cbuffalo.y_lat <- coord_cTS[2]
distance<-distm(cbind(coord.x_long, coord.y_lat), cbind(coord_cbuffalo.x_long, coord_cbuffalo.y_lat), fun = distGeo)


area_cbg=matrix(nrow = dim(sub_patt)[1], ncol = 1)
#par(mfrow=c(4,5))
for (i in 1:dim(sub_patt)[1]) {
  #points=census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2]
  coords=geo$geometry[[i]][[1]][[1]]
  area_cbg[i]=areaPolygon(coords)
}

area_cbg<-area_cbg/10^6


#-------------------------------------------
# SI DENSITY

data_spatial <-data.frame(coord.x,coord.y, median_dwell)
coordinates(data_spatial)<-c('coord.x', 'coord.y')

# histogram of med variable
hist(median_dwell, breaks=16, col="grey", main='Histogram of median dwell', prob = TRUE, xlab = 'median dwell') #asymmetric data
# highly skewed, transform to the log
hist(log(median_dwell), breaks=16, col="grey", main='Histogram of log(med)', prob = TRUE, xlab = 'log(med)')

med_dwell_area=median_dwell[-which(geo$BlockGroup==0)]/area_cbg[-which(geo$BlockGroup==0)]

x11()
ggplot() + 
  geom_sf(data = geo$geometry[-which(geo$BlockGroup==0)], aes(fill=log(med_dwell_area)))+scale_fill_gradient(low="lightyellow", high="red") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue") +ggtitle("Erie County")


hist(log(med_dwell_area), breaks=16, col="grey", main='Histogram of median dwell', prob = TRUE, xlab = 'median dwell') #asymmetric data

x11()
xyplot(log(med_dwell_area) ~ sqrt(distance), as.data.frame(data_spatial))


coord.x<-coord.x[-which(geo$BlockGroup==0)]
coord.y<-coord.y[-which(geo$BlockGroup==0)]

dist<-distance[-which(geo$BlockGroup==0)]

#------------------------------------------------
# non stationary variogram

# LOG(median_dwell/area)
data_spatial <-data.frame(coord.x,coord.y, med_dwell_area, dist)
coordinates(data_spatial)<-c('coord.x', 'coord.y')


v.log <- variogram(log(med_dwell_area) ~ sqrt(dist), data = data_spatial,cutoff=20000)
plot(v.log)

fit_log=fit.variogram(v.log, vgm(1.5, model='Exp', 15000, nugget=1))
fit_log

x11()
plot(v.log, fit_log, pch = 3)



# bc->median_dwell/area

library(car)
lambda <- powerTransform(med_dwell_area)
bc.med_dwell <- bcPower(med_dwell_area, lambda$lambda)

x11()
ggplot() + 
  geom_sf(data = geo$geometry[-which(geo$BlockGroup==0)], aes(fill=bc.med_dwell))+scale_fill_gradient(low="lightyellow", high="red") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue") +ggtitle("Erie County")


hist(bc.med_dwell, breaks=16, col="grey", main='Histogram of bc dist', prob = TRUE, xlab = 'bc dist from home')

x11()
xyplot(bc.med_dwell ~ sqrt(distance), as.data.frame(data_spatial))

data_spatial <-data.frame(coord.x,coord.y, bc.med_dwell, dist)
coordinates(data_spatial)<-c('coord.x', 'coord.y')


v.bc <- variogram(bc.med_dwell ~ sqrt(dist), data = data_spatial, cutoff=20000)
plot(v.bc)

fit.bc=fit.variogram(v.bc, vgm(10, model='Exp', 15000, nugget=4))
fit.bc

x11()
plot(v.bc, fit.bc, pch = 3)


#----------------------------------------------------------------------------------
# try anisotropy - log

v.dir <- variogram(log(med_dwell_area) ~ sqrt(dist),data_spatial,alpha=(0:3)*45) 
plot(v.dir)
v.anis <- vgm(1.3, "Exp", 15000, 0.5, anis=c(45, 0.3))

print(plot(v.dir, v.anis, pch=19))

# volendo 90 gradi è abbastanza fittato però oscilla un po' quindi non saprei
v.dir <- variogram(log(med_dwell_area) ~ sqrt(dist),data_spatial,alpha=90) 
plot(v.dir)
v.anis <- fit.variogram(v.dir,vgm(0.8, "Exp", 15000, 0.7, alpha=90))

x11()
plot(v.dir, v.anis, pch = 3)
v.anis

# compare the two variograms
v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}

x11()
plot(v.log$dist,v.log$gamma,xlab='distance',ylab='semivariance',pch=19,col='red')
curve(v.f.est(x, C0=fit_log[2,2]+fit_log[1,2], cov.pars=rbind(c(fit_log[2,2], fit_log[2,3]),c(fit_log[1,2], fit_log[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 20000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='red',lwd=2)

points(v.dir$dist,v.dir$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue')
curve(v.f.est(x, C0=v.anis[2,2]+v.anis[1,2], 
              cov.pars=rbind(c(v.anis[2,2], v.anis[2,3]),c(v.anis[1,2], v.anis[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 20000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='steelblue',lwd=2)

# meglio il modello con anisotropy


#----------------------------------------------------------------------------------
# try anisotropy - bc

v.dir <- variogram(bc.med_dwell ~ sqrt(dist),data_spatial,alpha=(0:3)*45) 
plot(v.dir)
v.anis <- vgm(7, "Sph", 15000, 4, anis=c(45, 0.3))

print(plot(v.dir, v.anis, pch=19))

#boh forse 135?

v.dir <- variogram(bc.med_dwell ~ sqrt(dist),data_spatial,alpha=135, cutoff=20000) 
plot(v.dir)
v.anis <- fit.variogram(v.dir,vgm(7, "Exp", 15000, 4, alpha=135))

x11()
plot(v.dir, v.anis, pch = 3)
v.anis


# compare the two variograms
v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}

x11()
plot(v.bc$dist,v.bc$gamma,xlab='distance',ylab='semivariance',pch=19,col='red')
curve(v.f.est(x, C0=fit.bc[2,2]+fit.bc[1,2], cov.pars=rbind(c(fit.bc[2,2], fit.bc[2,3]),c(fit.bc[1,2], fit.bc[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 20000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='red',lwd=2)

points(v.dir$dist,v.dir$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue')
curve(v.f.est(x, C0=v.anis[2,2]+v.anis[1,2], 
              cov.pars=rbind(c(v.anis[2,2], v.anis[2,3]),c(v.anis[1,2], v.anis[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 20000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='steelblue',lwd=2)

# è uguale, c'è qualcosa che non va....



#--------------------------------------------------
# trovare i coefficienti del modello migliore

g.tr <- gstat(formula = log(med_dwell_area) ~ sqrt(dist), data = data_spatial, model = v.anis)

pnull=0;
pone=1;
snull.new=data.frame(coord.x=514703.8, coord.y=5035569.3) # VAN BENE COORDINATE QUALSIASI
snull.new <- as.data.frame(c(snull.new,pnull))
names(snull.new) <- c('coord.x','coord.y','dist') #CAMBIARE x, y, population CON LE VARIABILI GIUSTE
coordinates(snull.new) <- c('coord.x','coord.y')

sone.new=data.frame(coord.x=514703.8, coord.y=5035569.3) # UTM coordinates
sone.new <- as.data.frame(c(sone.new,pone))
names(sone.new) <- c('coord.x','coord.y','dist')  #CAMBIARE x, y, population CON LE VARIABILI GIUSTE
coordinates(sone.new) <- c('coord.x','coord.y')


#COEFF a0 E a1
a0=predict(g.tr, snull.new, BLUE = TRUE)$var1.pred
a0
a1=predict(g.tr, sone.new, BLUE = TRUE)$var1.pred-a0
a1

#---------------------------------------------------------------------
# spatial prediction

g.tr <- gstat(formula = log(med_dwell_area) ~ sqrt(dist), data = data_spatial, model = v.anis)
# inserire le coordinate (longitudine, latitudine) che vogliamo
x=-74.3
y=40.5

coord<-SpatialPoints(cbind(x,y),proj4string=CRS("+proj=longlat"))
coord.UTM <- spTransform(coord, CRS("+proj=utm +zone=17 +datum=WGS84"))

stop.new=data.frame(coord.x=coord.UTM@coords[,1], y=coord.UTM@coords[,2]) # UTM coordinates
s0.dist <- min(rowSums(scale(t(rbind(coord.UTM.NY@coords[,1],coord.UTM.NY@coords[,1])),stop.new)^2))
stop.new <- as.data.frame(c(stop.new,s0.dist)) 
names(stop.new) <- c('coord.x','coord.y','dist') 
coordinates(stop.new) <- c('coord.x','coord.y')

predict(g.tr, stop.new, BLUE = FALSE)
