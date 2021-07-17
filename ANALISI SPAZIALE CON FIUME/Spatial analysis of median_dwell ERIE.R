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


centroids_NY <- st_centroid(geo$geometry, of_largest_polygon = FALSE)
coord_NY <- as.numeric(unlist(centroids_NY))
coord.x_long <- coord_NY[seq(1,length(coord_NY),by=2)]
coord.y_lat <- coord_NY[seq(2,length(coord_NY),by=2)]

# x11() #controlla se plotta i centroidi
# plot(coord.x_long,coord.y_lat,xlab="longitude",ylab="latitude",lwd=2)

coord<-SpatialPoints(cbind(coord.x_long,coord.y_lat),proj4string=CRS("+proj=longlat"))
coord.UTM.NY <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))

coord.x <- coord.UTM.NY@coords[,1]
coord.y <- coord.UTM.NY@coords[,2]

CBG_RIVER<-geo[which(geo$BlockGroup==0),]

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
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue")


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

hist(bc.med_dwell, breaks=16, col="grey", main='Histogram of bc dist', prob = TRUE, xlab = 'bc dist from home')

x11()
xyplot(bc.med_dwell ~ sqrt(distance), as.data.frame(data_spatial))

data_spatial <-data.frame(coord.x,coord.y, bc.med_dwell, dist)
coordinates(data_spatial)<-c('coord.x', 'coord.y')


v <- variogram(bc.med_dwell ~ sqrt(dist), data = data_spatial, cutoff=20000)
plot(v)

fit=fit.variogram(v, vgm(1.5, model='Exp', 15000, nugget=1))
fit

x11()
plot(v, fit, pch = 3)
