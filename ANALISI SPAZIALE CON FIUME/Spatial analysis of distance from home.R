
library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(ggplot2)
library(raster)
library(rgdal)


### GRAFICI su manhattan con colori rispetto a una quantità spatial ------------
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")

New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

attach(New_York_County_no_river)

#coordinate in utm
centroids_NY <- st_centroid(CBG_ny_no_river$geometry, of_largest_polygon = FALSE)
coord_NY <- as.numeric(unlist(centroids_NY))
coord.x_long <- coord_NY[seq(1,length(coord_NY),by=2)]
coord.y_lat <- coord_NY[seq(2,length(coord_NY),by=2)]
coord<-SpatialPoints(cbind(coord.x_long,coord.y_lat),proj4string=CRS("+proj=longlat"))
coord.UTM.NY <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))

coord.x <- coord.UTM.NY@coords[,1]
coord.y <- coord.UTM.NY@coords[,2]

#diversi z da vedere
#z<- ....

##focus su DISTANCE_FROM_HOME
rem <- which(distance_from_home > 20000) #[36] > 125000, [175]  > 50000, [1,9] >30000 , [92] >20000
CBG_ny_no_river$CensusBlockGroup[rem]
#dati molto alti che non fanno vedere pattern nel grafico -> da capire se togliere o no
min(distance_from_home)
max(distance_from_home)
# data_spatial <-data.frame(coord.x,coord.y, z)
# coordinates(data_spatial)<-c('coord.x', 'coord.y')
x11()
#png(file = "glop distance from home")
distance__from_home <-distance_from_home[-rem]
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-rem], aes(fill=distance__from_home))+scale_fill_gradient(low="lightyellow", high="red") +
  geom_sf(data = CBG_ny_no_river$geometry[rem,], fill="black")+
  geom_sf(data = CBG_ny_no_river$geometry[which(CBG_ny_no_river$TractCode=="011300"),], fill="yellow") +
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")
dev.off()
# non rimuovo dati
#f(s_i) = distanza da ipotetico centro di times square
k <- which(CBG_ny_no_river$TractCode == '011300')
centroid_TimesSquare <- st_centroid(CBG_ny_no_river$geometry[k,], of_largest_polygon = FALSE)
coord_cTS <- as.numeric(unlist(centroid_TimesSquare))
coord_cTS.x_long <- coord_cTS[1]
coord_cTS.y_lat <- coord_cTS[2]
distance<-distm(cbind(coord.x_long, coord.y_lat), cbind(coord_cTS.x_long, coord_cTS.y_lat), fun = distGeo)

data_spatial <-data.frame(coord.x,coord.y, distance_from_home, distance)
coordinates(data_spatial)<-c('coord.x', 'coord.y')

#choice of f(si)
f_distance <- sqrt(distance)

x11()
spplot(data_spatial,'distance_from_home')

hist(distance_from_home, breaks=16, col="grey", main='Histogram of distance_from_home', prob = TRUE, xlab = 'distance from home') #asymmetric data
# highly skewed, transform to the log
hist(log(distance_from_home), breaks=16, col="grey", main='Histogram of log(distance_from_home)', prob = TRUE, xlab = 'log(distance_from_home)')
#try box-cox
library(car)
lambda <- powerTransform(distance_from_home)
bc.distance_from_home <- bcPower(distance_from_home, lambda$lambda)
hist(bc.distance_from_home, breaks=16, col="grey", main='Histogram of bc dist', prob = TRUE, xlab = 'bc dist from home')

x11()
png(file = "nuvola log dfh.png")
xyplot(log(distance_from_home) ~ sqrt(distance), as.data.frame(data_spatial))
dev.off()
png(file = "nuvola box-cox dfh.png")
xyplot(bc.distance_from_home ~ sqrt(distance), as.data.frame(data_spatial))
dev.off()
# stationary variogram vs ->male
v.slog <- variogram(log(distance_from_home) ~ 1, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
fit1=fit.variogram(v.slog, vgm(0.4, model='Exp', 5000, nugget=0.2))
x11()
plot(v.slog, fit1, pch = 3)

v.sbc <- variogram(bc.distance_from_home ~ 1, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
fit1=fit.variogram(v.sbc, vgm(0.4, model='Exp', 5000, nugget=0.2))
x11()
plot(v.sbc, fit1, pch = 3)

#--------------------------------
# non stationary variogram v
vlog <- variogram(log(distance_from_home) ~ f_distance, data = data_spatial)
plot(vlog)

vlog <- variogram(log(distance_from_home) ~ f_distance, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
plot(vlog)
vlog.fit <- fit.variogram(vlog, vgm(0.3, "Exp", 3000, 0.05))
x11()
png(file="variogram log dfh.png")
plot(vlog, vlog.fit, pch = 3)
dev.off()
vlog.fit
#with box-cox
vbc <- variogram(bc.distance_from_home ~ f_distance, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
plot(vbc)
vbc.fit <- fit.variogram(vbc, vgm(50, "Exp", 3000, 10)) #o convergence after 200 iterations: try different initial values?
x11()
png(file="variogram bc dfh.png")
plot(vbc, vbc.fit, pch = 3)
dev.off()
vbc.fit

## ----------- rimuovo i picchi ----------------
dist_from_home <- distance_from_home[-rem]
dist <- distance[-rem]
data_spat <- data.frame(coord.x[-rem],coord.y[-rem], dist_from_home, distance[-rem])
names(data_spat) <- c('x','y','dist home', 'distance')
coordinates(data_spat)<-c('x', 'y')
#choice of f(si)
f_dist <- sqrt(dist)
x11()
hist(log(dist_from_home), breaks=16, col="grey", main='Histogram of log(distance_from_home)', prob = TRUE, xlab = 'log(distance_from_home)')
#try box-cox
library(car)
lambda <- powerTransform(dist_from_home)
bc.dist_from_home <- bcPower(dist_from_home, lambda$lambda)
x11()
hist(bc.dist_from_home, breaks=16, col="grey", main='Histogram of bc dist', prob = TRUE, xlab = 'bc dist from home')

x11()
png(file="nuvola log dfh nomax.png")
xyplot(log(dist_from_home) ~ sqrt(dist), as.data.frame(data_spat))
dev.off()
x11()
png(file="nuvola bc dfh nomax.png")
xyplot(bc.dist_from_home ~ sqrt(dist), as.data.frame(data_spat))
dev.off()
# stationary variogram vs -> male
v.slog <- variogram(log(dist_from_home) ~ 1, data = data_spat,boundaries = c(0,200,seq(400,6000,450)))
fit1=fit.variogram(v.slog, vgm(0.4, model='Exp', 5000, nugget=0.2))
x11()
plot(v.slog, fit1, pch = 3)

v.sbc <- variogram(bc.dist_from_home ~ 1, data = data_spat,boundaries = c(0,200,seq(400,6000,450)))
plot(v.sbc)
fit1=fit.variogram(v.sbc, vgm(5000, model='Exp', 5000, nugget=1000))
x11()
png(file ="variogram stationary bc.png")
plot(v.sbc, fit1, pch = 3)
dev.off()
#--------------------------------
# non stationary variogram v
vlog.nomax <- variogram(log(dist_from_home) ~ f_dist, data = data_spat)
plot(vlog.nomax)

vlog.nomax <- variogram(log(dist_from_home) ~ f_dist, data = data_spat, boundaries = c(0,200,seq(400,6000,450)))
plot(vlog.nomax)
vlog.fit.nomax <- fit.variogram(vlog.nomax, vgm(0.3, "Exp", 3000, 0.05))
x11()
png(file="variogram log dfh nomax.png")
plot(vlog.nomax, vlog.fit.nomax, pch = 3)
dev.off()
vlog.fit.nomax
#with box-cox
vbc.nomax <- variogram(bc.dist_from_home ~ f_dist, data = data_spat,boundaries = c(0,200,seq(400,6000,450)))
plot(vbc.nomax)
vbc.fit.nomax <- fit.variogram(vbc.nomax, vgm(5000, "Exp", 3000, 1000))
x11()
png(file="variogram bc dfh nomax.png")
plot(vbc.nomax, vbc.fit.nomax, pch = 3)
dev.off()
vlog.fit.nomax

#--------------------------------
# ## DUMMY per il sud ma mh perdo la variabilità + con picchi
# sud <- c(which(CBG_ny_no_river$TractCode<="013900"), 
#          which(CBG_ny_no_river$TractCode=='031900'),
#          which(CBG_ny_no_river$TractCode=='031704'),
#          which(CBG_ny_no_river$TractCode=='031703'),
#          which(CBG_ny_no_river$TractCode=='031704'))
# DUMMY <- rep(0,1092)
# DUMMY[sud] <- 1
# data <- data.frame(coord.x, coord.y, distance_from_home,DUMMY, distance)
# coordinates(data) <- c('coord.x','coord.y')
# x11()
# # non stationary variogram v
# vlog <- variogram(log(distance_from_home) ~ DUMMY + f_distance + f_distance*DUMMY, data = data)
# plot(vlog)
# # non converge, dato che c'? poca variabilit? tra i dati
# vlog <- variogram(log(distance_from_home) ~ DUMMY + f_distance + f_distance*DUMMY, data = data,boundaries = c(0,200,seq(400,6000,450)))
# plot(vlog)
# vlog.fit <- fit.variogram(vlog, vgm(0.3, "Exp", 3000, 0.05))
# x11()
# plot(vlog, vlog.fit, pch = 3)
# vlog.fit
# #with box-cox
# vbc <- variogram(bc.distance_from_home ~ DUMMY + f_distance + f_distance*DUMMY, data = data_spatial,boundaries = c(0,200,seq(400,6000,450)))
# plot(vbc)
# vbc.fit <- fit.variogram(vbc, vgm(5000, "Sph", 3000, 1000))
# # vbc.fit <- fit.variogram(vbc, vgm(5000, "Exp", 3000, 1000))
# #Warning messages:
# # 1: In fit.variogram(vbc, vgm(5000, "Exp", 3000, 1000)) :
# #   No convergence after 200 iterations: try different initial values?
# #   2: In fit.variogram(object, model, fit.sills = fit.sills, fit.ranges = fit.ranges,  :
# #                         singular model in variogram fit
# x11()
# plot(vbc, vbc.fit, pch = 3)
# vlog.fit

#--------------------------------------------
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/ANALISI SPAZIALE CON FIUME/presentazione spatial")
# compare the two variograms
v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}
#v con log(distance_from_home) con picchi
x11()
# png(file = "log vs log senza picchi.png")
plot(vlog$dist,vlog$gamma,xlab='distance',ylab='semivariance',pch=19,col='red')
curve(v.f.est(x, C0=vlog.fit[2,2]+vlog.fit[1,2], cov.pars=rbind(c(vlog.fit[2,2], vlog.fit[2,3]),c(vlog.fit[1,2], vlog.fit[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 6000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='red',lwd=2)

points(vlog.nomax$dist,vlog.nomax$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue')
curve(v.f.est(x, C0=vlog.fit.nomax[2,2]+vlog.fit.nomax[1,2], 
              cov.pars=rbind(c(vlog.fit.nomax[2,2], vlog.fit.nomax[2,3]),c(vlog.fit.nomax[1,2], vlog.fit.nomax[1,3])), cov.model = c("exponential","pure.nugget")), from = 0.0001, to = 6000,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='steelblue',lwd=2)

dev.off()
# scegliamo il modello non stazionario!


