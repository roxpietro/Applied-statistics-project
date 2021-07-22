setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/ANALISI FUNZIONALE STOPS")

load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")

New_York_County_no_river= New_York_County_no_river[order( New_York_County_no_river$area),]
CBG_ny_no_river<-CBG_ny_no_river[order( CBG_ny_no_river$CensusBlockGroup),]
attach( New_York_County_no_river)

stops<-matrix(nrow = 1092, ncol=30*24)

for (i in 1:dim( New_York_County_no_river)[1]) {
  stops[i,]<-stops_by_each_hour[[i]]
}

detach( New_York_County_no_river)

stops<-stops[,505:672]

stops<-t(stops)
colnames(stops)<- New_York_County_no_river$area
x11()
matplot(stops,type='l', main = "22/06 - 28/06")
#saltiamo smoothing per ora ...

# outliers
out=c(92,532)
cbg_out=New_York_County_no_river[out,1]
layout(1)
x11()
matplot(stops,type='l')
lines(stops[,92],lwd=4, col=2) 
lines(stops[,532],lwd=4, col=1) 

out1 <- New_York_County_no_river[out[1],]
out2 <- New_York_County_no_river[out[2],]

days<-matrix(nrow = 7, ncol=24)
days[1,]<-out1$popularity_by_hour_monday[[1]]
days[2,]<-out1$popularity_by_hour_tuesday[[1]]
days[3,]<-out1$popularity_by_hour_wednesday[[1]]
days[4,]<-out1$popularity_by_hour_thursday[[1]]
days[5,]<-out1$popularity_by_hour_friday[[1]]
days[6,]<-out1$popularity_by_hour_saturday[[1]]
days[7,]<-out1$popularity_by_hour_sunday[[1]]
days<-t(days)
my_col <- rainbow(7)
x11()
png(file = "popularity matplot.png")
matplot(days,type='l', col = my_col, main = " Dyas popularity in CBG 360610031001") #7 giorni settimanali, già fatta la media dentro popularity ...
legend("topleft", legend= c('mon', 'tue','wed', 'thu', 'fri', 'sat', 'sun'), fill = my_col)
dev.off()
lun <- matrix(nrow = 5, ncol=24)
for (i in 1:5){
  inizio <- seq(1,720,24*7)[i]
  fine <- seq(24,720,24*7)[i]
  lun[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
lun[4,]
mar <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(25,720,24*7)[i]
  fine <- seq(48,720,24*7)[i]
  mar[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
mer <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(49,720,24*7)[i]
  fine <- seq(72,720,24*7)[i]
  mer[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
gio <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(73,720,24*7)[i]
  fine <- seq(96,720,24*7)[i]
  gio[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
ven <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(97,720,24*7)[i]
  fine <- seq(120,720,24*7)[i]
  ven[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
sab <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(121,720,24*7)[i]
  fine <- seq(144,720,24*7)[i]
  sab[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
dom <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(145,720,24*7)[i]
  fine <- seq(168,720,24*7)[i]
  dom[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}

lun <- t(lun)
mar <- t(mar)
mer <- t(mer)
gio <- t(gio)
ven <- t(ven)
sab <- t(sab)
dom <- t(dom)

library(RColorBrewer)
col = brewer.pal(n = 8, name = 'YlOrRd')
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/ANALISI FUNZIONALE STOPS/Plot/outliers 92")
x11()
png(file = 'Monday.png')
matplot(lun,type='l', col = col[3:8], lwd =2, main = 'Monday')
dev.off()
#col = brewer.pal(n = 8, name = 'YlGnBu')
x11()
png(file = 'Tuesday.png')
matplot(mar,type='l', col = col[4:8],  lwd =2, main = 'Tuesday')
dev.off()
#col = brewer.pal(n = 8, name = 'YlGn')
x11()
png(file = 'Wednesday.png')
matplot(mer,type='l', col = col[4:8], lwd=2, main = 'Wednesday')
dev.off()
#col = brewer.pal(n = 8, name = 'RdPu')
x11()
png(file = 'Thursday.png')
matplot(gio,type='l', col = col[4:8], lwd=2, main = 'Thursday')
dev.off()
#col = brewer.pal(n = 8, name = 'OrRd')
x11()
png(file = 'Friday.png')
matplot(ven,type='l', col = col[4:8], lwd=2, main = 'Friday')
dev.off()
#col = brewer.pal(n = 8, name = 'Greens')
x11()
png(file = 'Saturday.png')
matplot(sab,type='l', col = col[4:8], lwd=2, main = 'Saturday')
dev.off()
#col = brewer.pal(n = 8, name = 'Blues')
x11()
png(file = 'Sunday.png')
matplot(dom,type='l', col = col[4:8], lwd=2, main = 'Sunday')
dev.off()

#non so che grandi commenti si possano fare. mi viene in mente:
# - le prime settimane (quelle chiare) hanno tante persone anche se c'erano più 
#   restrizioni sugli spostamenti -> proteste articoli forse?
# - nel fine settimana in generale si vedono più persone
# - grande afflusso di gente (anche ora tarda), principalmente la sera -> forse 
#   perchè è zona di passaggio per ritorno a casa (vd ponte strade, Brooklyn Bridge)

days<-matrix(nrow = 7, ncol=24)
days[1,]<-out2$popularity_by_hour_monday[[1]]
days[2,]<-out2$popularity_by_hour_tuesday[[1]]
days[3,]<-out2$popularity_by_hour_wednesday[[1]]
days[4,]<-out2$popularity_by_hour_thursday[[1]]
days[5,]<-out2$popularity_by_hour_friday[[1]]
days[6,]<-out2$popularity_by_hour_saturday[[1]]
days[7,]<-out2$popularity_by_hour_sunday[[1]]
days<-t(days)
my_col <- rainbow(7)
x11()
png(file = "popularity matplot.png")
matplot(days,type='l', col = my_col, main = "Days popularity in CBG 360610143001" ) #7 giorni settimanali, già fatta la media dentro popularity ...
legend("topleft", legend= c('mon', 'tue','wed', 'thu', 'fri', 'sat', 'sun'), fill = my_col)
dev.off()
lun <- matrix(nrow = 5, ncol=24)
for (i in 1:5){
  inizio <- seq(1,720,24*7)[i]
  fine <- seq(24,720,24*7)[i]
  lun[i,] <- out2$stops_by_each_hour[[1]][inizio:fine]
}
lun[4,]
mar <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(25,720,24*7)[i]
  fine <- seq(48,720,24*7)[i]
  mar[i,] <- out2$stops_by_each_hour[[1]][inizio:fine]
}
mer <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(49,720,24*7)[i]
  fine <- seq(72,720,24*7)[i]
  mer[i,] <- out2$stops_by_each_hour[[1]][inizio:fine]
}
gio <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(73,720,24*7)[i]
  fine <- seq(96,720,24*7)[i]
  gio[i,] <- out2$stops_by_each_hour[[1]][inizio:fine]
}
ven <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(97,720,24*7)[i]
  fine <- seq(120,720,24*7)[i]
  ven[i,] <- out2$stops_by_each_hour[[1]][inizio:fine]
}
sab <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(121,720,24*7)[i]
  fine <- seq(144,720,24*7)[i]
  sab[i,] <- out2$stops_by_each_hour[[1]][inizio:fine]
}
dom <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(145,720,24*7)[i]
  fine <- seq(168,720,24*7)[i]
  dom[i,] <- out2$stops_by_each_hour[[1]][inizio:fine]
}

lun <- t(lun)
mar <- t(mar)
mer <- t(mer)
gio <- t(gio)
ven <- t(ven)
sab <- t(sab)
dom <- t(dom)

library(RColorBrewer)
col = brewer.pal(n = 8, name = 'YlOrRd')
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/ANALISI FUNZIONALE STOPS/Plot/outlier 532")
x11()
png(file = 'Monday.png')
matplot(lun,type='l', col = col[3:8], lwd =2, main = 'Monday')
dev.off()
#col = brewer.pal(n = 8, name = 'YlGnBu')
x11()
png(file = 'Tuesday.png')
matplot(mar,type='l', col = col[4:8],  lwd =2, main = 'Tuesday')
dev.off()
#col = brewer.pal(n = 8, name = 'YlGn')
x11()
png(file = 'Wednesday.png')
matplot(mer,type='l', col = col[4:8], lwd=2, main = 'Wednesday')
dev.off()
#col = brewer.pal(n = 8, name = 'RdPu')
x11()
png(file = 'Thursday.png')
matplot(gio,type='l', col = col[4:8], lwd=2, main = 'Thursday')
dev.off()
#col = brewer.pal(n = 8, name = 'OrRd')
x11()
png(file = 'Friday.png')
matplot(ven,type='l', col = col[4:8], lwd=2, main = 'Friday')
dev.off()
#col = brewer.pal(n = 8, name = 'Greens')
x11()
png(file = 'Saturday.png')
matplot(sab,type='l', col = col[4:8], lwd=2, main = 'Saturday')
dev.off()
#col = brewer.pal(n = 8, name = 'Blues')
x11()
png(file = 'Sunday.png')
matplot(dom,type='l', col = col[4:8], lwd=2, main = 'Sunday')
dev.off()

#anche qua che commneti boh, andamento abbastanza più regolare con picco sempre alla sera nel pre cena, alti flussi sempre anche mattina molto presto
library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(ggplot2)
library(raster)
library(rgdal)
### guardiamo da dove arriva la gente che va lì: HOME
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Complete_dataset.RData")
rm(patterns_ny)
rm(census_metadata)
# order patterns_ny and census_block_ny by CBG of New York County
complete_dataset = complete_dataset[order(complete_dataset$area),]
census_blocks_ny = census_blocks_ny[order(census_blocks_ny$CensusBlockGroup),]
#remove not common cbg
remove <- which(census_blocks_ny$CensusBlockGroup %in% complete_dataset$area == FALSE)
census_blocks_ny <- census_blocks_ny[-remove,]
dim(census_blocks_ny)

#coordinate in utm
centroids <- st_centroid(census_blocks_ny$geometry, of_largest_polygon = FALSE)
coord <- as.numeric(unlist(centroids))
coord.x_long <- coord[seq(1,length(coord),by=2)]
coord.y_lat <- coord[seq(2,length(coord),by=2)]
coord<-SpatialPoints(cbind(coord.x_long,coord.y_lat),proj4string=CRS("+proj=longlat"))
coord.UTM <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))

coord.x <- coord.UTM@coords[,1]
coord.y <- coord.UTM@coords[,2]

#out 1: 92 quello  grande vicino a Brookyln Bridge
home <- names(out1$device_home_areas[[1]])
index_home <- c()
tot <- 0 #già si vede che su 4892 cbg home diverse ne trova nello stato di NY 3125
for (i in 1: length(home)){
  k <- which(census_blocks_ny$CensusBlockGroup ==home[i])
  if (length(k) != 0){
    tot <- tot+1
    index_home[tot] <-k
  }
}

x11()
ggplot() + 
  geom_sf(data = census_blocks_ny$geometry) +
  geom_sf(data = census_blocks_ny$geometry[index_home], fill="red")+
  geom_sf(data = census_blocks_ny$geometry[which(census_blocks_ny$CensusBlockGroup == cbg_out[1]),], fill="yellow")
