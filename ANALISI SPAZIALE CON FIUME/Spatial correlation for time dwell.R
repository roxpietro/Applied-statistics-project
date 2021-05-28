###################################
### SPATIAL ANALYSIS WITH RIVER ###
###################################

library(geosphere)
library(sf)
library(lattice)           ## Data management
library(rgdal)


# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA 
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/River_Dataset.RData")
# terri
#load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Data frame county/New York County.RData") #TERRI
#load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")

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
# Save centroid coordinates of each CBG
centroids <- st_centroid(CBG_ny$geometry, of_largest_polygon = FALSE)


x11()
plot(st_geometry(CBG_ny$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
par(new=T)
plot(st_geometry(CBG_RIVER$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = "lightblue")
par(new=T)
plot(centroids, xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ", pch='.')

coord <- as.numeric(unlist(centroids))
coord.x <- coord[seq(1,length(coord),by=2)]
coord.y <- coord[seq(2,length(coord),by=2)]

coord<-SpatialPoints(cbind(coord.x,coord.y),proj4string=CRS("+proj=longlat"))
coord.UTM <- spTransform(coord, CRS("+init=epsg:32748"))

data_spatial <-data.frame(CBG_ny, New_York_County$median_dwell, coord.UTM)
data_spatial2 <-data.frame(New_York_County$median_dwell, coord.UTM)

coordinates(data_spatial2) <- c('coord.x','coord.y')


x11()
bubble(data_spatial2, 'New_York_County.median_dwell', do.log=TRUE,key.space='bottom')
spplot(data_spatial2,'New_York_County.median_dwell')

median_dwell <-data_spatial2$New_York_County.median_dwell

ggplot() + 
  geom_sf(data = data_spatial$geometry, aes(fill=median_dwell))+scale_fill_gradient(low="lightyellow", high="blue")


# i_range1<-which(data_spatial$New_York_County.median_dwell<40 )
# i_range2<-which(data_spatial$New_York_County.median_dwell<120 & data_spatial$New_York_County.median_dwell>=40 )
# i_range3<-which(data_spatial$New_York_County.median_dwell>=120 )
# 
# 
# 
# library(viridis)
# colors <- rainbow(3)
# x11()
# plot(st_geometry(CBG_ny$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
# par(new=T)
# plot(st_geometry(CBG_ny$geometry[i_range1]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ", col=colors[1])
# par(new=T)
# plot(st_geometry(CBG_ny$geometry[i_range2]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ", col=colors[2])
# par(new=T)
# plot(st_geometry(CBG_ny$geometry[i_range3]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ", col=colors[3])
# legend("topleft", c("[0,40]","[40,120]","[120,400]"),fill=colors)


hist(data_spatial2$New_York_County.median_dwell,breaks=16, col="grey", main='Histogram of median dwell', prob = TRUE) #asymmetric data


#------------------------------------------------------------------------------------------------------------------------------------
# ALL STATE of NY
 load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Complete_dataset.RData")
# 
# i_range1<-which(complete_dataset$median_dwell<50 )
# i_range2<-which(complete_dataset$median_dwell<100 & complete_dataset$median_dwell>=50 )
# i_range3<-which(complete_dataset$median_dwell<150 & complete_dataset$median_dwell>=100 )
# i_range4<-which(complete_dataset$median_dwell<200 & complete_dataset$median_dwell>=150 )
# i_range5<-which(complete_dataset$median_dwell<300 & complete_dataset$median_dwell>=200 )
# i_range7<-which(complete_dataset$median_dwell<450 & complete_dataset$median_dwell>=300 )
# i_range8<-which(complete_dataset$median_dwell<800 & complete_dataset$median_dwell>=450 )
# 
# 
# x11()
# colors <- rainbow(8)
# plot(st_geometry(complete_dataset$geometry[i_range1]),xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = rainbow(1))
#   par(new=T)
# plot(st_geometry(complete_dataset$geometry[i_range2]),xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = rainbow(2))
#   par(new=T)
# plot(st_geometry(complete_dataset$geometry[i_range3]),xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = rainbow(3))
#   par(new=T)
# plot(st_geometry(complete_dataset$geometry[i_range4]),xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = rainbow(4))
#   par(new=T)
# plot(st_geometry(complete_dataset$geometry[i_range5]),xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = rainbow(5))
#   par(new=T)
# plot(st_geometry(complete_dataset$geometry[i_range7]),xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = rainbow(7))
#   par(new=T)
# plot(st_geometry(complete_dataset$geometry[i_range8]),xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = rainbow(8))
#   par(new=T)
#   
# legend("bottomleft",c("[0,50]","[50,100]","[100,150]","[150,200]","[200,300]","[300,450]","[450,800]"),fill =colors)
# title("Median_Dwell of the State of NY")


median_dwell2<-complete_dataset$median_dwell
ggplot() + 
  geom_sf(data = complete_dataset$geometry, aes(fill=median_dwell2))+scale_fill_gradient(low="lightblue", high="red")

rm(median_dwell2)
rm(complete_dataset)

#------------------------------------------------------------
# Spatial Correlation
library(automap)

svgm <- variogram(data_spatial2$New_York_County.median_dwell ~ 1, data_spatial2) #response variable ~ 1 #stationary dataset
# Zs = ms + delta_s
# sum_l a_l*f_l(s) + delta_s amd with 1 we say "take only f_0(s)"
plot(svgm, main = 'Sample Variogram',pch=19)

plot(variogram(data_spatial2$New_York_County.median_dwell ~ 1, data_spatial2,alpha = c(0, 45, 90, 135)),pch=19)

# Fit
v <- variogram(data_spatial2$New_York_County.median_dwell ~ 1, data_spatial2,alpha=45 )
plot(v,pch=19)

v.fit = autofitVariogram(data_spatial2$New_York_County.median_dwell~1,
                             data_spatial2,
                             model = c("Ste"),
                         kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                         fix.values = c(NA, NA, NA),
                         start_vals = c(NA,NA,NA),
                         verbose = T)

# da modificare per trovare il modello migliore


plot(v, v.fit$var_model, pch = 19)
