###################################
### SPATIAL ANALYSIS WITH RIVER ###
###################################

library(geosphere)
library(sf)

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
CBG_ny$geometry<-st_geometry(CBG_ny$geometry)
centroids <- st_centroid(CBG_ny$geometry, of_largest_polygon = FALSE)

x11()
plot(st_geometry(CBG_ny$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
par(new=T)
plot(st_geometry(CBG_RIVER$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = "lightblue")
par(new=T)
plot(centroids, xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ", pch='.')

spatial_data <- as(CBG_ny, "Spatial")

#------------------------------------------------------------
# Spatial Correlation

bubble(data,'median_dwell',do.log=TRUE,key.space='bottom')


svgm <- variogram(New_York_County$median_dwell ~ 1, CBG_ny, locations = centroids) #response variable ~ 1 #stationary dataset
# Zs = ms + delta_s
# sum_l a_l*f_l(s) + delta_s amd with 1 we say "take only f_0(s)"
plot(svgm, main = 'Sample Variogram',pch=19)

plot(variogram(New_York_County$median_dwell ~ 1, CBG_ny,alpha = c(0, 45, 90, 135),locations = centroids),pch=19)

# Fit
v <- variogram(New_York_County$median_dwell ~ 1, CBG_ny,locations = centroids)
plot(v,pch=19)

v.fit<-fit.variogram(v, vgm(1, "Sph", 800, 1))

plot(v, v.fit, pch = 19)
