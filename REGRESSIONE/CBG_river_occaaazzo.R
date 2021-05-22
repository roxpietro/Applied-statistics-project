##########################
### PLOTS of MANHATTAN ###
##########################

library(geosphere)
library(sf)

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA 
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
# terri
#load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Data frame county/New York County.RData") #TERRI


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


attach(New_York_County)
lambda_multivariate <- powerTransform(cbind(New_York_County$raw_stop_counts, New_York_County$raw_device_counts,New_York_County$distance_from_home,New_York_County$distance_from_primary_daytime_location,New_York_County$median_dwell))
lambda_multivariate

BC.stop <- bcPower(raw_stop_counts, 0)
BC.device <- bcPower(raw_device_counts, 0)
BC.home <- bcPower(distance_from_home, lambda_multivariate$lambda[3])
BC.primary <- bcPower(distance_from_primary_daytime_location, lambda_multivariate$lambda[4])
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[5])

LM_2 <- data.frame(BC.stop,BC.device ,BC.home,BC.primary,median_dwell )

index_stop<-which(BC.stop<=6.5)
area_stop<-New_York_County$area[index_stop]

index_cbg_stop<-c()
for (i in 1:length(area_stop))
  index_cbg_stop[i] <- which(area_stop[i]==CBG_ny$CensusBlockGroup)

x11()
plot(st_geometry(CBG_ny$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
par(new=T)
plot(st_geometry(CBG_ny$geometry[index_cbg_stop]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = "red")

