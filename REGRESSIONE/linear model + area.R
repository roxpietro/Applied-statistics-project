load("Patterns_NY.RData")
load("Original_Cyber_Capital.RData")

library(geosphere)
library(car)

attach(census_blocks_ny)


area_CBG=matrix(nrow = 15463, ncol = 1)
#par(mfrow=c(4,5))
for (i in 1:15463) {
  #points=census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2]
  coords=census_blocks_ny$geometry[[i]][[1]][[1]]
  area_CBG[i]=areaPolygon(coords)
}

Census_Blocks_NY<-data.frame(CensusBlockGroup, area_CBG)

detach(census_blocks_ny)

rm(census_blocks_ny)
rm(census_metadata)
rm(patterns_ny)
rm(coords)
rm(area_CBG)

attach(Census_Blocks_NY)

attach(Patterns_NY)
Patterns_NY <- Patterns_NY[order(area),]

Census_Blocks_NY<-Census_Blocks_NY[order(Census_Blocks_NY$CensusBlockGroup),]

area_cbg=c()
j=0
for (i in 1:15463) {
  while(Patterns_NY$area[i]>Census_Blocks_NY$CensusBlockGroup[i+j] && i<=15446) {
    j=j+1
  }
  if (Patterns_NY$area[i]==Census_Blocks_NY$CensusBlockGroup[i+j] && i<=15446) {
    area_cbg[i]=Census_Blocks_NY$area_CBG[i]
  }
}


LM_1=data.frame(median_dwell, raw_stop_counts, raw_device_counts, distance_from_home, distance_from_primary_daytime_location, area_cbg)

detach(Census_Blocks_NY)
detach(Patterns_NY)
rm(Census_Blocks_NY)
rm(area_cbg)



attach(LM_1)



lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,distance_from_home,distance_from_primary_daytime_location,area_cbg,median_dwell))

BC.stop <- bcPower(raw_stop_counts, 0)
BC.device <- bcPower(raw_device_counts, 0) #capire se tenere il reale o metterlo a zero
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[6])
BC.home <- bcPower(distance_from_home, 0)
BC.primary <- bcPower(distance_from_primary_daytime_location, 0)
BC.area <- bcPower(area_cbg, lambda_multivariate$lambda[5])

mod_multivariate_complete=lm(formula = BC.median ~ BC.stop + BC.device + BC.home + BC.area)
summary(mod_multivariate_complete) #0.61






detach(LM_1)





