# library(geosphere)
# library(sf)
# library(sp)           ## Data management
# library(lattice)      ## Data management
# library(geoR)         ## Geostatistics
# library(gstat)        ## Geostatistics
# library(ggplot2)
# library(raster)
# library(rgdal)

load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")

New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

attach(New_York_County_no_river)

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
inside <- c()
tot_home <- c()
home <- c()
for (j in 1:dim(New_York_County_no_river)[1]){
  home <- names(device_home_areas[[j]])
  tot_home[j] <- length(home)
  index_home <- c()
  tot <- 0
  for (i in 1: length(home)){
    k <- which(census_blocks_ny$CensusBlockGroup == home[i])
    if (length(k) != 0){
      tot <- tot+1
      index_home[tot] <-k
    }
  }
  inside[j] <- tot

}

outside <- tot_home-inside
data <- data.frame(total_home = tot_home, home_in = inside, home_out = outside)
x11()
pairs(data)

inout <- data.frame(home_in = inside, home_out = outside)
#che ci sia legame lineare tra totale e una sua parte ok me lo aspetto abbastanza 
#incuriosisce che chi ha tot capacità attrattiva per persone che vengono da dentro ha una attrattiva analoga per persone che vengono da fuori
#quindi potrebbero essere principalmente zone ad equo potere attrattivo (turisti princ...)

mod <- lm(home_in ~ home_out, inout)
summary(mod) #Adjusted R-squared:  0.771,  pvalue bassisimi
shapiro.test(mod$residuals) #< 2.2e-16
x11()
par(mfrow = c(2,2))
plot(mod)
#tot di outliers, per ora lasciamoli

library(car)
lambda <- powerTransform(cbind(inside, (outside+1e-16)))
which(outside <=0) #743 1079
lambda$lambda
bc.inside <- bcPower(inside, lambda$lambda[1])
bc.outside <- bcPower((outside+1e-16), lambda$lambda[2])
bc.data <- data.frame(total_home = tot_home, home_in = bc.inside, home_out = bc.outside)
x11()
pairs(bc.data)

bc.mod <- lm(bc.inside ~ bc.outside)
summary(bc.mod) #Adjusted R-squared:  0.7043, pvalue bassisimi
shapiro.test(bc.mod$residuals) #p-value = 1.278e-11
x11()
par(mfrow = c(2,2))
plot(bc.mod)

#torno indietro e rimuovo outliers
rem <-c(36,92,401,532)
data_noout <- data[-rem,]
mod <- lm(home_in ~ home_out, data)
summary(mod) #Adjusted R-squared:  0.729,  pvalue bassisimi
shapiro.test(mod$residuals) #< 2.2e-16
x11()
par(mfrow = c(2,2))
plot(mod)
x11()
boxplot(data)
hist(inside)
hist(outside)

#rimuovo outliers a occhio...
x11()
plot(outside, inside, main='Scatterplot outside vs inside NY state', lwd=2,
     xlab='outside', ylab='inside')
text(outside, inside, dimnames(inout)[[1]],cex=1)
abline(h=1100)
abline(v=300)
x11()
plot(outside[which(inside<1100 & outside<300)], inside[which(inside<1100 & outside<300)], main='Scatterplot brain weight vs body weight', lwd=2,
     xlab='Body weight', ylab='Brain weight')

remove <- which(inside>=1100 | outside>=300)
remove
inside[remove]
outside[remove]
col = rep('black',1092)
col[remove] = 'red'
x11()
plot(outside, inside, main='Scatterplot outside vs inside NY state', lwd=2,
     xlab='outside', ylab='inside', col = col)
text(outside, inside, dimnames(inout)[[1]],cex=1)

new_inside <- inside[-remove]
new_outside <- outside[-remove]
new_mod <- lm(new_inside ~ new_outside)
summary(new_mod) #Adjusted R-squared: 0.7185,  pvalue bassisimi
shapiro.test(new_mod$residuals) #< 2.2e-16
x11()
par(mfrow = c(2,2))
plot(new_mod)
x11()
hist(new_inside)
hist(new_outside)

lambda <- powerTransform(cbind(new_inside, (new_outside+1e-16)))
which(new_outside <=0) #743 1079
lambda$lambda
bc.inside <- bcPower(new_inside, lambda$lambda[1])
bc.outside <- bcPower((new_outside+1e-16), lambda$lambda[2])
x11()
hist(bc.inside)
hist(bc.outside)

bc.mod_new <- lm(bc.inside~bc.outside)
summary(bc.mod_new)#0.6912 
shapiro.test(bc.mod_new$residuals)#p-value = 1.238e-08

plot(bc.inside, bc.outside)
############# plotto gli ouliers, quelli che hanno outside molto alti ...#######

library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(ggplot2)
library(raster)
library(rgdal)

load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")

#coordinate in utm
centroids_NY <- st_centroid(CBG_ny_no_river$geometry, of_largest_polygon = FALSE)
coord_NY <- as.numeric(unlist(centroids_NY))
coord.x_long <- coord_NY[seq(1,length(coord_NY),by=2)]
coord.y_lat <- coord_NY[seq(2,length(coord_NY),by=2)]
coord<-SpatialPoints(cbind(coord.x_long,coord.y_lat),proj4string=CRS("+proj=longlat"))
coord.UTM.NY <- spTransform(coord, CRS("+proj=utm +zone=18 +datum=WGS84"))

coord.x <- coord.UTM.NY@coords[,1]
coord.y <- coord.UTM.NY@coords[,2]

rem_out <- which(outside>=300)
rem_in <- which(inside>=1100)
obj <-outside[-remove]
x11()
#png(file = "glop distance from home.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-remove], aes(fill=obj))+scale_fill_gradient(low="yellow", high="red") +
  geom_sf(data = CBG_ny_no_river$geometry[-remove]) +
  geom_sf(data = CBG_ny_no_river$geometry[remove,], fill="violet")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")
dev.off()

####### guardiamo la percentuale di gente per igni cbg che viene da fuori ######
out_NY_city <- rep(0, dim(New_York_County_no_river)[1]) #conterà quanti hanno casa fuori da manhattan
out_NY_state <-rep(0, dim(New_York_County_no_river)[1]) #conterà quanti hanno casa fuori da ny state

inside <- c()
tot_home <- c()
home <- c()
for (j in 1:dim(New_York_County_no_river)[1]){
  home <- names(device_home_areas[[j]])
  tot_home[j] <- length(home)
  index_home <- c()
  tot <- 0
  for (i in 1: length(home)){
    k_state <- which(census_blocks_ny$CensusBlockGroup == home[i])
    k_city <- which(CBG_ny_no_river$CensusBlockGroup == home[i])
    if (length(k_state) == 0){
      out_NY_state[j] <- sum(out_NY_state[j],device_home_areas[[j]][i])
    }
    if (length(k_city) == 0){
      out_NY_city[j] <- sum(out_NY_city[j],device_home_areas[[j]][i])
    }
  }
  inside[j] <- tot
  
}

outside <- tot_home-inside

################### guardiamo le diverse fasce orarie??? ###############