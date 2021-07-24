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

New_York_County_no_river = New_York_County_no_river[order(New_York_County_no_river$area),]
CBG_ny_no_river = CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

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
#misurare quante home di device home from areas vengono da fuori NY state
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
#remove <- which(inside>=1100 | outside>=300)
rem_out <- which(outside>=300)
rem_in <- which(inside>=1100)
home_out_NYstate <-outside[-remove]
x11()
#png(file = "quantity home outside NY State.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-remove], aes(fill=home_out_NYstate))+scale_fill_gradient(low="moccasin", high="red") +
  geom_sf(data = CBG_ny_no_river$geometry[remove,], fill="red4")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Quantity of devices home outside NY State')
  dev.off()

####### guardiamo la percentuale di gente per igni cbg che viene da fuori ######
out_NY_city <- rep(0, dim(New_York_County_no_river)[1]) #conterà quanti hanno casa fuori da manhattan
out_NY_state <-rep(0, dim(New_York_County_no_river)[1]) #conterà quanti hanno casa fuori da ny state

in_NY_state <- rep(0, dim(New_York_County_no_river)[1])
in_NY_city<- rep(0, dim(New_York_County_no_river)[1])
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
    else{
      in_NY_state[j] <- sum(in_NY_state[j], device_home_areas[[j]][i])
    }
    if (length(k_city) == 0){
      out_NY_city[j] <- sum(out_NY_city[j],device_home_areas[[j]][i])
    }
    else{
      in_NY_city[j] <- sum(in_NY_city[j], device_home_areas[[j]][i])
    }
  }
  inside[j] <- tot
  
}
#check mongolo
which(out_NY_city ==0 & out_NY_state !=0)
which(in_NY_city !=0 & in_NY_state ==0)
outside <- tot_home-inside

data <- data.frame(city = out_NY_city, state = out_NY_state)
pairs(data)
#facciamo percentuale
perc.out_NY_city <- out_NY_city / sum_device_home_areas
perc.out_NY_state <- out_NY_state / sum_device_home_areas
perc.in_NY_city <- in_NY_city / sum_device_home_areas
perc.in_NY_state <- in_NY_state / sum_device_home_areas

percentage_out_Manhattan <- perc.out_NY_city
x11()
#png(file = "perc people outNYC .png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, aes(fill=percentage_out_Manhattan))+scale_fill_gradient(low="moccasin", high="red") +
  #geom_sf(data = CBG_ny_no_river$geometry[remove,], fill="red4")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Percentage of devices with HOME outside Manhattan')
dev.off()

percentage_out_NYState <- perc.out_NY_state
x11()
#png(file = "perc people outNYS.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, aes(fill=percentage_out_NYState))+scale_fill_gradient(low="moccasin", high="red") +
  #geom_sf(data = CBG_ny_no_river$geometry[remove,], fill="red4")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Percentage of devices with HOME outside NY State')
dev.off()

percentage_in_Manhattan <- perc.in_NY_city
x11()
#png(file = "perc people inNYC.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, aes(fill=percentage_in_Manhattan))+scale_fill_gradient(low="moccasin", high="red") +
  #geom_sf(data = CBG_ny_no_river$geometry[remove,], fill="red4")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Percentage of devices with HOME inside Manhattan')
dev.off()

percentage_in_NYState <- perc.in_NY_state
x11()
#png(file = "perc people inNYS.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, aes(fill=percentage_in_NYState))+scale_fill_gradient(low="moccasin", high="red") +
  #geom_sf(data = CBG_ny_no_river$geometry[remove,], fill="red4")+
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Percentage of devices with HOME inside NY State')
dev.off()

#classifica zone turistiche e non??

x11()
hist(perc.in_NY_city)
hist(perc.in_NY_state)
hist(perc.out_NY_city)
hist(perc.out_NY_state)

library(mvtnorm)
library(rgl)
library(car)

# data <- cbind(perc.in_NY_city, perc.in_NY_state, perc.out_NY_city, perc.out_NY_state)
# iris.e <- dist(data, method='euclidean')
# iris.es <- hclust(iris.e, method='single')
# names(iris.es)
# 
# # plot of the dendrograms
# x11()
# # par(mfrow=c(1,3))
# plot(iris.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# rect.hclust(iris.es, k=2)
# 
# cluster.ec <- cutree(iris.ec, k=2) # euclidean-complete:
# cluster.ec
# coph.es <- cophenetic(iris.es)
# # compute cophenetic coefficients CPCC
# es <- cor(iris.e, coph.es)

data <- cbind(perc.out_NY_city, perc.out_NY_state)
iris.e <- dist(data, method='euclidean')
iris.ea <- hclust(iris.e, method='average') #average linkage
iris.ec <- hclust(iris.e, method='complete')
iris.ew <- hclust(iris.e, method='ward.D2')

names(iris.es)

# plot of the dendrograms
x11()
# par(mfrow=c(1,3))
plot(iris.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(iris.ea, k=2)
rect.hclust(iris.ea, k=3)

# plot of the dendrograms
x11()
# par(mfrow=c(1,3))
plot(iris.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(iris.ec, k=2)
rect.hclust(iris.ec, k=3)

# plot of the dendrograms
x11()
# par(mfrow=c(1,3))
plot(iris.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(iris.ew, k=2)
rect.hclust(iris.ew, k=3)

cluster.ea2 <- cutree(iris.ea, k=2)
cluster.ea3 <- cutree(iris.ea, k=3)
coph.ea <- cophenetic(iris.ea)
# compute cophenetic coefficients CPCC
ea <- cor(iris.e, coph.ea)

cluster.ec2 <- cutree(iris.ec, k=2)
cluster.ec3 <- cutree(iris.ec, k=3)
coph.ec <- cophenetic(iris.ec)
# compute cophenetic coefficients CPCC
ec <- cor(iris.e, coph.ec)

cluster.ew2 <- cutree(iris.ew, k=2)
cluster.ew3 <- cutree(iris.ew, k=3)
coph.ew <- cophenetic(iris.ew)
# compute cophenetic coefficients CPCC
ew <- cor(iris.e, coph.ew)

cophenetic_coefficient <- data.frame(Eucl_Compl = ec, Eucl_Av = ea, Eucl_Ward = ew)

setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/ANALISI SPAZIALE CON FIUME/analysis home")
#plot classificati
#EUCLEDIAN-COMPLETE
#k=2
cluster.ec2
my_col = rep('blue', 1092)
my_col[which(cluster.ec2 == 1)] = 'red'
#x11()
png(file = "EC k2.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill = my_col) +
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Eucledian-Complete k = 2')
dev.off()

png(file = "dati EC k2.png")
plot(perc.out_NY_city, perc.out_NY_state, col =my_col)
dev.off()
#k=3
cluster.ec3
my_col = rep('green', 1092)
my_col[which(cluster.ec3 == 2)] = 'blue'
my_col[which(cluster.ec3 == 3)] = 'red'
#x11()
png(file = "EC k3.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill = my_col) +
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Eucledian-Complete k = 3') #con k=3 evidenza quelli che penso siano punti di attracco di barche ...
dev.off()
png(file = "dati EC k3.png")
plot(perc.out_NY_city, perc.out_NY_state, col =my_col)
dev.off()
#EUCLEDIAN-AVERAGE
#k=2
cluster.ec3
my_col = rep('red', 1092)
my_col[which(cluster.ec2 == 2)] = 'blue'
#x11()
png(file = "EA k2.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill = my_col) +
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Map of the Clustering')
dev.off()
png(file = "dati EA k2.png")
plot(perc.out_NY_city, perc.out_NY_state, col =my_col, xlab="percentage of home outside Manhattan",
     ylab="percentage of home outside New Yorl State", main="Eucledian-Average with k=2")
dev.off()
#k=3
cluster.ea2
my_col = rep('green', 1092)
my_col[which(cluster.ea3 == 2)] = 'red'
my_col[which(cluster.ea3 == 3)] = 'blue'

# x11()
png(file = "EA k3.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill = my_col) +
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Eucledian-Average k = 3') #con k=3 evidenza quelli che penso siano punti di attracco di barche ...
dev.off()
png(file = "dati EA k3.png")
plot(perc.out_NY_city, perc.out_NY_state, col =my_col)
dev.off()
#EUCLEDIAN-WARD
#k=2
cluster.ew2
my_col = rep('red', 1092)
my_col[which(cluster.ew2 == 2)] = 'blue'
#x11()
png(file = "EW k2.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill = my_col) +
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Eucledian-Ward k = 2')
sdev.off()
png(file = "dati EW k2.png")
plot(perc.out_NY_city, perc.out_NY_state, col =my_col)
dev.off()
#k=3
cluster.ew2
my_col = rep('red', 1092)
my_col[which(cluster.ew3 == 2)] = 'blue'
my_col[which(cluster.ew3 == 3)] = 'green'

#x11()
png(file = "EW k3.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry, fill = my_col) +
  geom_sf(data = CBG_RIVER$geometry, fill="lightblue")+
  labs(title='Eucledian-Ward k = 3') #terza classe decisamente più randomica a livello spaziale
dev.off()
png(file = "dati EW k3.png")
plot(perc.out_NY_city, perc.out_NY_state, col =my_col)
dev.off()
################### guardiamo le diverse fasce orarie??? ###############