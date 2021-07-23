library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(ggplot2)
library(raster)
library(rgdal)
library(RColorBrewer)

load("C:/Users/roxpi/Desktop/R_directory/applied stat/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/roxpi/Desktop/R_directory/applied stat/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("C:/Users/roxpi/Desktop/R_directory/applied stat/Applied-statistics-project/DATASET/River_Dataset.RData")


New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

attach(New_York_County_no_river)


#BREAKFAST

rem_b <- which(sum_breakfast > 3000) 
CBG_ny_no_river$CensusBlockGroup[rem_b] #"360610031001" "360610143001"
col_b <-brewer.pal(n = 11, name = 'Greens'); 


Breakfast = New_York_County_no_river$sum_breakfast[-rem_b]
x11()
#png(file = "glop distance from home.png")
ggplot() + 
geom_sf(data = CBG_ny_no_river$geometry[-rem_b], aes(fill=Breakfast))+scale_fill_gradient(low=col_b[2], high="forestgreen")



#LUNCH 
rem_l <- which(sum_lunch > 3000) 
CBG_ny_no_river$CensusBlockGroup[rem_l] #"360610031001" "360610143001"

col_l <-brewer.pal(n = 11, name = 'PiYG'); 

Lunch = New_York_County_no_river$sum_lunch[-rem_l]
x11()
#png(file = "glop distance from home.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-rem_l], aes(fill=Lunch))+scale_fill_gradient(low=col_l[5], high=col_l[1]) 



#AFTERNOON 
rem_a <- which(sum_afternoon > 3000) 
CBG_ny_no_river$CensusBlockGroup[rem_a] # "360610031001" "360610143001"

col_a <-brewer.pal(n = 7, name = 'Oranges'); 

Afternoon = New_York_County_no_river$sum_afternoon[-rem_a]
x11()
#png(file = "glop distance from home.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-rem_a], aes(fill=Afternoon))+scale_fill_gradient(low=col_a[1], high=col_a[6])


#DINNER
rem_d <- which(sum_dinner > 3000) 
CBG_ny_no_river$CensusBlockGroup[rem_d] # "360610031001" "360610143001"

col_d <-brewer.pal(n = 7, name = 'YlOrBr');
Dinner = New_York_County_no_river$sum_dinner[-rem_d]
x11()
#png(file = "glop distance from home.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-rem_d], aes(fill=Dinner))+scale_fill_gradient(low=col_d[1], high="gold4")


#NIGHT
rem_n <- which(sum_night > 2000) 
CBG_ny_no_river$CensusBlockGroup[rem_n] # "360610031001" "360610143001"

col_n <-brewer.pal(n = 7, name = 'Blues'); 

Night = New_York_County_no_river$sum_night[-rem_n]
x11()
#png(file = "glop distance from home.png")
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry[-rem_n], aes(fill=Night))+scale_fill_gradient(low=col_n[1], high=col_n[7])
