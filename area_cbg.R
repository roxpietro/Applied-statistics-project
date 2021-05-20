# Set Working Directory
setwd("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto")

library(geosphere)
library(RColorBrewer)
library("viridis")
colcountyNY <-brewer.pal(n = 5, name = 'Accent'); 

# Load Dataset
load("Cyber_Capital.RData")

attach(census_blocks_ny)


area=matrix(nrow = 1, ncol = 15463)
#par(mfrow=c(4,5))
for (i in 1:15463) {
  #points=census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2]
  coords=census_blocks_ny$geometry[[i]][[1]][[1]]
  area[i]=areaPolygon(coords)
}

x11()
for (i in 1:15463) {
  plot(census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2], type = "l", xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ")
  par(new=T)
  }
title(main = "NY country", xlab = "Longitude", ylab = "Latitude")


NY_counties <- c('Bronx County', 'New York County', 'Kings County', 'Queens County', 'Richmond County');
bronx_index=which(County==NY_counties[1])
ny_index=which(County==NY_counties[2])
kings_index=which(County==NY_counties[3])
queens_index=which(County==NY_counties[4])
richmond_index=which(County==NY_counties[5])

x11()
for (i in c(bronx_index, ny_index, kings_index, queens_index, richmond_index)) {
  if (County[i]==NY_counties[1])
    j=colcountyNY[1]
  if (County[i]==NY_counties[2])
    j=colcountyNY[2]
  if (County[i]==NY_counties[3])
    j=colcountyNY[3]
  if (County[i]==NY_counties[4])
    j=colcountyNY[4]
  if (County[i]==NY_counties[5])
    j=colcountyNY[5]
  plot(census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2], type = "l", xlim = c(-74.5,-73.5), ylim = c(40.25,41), xlab = " ", ylab = " ", col=j)
  par(new=T)
}
title(main = "NY City", xlab = "Longitude", ylab = "Latitude")
legend("topleft", legend=NY_counties, col=colcountyNY,pch = 19, bty = "n")


