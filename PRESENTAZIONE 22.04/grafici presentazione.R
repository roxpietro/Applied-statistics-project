#graphical description of Dataset

# Set Working Directory
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto")

# Load Dataset
load("~/Documenti/UNIVERSITA/STAT APP/progetto/Patterns_NY.RData")
load("~/Documenti/UNIVERSITA/STAT APP/progetto/Cyber.RData")

attach(Patterns_NY)
attach(census_blocks_ny)

library(geosphere)
jj <- which(Patterns_NY$area == 360050175002); #CBG nell'img della presentazione
# coords=census_blocks_ny$geometry[jj][[1]][[1]];
# area <- areaPolygon(coords); #PERCHÈ CAZZO NON VA forse   package ‘areaPolygon’ is not available (for R version 3.6.3)
coords <- census_blocks_ny$geometry[[jj]][[1]][[1]];

png(file = "cbgese.png")
x11()
plot(census_blocks_ny$geometry[[jj]][[1]][[1]][,1],census_blocks_ny$geometry[[jj]][[1]][[1]][,2], type = "l", xlab = " ", ylab = " ")
par(new=T)
dev.off()

detach(Patterns_NY)
detach(census_blocks_ny)
esempio <- Patterns_NY[jj,];
attach(esempio)
esempio$date_range_start
weekday_device_home_areas <- esempio$weekday_device_home_areas;
weekday_device_home_areas[[1]]
detach(esempio)
#disegno vicinato
NY_counties <- c('Bronx County', 'New York County', 'Kings County', 'Queens County', 'Richmond County');
bronx_index=which(County==NY_counties[1])
png(file = "bronx.png")
for (i in bronx_index) {
  if (i == jj)
    j = "red"
  else
    j= "grey"
  plot(census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2], type = "l",  xlim = c(-74.5,-73.5), ylim = c(40.25,41), xlab = " ", ylab = " ", col=j);
  par(new=T)
}
title(main = "Bronx NY county", xlab = "Longitude", ylab = "Latitude")
dev.off()

png(file = "zoom_bronx.png")
for (i in bronx_index) {
  if (i == jj)
    j = "red"
  else
    j= "grey"
  plot(census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2], type = "l",xlim = c(-73.97, -73.9), ylim=c(41.58, 41.65), xlab = " ", ylab = " ", col=j);
  par(new=T)
}
title(main = "Bronx NY county", xlab = "Longitude", ylab = "Latitude")
dev.off()

#disegno frecce per dare idea flusso ...
arrivo <- coords;
cbg_start <- names(Patterns_NY$device_home_areas[[jj]]);
partenza <- c(matrix(NA, nrow = dim(coords)[1], ncols = dim(coords)[2]));
for(i in 1: length(cbg_start)){
  j <- match(cbg_start[i],CensusBlockGroup); #estrae gli indici delle cbg in PatternNY corrispondenti ai CBG nel dataframe census_blocks_ny
  partenza[i] <-census_blocks_ny$geometry[[j]][[1]][[1]];
}

x11()
for (i in 1:15463) {
  plot(census_blocks_ny$geometry[[i]][[1]][[1]][,1],census_blocks_ny$geometry[[i]][[1]][[1]][,2], type = "l", xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ")
  par(new=T)
}
title(main = "NY country", xlab = "Longitude", ylab = "Latitude")
