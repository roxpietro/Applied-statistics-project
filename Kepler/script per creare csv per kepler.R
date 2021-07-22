library(geosphere)
library(sf)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(ggplot2)
library(raster)
library(rgdal)

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Complete_dataset.RData")
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

k=which(complete_dataset$area=="360610031001")
k=which(complete_dataset$area=="360610143001")



out1 <- complete_dataset[k,]


#out 1: 92 quello  grande vicino a Brookyln Bridge
home <- names(out1$device_home_areas[[1]])
dev_home<-out1$device_home_areas[[1]]
index_home <- c()
rem<-c()
tot <- 0 #già si vede che su 4892 cbg home diverse ne trova nello stato di NY 3125
for (i in 1: length(home)){
  k <- which(complete_dataset$CensusBlockGroup ==home[i])
  if (length(k) != 0){
    tot <- tot+1
    index_home[tot] <-k
  }
  else{
    rem<-append(rem, i)
  }
}
dev_home<-dev_home[-rem]

coord_ciccione.x<-rep(coord.x_long[k],length(dev_home))
coord_ciccione.y<-rep(coord.y_lat[k],length(dev_home))
plot<-census_blocks_ny$geometry[index_home]
data<-data.frame(arrivo_lng=coord_ciccione.x, arrivo_lat=coord_ciccione.y,
                 partenza_lng= coord.x_long[index_home], partenza_lat = coord.y_lat[index_home], 
                 dev_home)





# csv
write.table(data, "data.csv", 
            sep = ",",             # punto e virgola
            row.names = FALSE,     # se abbiamo la variabile ID
            dec = ".",             # separatore di decimali
            na = "",               # dati mancanti come celle vuote
            quote = TRUE
)

# csv
write.table(plot, "plot.csv", 
            sep = ";",             # punto e virgola
            row.names = FALSE,     # se abbiamo la variabile ID
            dec = ".",             # separatore di decimali
            na = "",               # dati mancanti come celle vuote
            quote = TRUE
)
