###################################
### Find the river of Manhattan ###
###################################

library(geosphere)
library(sf)

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA 
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
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

# TROVIAMO I CBG DEI FIUMI

track_code_river <- c("027500", "025500", "024700", "024100", "023700", "023300", "022900", "022500", "022301", "021900","021100","020500", "019900", "019500",
                      "019100", "018700", "018300", "017900", "017500", "017100","016700", "016300", "015900", "015500", "015100", "013500", "012900", "011700", "009900", "007900", "007500", "006900", "003700",
                      "003900", "031703", "031704","031900", "000500", "000900","000700", "001502", "001501", "002500", "000800","000600","000201", "000202", "001001", "001002", "002000", "002400", "004400",
                      "006000", "006200", "008601", "008602", "023801", "010601","010602","011600", "012400", "013200", "013600", "023802", "015200","017800", "019200", "024200", "021000", "023600", "024302", "031100",
                      "029900", "029700", "028700", "022302", "016200", "024000")

index_river=c()
k=1
for (i in 1:dim(CBG_ny)[1]) {
  if (CBG_ny$TractCode[i] %in% track_code_river ) {
    if (CBG_ny$BlockGroup[i] == "0") {
      index_river[k] = i
      k=k+1
    }
  }
}
CBG_RIVER<-CBG_ny[index_river,]


x11()
plot(st_geometry(CBG_ny$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
par(new=T)
plot(st_geometry(CBG_RIVER$geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = "lightblue")

save (CBG_RIVER, file="River_Dataset.RData")
