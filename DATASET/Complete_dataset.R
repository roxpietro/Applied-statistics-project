##### UNIQUE DATASET ####



load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
rm(patterns_ny)
rm(census_metadata)
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Patterns_NY.RData")



Patterns_NY=Patterns_NY[order(Patterns_NY$area),]
census_blocks_ny = census_blocks_ny[order(census_blocks_ny$CensusBlockGroup),]

# make the two datasets equal
remove=c()
k=1
for (i in 1:dim(census_blocks_ny)[1]) {
  index=which(Patterns_NY$area==census_blocks_ny$CensusBlockGroup[i])
  if (length(index)==0) {
    remove[k]=i
    k=k+1
  }
}

census_blocks_ny=census_blocks_ny[-remove,]

complete_dataset <- data.frame(Patterns_NY, census_blocks_ny)
save(complete_dataset,file="Complete_dataset.RData")
