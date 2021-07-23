## CLUSTERING
setwd("C:/Users/roxpi/Desktop/R_directory/applied stat/Applied-statistics-project/CLASSIFICAZIONE/Brands/lomba")

#libraries
library(rgl)
library(mvtnorm)


## DATASET
load("NYC_no_river.RData")
load("top_category.Rdata")

#rimuovo gli outliers/leverages  (righe 213,247,712,918)
lev = c(213,247,712,918)
new_dataset_withoutlier <- New_York_County_no_river[-lev,]
rm(New_York_County_no_river)


#rimuovo i cbg che non sono fast food o coffee
index = which(top_category != "Coffee" & top_category != "Fast Food")
new_dataset <- new_dataset_withoutlier[-index,]
rm(new_dataset_withoutlier)
attach(new_dataset)



## CREAZIONE FEATURES --> usare max 2 o 3 per avere un plot in 2 o 3 dimensioni


# proviamo a usare la pca. devo crearmi un nuovo dataset con solo le variabili che volgio utilizzare. 

dataset_pca = data.frame(raw_stop_counts, raw_device_counts, distance_from_home, sum_stops_by_day, sum_afternoon,sum_breakfast, sum_dinner, sum_lunch, sum_night, sum_device_daytime)

pca <- princomp(dataset_pca, scores=T)
pca
summary(pca)

load <- pca$loadings
load
load[,1:8]

scores <- pca$scores
scores




#provo il caso più generale con sum_breakfast, sum_lunch e sum_dinner
tre_sum <- data.frame(
  scores[,1],
  scores[,2] 
)



## HIERACHICAL CLUSTERING
x.e <- dist(tre_sum, method='manhattan')
x.ew <- hclust(x.e, method='ward.D2')
x11()
plot(x.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(x.ew, k=2)
cluster.ew <- cutree(x.ew, k=2)

top_category_new <- top_category[-index]
top_category_new <- top_category_new[-lev] #DA FARE SOLO SE HO TOLTO I LEVERAGES ANCHE PRIMA

coffee_index <- which(top_category_new == 'Coffee')
fast_food_index <- which(top_category_new == 'Fast Food')



x11()
#plot(x_new, col=ifelse(cluster.ew==1,'red','blue'), pch=19)
plot(tre_sum[coffee_index,], col = 'red')
points(tre_sum[fast_food_index,], col = 'blue')



