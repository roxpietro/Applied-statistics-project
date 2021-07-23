# tolti alcuni valori alti che sballavano tutto (righe 213,247,712,918)
# creazione x_new su cui fare clustering

# x_new <- data.frame(
#   log(New_York_County_no_river$distance_from_home[c(1:212,214:246,248:711,713:917,919:1092)]),
#   freq_top_brand[c(1:212,214:246,248:711,713:917,919:1092)]
# )



x_new <- data.frame(
   log(New_York_County_no_river$distance_from_home),
   freq_top_brand
 )



#plot per capire l'andamento delle due variabili in x_new
x11()
plot(log(New_York_County_no_river$distance_from_home[c(1:212,214:246,248:711,713:917,919:1092)]), freq_top_brand[c(1:212,214:246,248:711,713:917,919:1092)])

#clustering molto casuale

x.e <- dist(x_new, method='euclidean')
x.ew <- hclust(x.e, method='ward.D2')
x11()
plot(x.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(x.ew, k=2)
cluster.ew <- cutree(x.ew, k=2)

top_category_new <- top_category
coffee_index <- which(top_category_new == 'Coffee')
fast_food_index <- which(top_category_new == 'Fast Food')


x11()
#plot(x_new, col=ifelse(cluster.ew==1,'red','blue'), pch=19)
plot(x_new[coffee_index,], col = 'black')
points(x_new[fast_food_index,], col = 'yellow')

i1 <- which(cluster.ew == 1)
i2 <- which(cluster.ew == 2)

# plot sulla mappa di manhattan dei cluster

x11()
ggplot() + 
  geom_sf(data = CBG_ny_no_river$geometry) +
  geom_sf(data = CBG_ny_no_river$geometry[i1], fill="red") +
  geom_sf(data = CBG_RIVER$geometry, fill = "lightblue") +ggtitle("CBG more frequented during 21.00-24.00")
