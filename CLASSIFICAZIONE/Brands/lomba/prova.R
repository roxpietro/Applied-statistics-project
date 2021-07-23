indici <- c(8, 9, 22, 23, 24, seq(40,50,1))
for (i in 1:length(indici)){
  x_new <- data.frame(
    log(New_York_County_no_river[,indici[i]]),
    freq_top_brand
  )
  x.e <- dist(x_new, method='euclidean')
  x.ew <- hclust(x.e, method='ward.D2')
  cluster.ew <- cutree(x.ew, k=2)
  i1 <- which(cluster.ew == 1)
  i2 <- which(cluster.ew == 2)
  
  x11()
  ggplot() + 
    geom_sf(data = CBG_ny_no_river$geometry) +
    geom_sf(data = CBG_ny_no_river$geometry[i1], fill="red") +
    geom_sf(data = CBG_RIVER$geometry, fill = "lightblue") +ggtitle("CBG more frequented during 21.00-24.00")
  
}

