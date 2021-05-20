#################### SIGNIFICANT BRANDS #######################

# set working directory data
setwd("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto")

# Load Dataset
load("Patterns_NY.RData")
load("Cyber_Capital.RData")
rm(patterns_ny)
rm(census_metadata)

#libraries
library(geosphere)
library(sf)
library(ggplot2)

ny_index=which(census_blocks_ny$County=="New York County")
ny_index=sort(ny_index)


# take only the first 3 top same month brand for each CBG
top_brand=c()
top_brand_ny=c()
k=1

for (i in 1:dim(Patterns_NY)[1] ){
  if (Patterns_NY$area[i] == Patterns_NY$area[ny_index[k]] && k<1169) {
    for (j in 1:3) {
    top_brand_ny=append(top_brand_ny,Patterns_NY$top_same_month_brand_name [[i]][j])
    top_brand=append(top_brand,Patterns_NY$top_same_month_brand_name [[i]][j]) 
    }
    k=k+1
  }
  else{
  for (j in 1:3) {
    top_brand=append(top_brand,Patterns_NY$top_same_month_brand_name [[i]][j]) 
  }
  }
  
}

# make a copy
top_3_brands=top_brand
top_3_brands_ny=top_brand_ny

# start to build factor
top_brand_original=top_brand
top_brand_original_ny=top_brand_ny


# take only unique brands
top_brand=unique(top_brand)
top_brand_ny=unique(top_brand_ny)


# do factor (LAB1)
top_brand_original <- factor(top_brand_original,levels=top_brand) 
top_brands <- table(top_brand_original) 
top_brands=sort(top_brands,decreasing = TRUE)
last_ones=c(which(top_brands==3),which(top_brands==2), which(top_brands==1))
top_brands=top_brands[-last_ones]

top_brand_original_ny <- factor(top_brand_original_ny,levels=top_brand_ny) 
top_brands_ny <- table(top_brand_original_ny) 
last_ones=c(which(top_brands_ny==3),which(top_brands_ny==2), which(top_brands_ny==1))
top_brands_ny=top_brands_ny[-last_ones]
top_brands_ny=sort(top_brands_ny,decreasing = TRUE)

# build the dataframe
brands<-read.delim(file = "Classificazione brands.txt", header = FALSE, sep=":")


top_brand_ny=top_brand_ny[-last_ones]

vect_brands_ny_classification = c()
vect_freq_brands = c()
for (i in 1:length(top_brand_ny)){
  index=which(brands[,1]==top_brand_ny[i])
    vect_brands_ny_classification[i]= brands[index,2]
    vect_freq_brands[i]=top_brands_ny[top_brand_ny[i]]
}

#--------------------------------------------------------------------------
### NEW YORK BRANDS ###

# new dataframe: BRANDS_MANHATTAN:
# 1) top_brand_month
# 2) frequency
# 3) category_top_brand

BRANDS_MANHATTAN = data.frame(top_brand_ny,vect_freq_brands,rep(vect_brands_ny_classification))
colnames(BRANDS_MANHATTAN)<- c("Names","Frequency","Category")
BRANDS_MANHATTAN <- BRANDS_MANHATTAN[order(BRANDS_MANHATTAN$Frequency, decreasing = TRUE),]
rm(Patterns_NY)
load("New York County.RData")


# order patterns_ny and census_block_ny by CBG of New York County
sub_patt=sub_patt[order(sub_patt$area),]
CBG_ny_index = which(census_blocks_ny$County=="New York County")
CBG_ny = census_blocks_ny[CBG_ny_index,]


# make the two datasets equal
remove=c()
k=1
for (i in 1:1170) {
  index=which(sub_patt$area==CBG_ny$CensusBlockGroup[i])
  if (length(index)==0) {
    remove[k]=i
    k=k+1
  }
}

CBG_ny=CBG_ny[-remove,]
CBG_ny=CBG_ny[order(CBG_ny$CensusBlockGroup),]

CBG_ny_index=CBG_ny_index[-remove]

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(11,"Set3")

cols<- levels(factor(BRANDS_MANHATTAN$Category))



########################################
# library(sf)
# library(ggplot2)
# myColors <- brewer.pal(12,"Set3")
# x11()
# g<-ggplot(census_blocks_ny$geometry ) + xlim(-74.1,-73.8) + ylim(40.68,40.88)
# k=0
# for (i in CBG_ny_index) {
#   k=k+1
#   index_brands=which(BRANDS_MANHATTAN$Names==sub_patt$top_same_month_brand_name[k][[1]][1])
#   
#   if(length(index_brands)!=0) {
#   #assegno il colore
#   col=which(BRANDS_MANHATTAN$Category[index_brands]==cols)
#   g<- g+geom_sf(census_blocks_ny$geometry[i], mapping=aes(), colour="black", fill=myColors[col])
#   #centroid_cbg = centroid(CBG_ny$geometry[[i]][[1]][[1]])
#   #g<-g+ geom_polygon(data=data.frame(x=CBG_ny$geometry[[i]][[1]][[1]][,1],y=CBG_ny$geometry[[i]][[1]][[1]][,2]), aes(x=x, y=y, fill = myColors[col]))
#   #plot(centroid_cbg, xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ", pch="*")
#   #par(new=T)
#   }
# }
# 
# g <- g+labs(x = "Longitude", y = "Latitude") + theme(legend.position="right") 
# #+ scale_color_manual(name = "Categories",breaks = cols,values = c(" autostrade" = myColors[1], " Coffe" = myColors[2]," Convenience store" = myColors[3]," Fast Food" = myColors[4]," Fuel industry company" = myColors[5]," Medical field" = myColors[6]," metropolitana" = myColors[7]," Restaurant company" = myColors[8]," Retail company" = myColors[9], " Supermarket company"= myColors[10]," Variety store company" = myColors[11]),guide = "legend")
# #legenda boh
# g

################################################

# Plot by Categories

x11()
k=0
for (i in CBG_ny_index) {
  k=k+1
  index_brands=which(BRANDS_MANHATTAN$Names==sub_patt$top_same_month_brand_name[k][[1]][1])
  
  #assegno il colore
  col=which(BRANDS_MANHATTAN$Category[index_brands]==cols)
  plot(st_geometry(census_blocks_ny$geometry[i]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = myColors[col])
  par(new=T)
}
title(main = "NY country Categories", xlab = "Longitude", ylab = "Latitude")
legend(legend=cols,"bottomright", fill=myColors)


# Plot by Names

colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors[1]="yellow"
colors[2]="pink"
colors[3]="red"
colors[4]="cyan"
colors[5]="green3"
colors[6]="blue"
x11()
k=0
for (i in CBG_ny_index) {
  k=k+1
  index_brands=which(BRANDS_MANHATTAN$Names==sub_patt$top_same_month_brand_name[k][[1]][1])
  
  #assegno il colore
  #col=which(BRANDS_MANHATTAN$Category[index_brands]==cols)
  plot(st_geometry(census_blocks_ny$geometry[i]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = colors[index_brands])
  par(new=T)
}
title(main = "NY country Names", xlab = "Longitude", ylab = "Latitude")
legend(legend=BRANDS_MANHATTAN$Names[1:6],"bottomright", fill=colors[1:6])


# we see many starbucks, we plot them
library(readr)
starbucks <- read_csv("all_starbucks_locations_nyc.csv")
starbucks <- starbucks[,c(23,24)]
x11()
k=0
for (i in CBG_ny_index) {
  k=k+1
  if("Starbucks"==sub_patt$top_same_month_brand_name[k][[1]][1])
    col="green3"
  else 
    col="white"
  plot(st_geometry(census_blocks_ny$geometry[i]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = col)
  par(new=T)
}
points(starbucks$longitude,starbucks$latitude, xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = "red",pch=20)
title(main = "NY country Names", xlab = "Longitude", ylab = "Latitude")
legend(legend="Starbucks","bottomright", fill="red")

rm(starbucks)


# we see many MCdonald's, we plot them
library(readxl)
McDonalds_Manhattan <- read_excel("McDonalds_Manhattan.xlsx")
McDonalds_Manhattan <- McDonalds_Manhattan[,c(5,6)]

x11()
k=0
for (i in CBG_ny_index) {
  k=k+1
  if("McDonald's"==sub_patt$top_same_month_brand_name[k][[1]][1])
    col="yellow"
  else 
    col="white"
  plot(st_geometry(census_blocks_ny$geometry[i]), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = col)
  par(new=T)
}
points(McDonalds_Manhattan$Longitude_new,McDonalds_Manhattan$Latitude_new, xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ",col = "red",pch=20)
title(main = "NY country Names", xlab = "Longitude", ylab = "Latitude")
legend(legend="McDonald's","bottomright", fill="red")

#---------------------------------------------------------------------------------------------------
### BRANDS ###
brands<-brands[2]

BRANDS = data.frame(top_brands,brands)

colnames(BRANDS)<- c("Names","Frequeny","Category")

attach(BRANDS)

rm(brands)

remove<-c(which(Names=="Fastrac"), which(Names=="Boys & Girls Clubs of America"), which(Names=="Blink Fitness"),
          which(Names=="United States Postal Service (USPS)"), which(Names=="CrossFit"), which(Names=="Advance Auto Parts"),
          which(Names=="SmartStyle Family Hair Salons"), which(Names=="Subway"),which(Names=="Speedway"))

BRANDS<-BRANDS[-remove,]



# Plot by Categories

x11()
k=0
for (i in 1:dim(census_blocks_ny)[1]) {
  k=k+1
  index_brands=which(BRANDS$Names==sub_patt$top_same_month_brand_name[k][[1]][1])
  
  #assegno il colore
  col=which(BRANDS_MANHATTAN$Category[index_brands]==cols)
  plot(st_geometry(census_blocks_ny$geometry[i]), xlim = c(-80,-71), ylim = c(40,45), xlab = " ", ylab = " ",col = myColors[col])
  par(new=T)
}
title(main = "NY country Categories", xlab = "Longitude", ylab = "Latitude")
legend(legend=cols,"bottomright", fill=myColors)




detach(BRANDS)

