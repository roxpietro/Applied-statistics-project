# Loading 
library(car)
library(geosphere)
library(sf)

# fra
#load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA 

# terri
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Data frame county/New York County.RData")
New_York_County<-New_York_County[-92,]
attach(New_York_County)
n <- dim(New_York_County)[1];

#Y = median_dwell
# raw_stop_counts in LM_1
# raw_device_counts in LM_1
# distance_from_home in LM_1
# distance_from_primary_daytime_location in LM_1
# LM_2 power tranformation ...

# stops_by_day -> utile da capire che info estarre(max, min, mean, diff)
mean_stops_by_day <- c();
max_stops_by_day <- c();
min_stops_by_day <- c();
for (i in 1:n){
  mean_stops_by_day[i] <- mean(stops_by_day[[i]]);
  max_stops_by_day[i] <- max(stops_by_day[[i]]);
  min_stops_by_day[i] <- min(stops_by_day[[i]]); #mh non credo servirà
}
# stops_by_each_hour -> utile da capire che info estrarre
mean_stops_by_each_hour <- c();
max_stops_by_each_hour <- c();
min_stops_by_each_hour <- c();
for (i in 1:n){
  mean_stops_by_each_hour[i] <- mean(stops_by_each_hour[[i]]);
  max_stops_by_each_hour[i] <- max(stops_by_each_hour[[i]]);
  min_stops_by_each_hour[i] <- min(stops_by_each_hour[[i]]); #mh non credo servirà
}

aa <- stops_by_each_hour[[1]];
x11()
for(i in 1:30){
  plot(aa[((i*24)-23):(i*24)], col = i, type="l"); 
  par(new=TRUE)
}

# device_home_areas -> possibili info da estrarre(quante persone singole->sum, 
# weekday_device_home_areas    da quanti posti diversi home diverse->count, differenza tra qst,...)
# weekend_device_home_areas
count_home_areas <- rep(0,n);
for (i in 1:n){
  count_home_areas[i] <- length(device_home_areas[[i]]);
}

count_weekday_home_areas <- rep(0,n);
for (i in 1:n){
  count_weekday_home_areas[i] <- length(weekday_device_home_areas[[i]]);
}

count_weekend_home_areas <- rep(0,n);
for (i in 1:n){
  count_weekend_home_areas[i] <- length(weekend_device_home_areas[[i]]);
}

load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Cyber_Capital.RData")
attach(census_blocks_ny)
all_homes <- c();
for(i in 1:n){
  all_homes <- append(all_homes,names(device_home_areas[[i]]));
  all_homes <- unique(all_homes);
}
index_homes <- c(); 
index_home_problematic <- c();#PROBLEMI > all_homes[15] [1] "120990074181"s  > all_homes[17] [1] "340030031005"
for(i in 1:length(all_homes)){
  j <- which(CensusBlockGroup == all_homes[i]);
  if(length(j) != 0){
    index_homes <-append(index_homes,j);
  }
  else{
    index_home_problematic <- append(index_home_problematic, i); #-> sono 10388 !!!
  }
}

x11()
#plot(st_geometry($geometry), xlim = c(-74.1,-73.8), ylim = c(40.68,40.88), xlab = " ", ylab = " ")
#par(new=T)
plot(st_geometry(census_blocks_ny$geometry[index_homes]), xlab = " ", ylab = " ",col = "red")

# breakfast_device_home_areas ->persone singole nelle fasce (ci sono le SUM), da quanti diversi luoghi
# lunch_device_home_areas
# afternoon_tea_device_home_areas
# dinner_device_home_areas
# nightlife_device_home_areas

count_breakfast_home_areas <- rep(0,n);
for (i in 1:n){
  count_breakfast_home_areas[i] <- length(breakfast_device_home_areas[[i]]);
}
count_lunch_home_areas <- rep(0,n);
for (i in 1:n){
  count_lunch_home_areas[i] <- length(lunch_device_home_areas[[i]]);
}
count_afternoon_home_areas <- rep(0,n);
for (i in 1:n){
  count_afternoon_home_areas[i] <- length(afternoon_tea_device_home_areas[[i]]);
}
count_dinner_home_areas <- rep(0,n);
for (i in 1:n){
  count_dinner_home_areas[i] <- length(dinner_device_home_areas[[i]]);
}
count_nightlife_home_areas <- rep(0,n);
for (i in 1:n){
  count_nightlife_home_areas[i] <- length(nightlife_device_home_areas[[i]]);
}
# work_hours_device_home_areas
count_work_hours_home_areas <- rep(0,n);
for (i in 1:n){
  count_work_hours_home_areas[i] <- length(work_hours_device_home_areas[[i]]);
}
# work_behavior_device_home_areas: device che sono stati in quest'area per almeno 6 ore -> mh .... interesting
count_work_behavior_home_areas <- rep(0,n);
for (i in 1:n){
  count_work_behavior_home_areas[i] <- length(work_behavior_device_home_areas[[i]]);
}
#MH NON ESISTE

#LM_casuale = data.frame(median_dwell, max_stops_by_day, mean_stops_by_day, min_stops_by_day,
#                                      max_stops_by_each_hour,mean_stops_by_each_hour, min_stops_by_each_hour);
LM_casuale = data.frame(median_dwell, sum_device_home_areas, sum_weekday_device_home_areas);

LM_casuale = data.frame(median_dwell, count_home_areas, count_weekday_home_areas);

x11()
pairs(LM_casuale)

lambda_multivariate <- powerTransform(cbind(count_home_areas, count_weekday_home_areas, median_dwell));
lambda_multivariate


# device_daytime_areas  (troppo simile a prima, ecludo a prescindere)
# top_same_month_brand, brand cose ... -> direi NO
# popularity_by_hour_monday ecc. ->too much ...


detach(New_York_County)
