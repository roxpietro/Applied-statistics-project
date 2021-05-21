#################################
#### CYBER CONVERTER DATASET ####
#################################

# Set Working Directory
setwd("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto")

# Load Dataset
load("Cyber_Capital.RData")

##################### RIMOZIONE DELLE RIGHE NA ######################
# Attach names
attach(patterns_ny)

#righe da eliminare dal dataset originale:
# - 189 -> 360050019005
# - 841 -> 360470056010
# - 1144 -> 360470168000
# - 1145 -> 360470916003
# - 1324 -> 360470054000
# - 2301 -> 360050319000
# - 2313 -> 360850128050
# - 2790 -> 360470280000
# - 3979 -> 360810964000
# - 4434 -> 360610183000
# - 4617 -> 360470292000
# - 7407 -> 360850228000
# - 8301 -> 360050115020
# - 9685 -> 360639401001
# - 13037 -> 360610179000

removed = c(189,841,1144,1145,1324,2301,2313,2790,3979,4434,4617,7407,8301,9685,13037)
Patterns_NY <- patterns_ny[-removed,]

detach(patterns_ny)

attach(Patterns_NY)

n=length(stops_by_day)
names<-c(1:n)
rownames(Patterns_NY)<-names


save(Patterns_NY, file = "Patterns_NY.RData")

detach(Patterns_NY)

##################################### CONVERSIONE DEL DATASET #####################################################
rm(list= ls())
load("Patterns_NY.RData")

#add coloumns for names
v = colnames(Patterns_NY)
Patterns_NY$day_counts_name<- c(1)
index = which(v == "day_counts")
v = append(v, "day_counts_name", index)
Patterns_NY <- Patterns_NY[v] 

v = colnames(Patterns_NY)
Patterns_NY$top_same_day_brand_name <- c(1)
index = which(v == "top_same_day_brand")
v = append(v, "top_same_day_brand_name", index)
Patterns_NY <- Patterns_NY[v] 

v = colnames(Patterns_NY)
Patterns_NY$top_same_month_brand_name <- c(1)
index = which(v == "top_same_month_brand")
v = append(v, "top_same_month_brand_name", index)
Patterns_NY <- Patterns_NY[v] 

v = colnames(Patterns_NY)
Patterns_NY$device_type_name <- c(1)
index = which(v == "device_type")
v = append(v, "device_type_name", index)
Patterns_NY <- Patterns_NY[v] 


# Make the dataset readable
library(stringr)
attach(Patterns_NY)
n=length(stops_by_day)

for (i in 1:n) {
  
  #neglect first and last character []
  stops_days<-str_sub(stops_by_day[i], 2, -2)
  stops_hours<-str_sub(stops_by_each_hour[i], 2, -2)
  pop_mon<-str_sub(popularity_by_hour_monday[i], 2, -2)
  pop_tue<-str_sub(popularity_by_hour_tuesday[i], 2, -2)
  pop_wed<-str_sub(popularity_by_hour_wednesday[i], 2, -2)
  pop_thr<-str_sub(popularity_by_hour_thursday[i], 2, -2)
  pop_fri<-str_sub(popularity_by_hour_friday[i], 2, -2)
  pop_sat<-str_sub(popularity_by_hour_saturday[i], 2, -2)
  pop_sun<-str_sub(popularity_by_hour_sunday[i], 2, -2)
  
  
  dev_h_a<-str_sub(device_home_areas[i], 2, -2)
  week<-str_sub(weekday_device_home_areas[i], 2, -2)
  week_end<-str_sub(weekend_device_home_areas[i], 2, -2)
  
  breakfast<-str_sub(breakfast_device_home_areas[i], 2, -2)
  lunch<-str_sub(lunch_device_home_areas[i], 2, -2)
  tea<-str_sub(afternoon_tea_device_home_areas[i], 2, -2)
  dinner<-str_sub(dinner_device_home_areas[i], 2, -2)
  night<-str_sub(nightlife_device_home_areas[i], 2, -2)
  work<-str_sub(work_hours_device_home_areas[i], 2, -2)
  daytime<-str_sub(device_daytime_areas[i], 2, -2)
  
  day<-str_sub(day_counts[i], 2, -2)
  day_brand<-str_sub(top_same_day_brand[i], 2, -2)
  month_brand<-str_sub(top_same_month_brand[i], 2, -2)
  device<-str_sub(device_type[i], 2, -2)
  
  
  #neglect \" 
  dev_h_a<-gsub('\"', "", dev_h_a, fixed = TRUE)
  week<-gsub('\"', "", week, fixed = TRUE)
  week_end<-gsub('\"', "", week_end, fixed = TRUE)
  
  breakfast<-gsub('\"', "", breakfast, fixed = TRUE)
  lunch<-gsub('\"', "", lunch, fixed = TRUE)
  tea<-gsub('\"', "", tea, fixed = TRUE)
  dinner<-gsub('\"', "", dinner, fixed = TRUE)
  night<-gsub('\"', "", night, fixed = TRUE)
  work<-gsub('\"', "", work, fixed = TRUE)
  daytime<-gsub('\"', "", daytime, fixed = TRUE)
  
  day<-gsub('\"', "", day, fixed = TRUE)
  day_brand<-gsub('\"', "", day_brand, fixed = TRUE)
  month_brand<-gsub('\"', "", month_brand, fixed = TRUE)
  device<-gsub('\"', "", device, fixed = TRUE)
  
  #split the string
  stops_days<-unlist(strsplit(stops_days, split=","))
  stops_hours<-unlist(strsplit(stops_hours, split=","))
  pop_mon<-unlist(strsplit(pop_mon, split=","))
  pop_tue<-unlist(strsplit(pop_tue, split=","))
  pop_wed<-unlist(strsplit(pop_wed, split=","))
  pop_thr<-unlist(strsplit(pop_thr, split=","))
  pop_fri<-unlist(strsplit(pop_fri, split=","))
  pop_sat<-unlist(strsplit(pop_sat, split=","))
  pop_sun<-unlist(strsplit(pop_sun, split=","))
  
  dev_h_a<-unlist(strsplit(dev_h_a, split = ","))
  dev_h_a<-unlist(strsplit(dev_h_a, split = ":"))
  
  week<-unlist(strsplit(week, split = ","))
  week<-unlist(strsplit(week, split = ":"))
  week_end<-unlist(strsplit(week_end, split = ","))
  week_end<-unlist(strsplit(week_end, split = ":"))
  breakfast<-unlist(strsplit(breakfast, split = ","))
  breakfast<-unlist(strsplit(breakfast, split = ":"))
  lunch<-unlist(strsplit(lunch, split = ","))
  lunch<-unlist(strsplit(lunch, split = ":"))
  tea<-unlist(strsplit(tea, split = ","))
  tea<-unlist(strsplit(tea, split = ":"))
  dinner<-unlist(strsplit(dinner, split = ","))
  dinner<-unlist(strsplit(dinner, split = ":"))
  night<-unlist(strsplit(night, split = ","))
  night<-unlist(strsplit(night, split = ":"))
  work<-unlist(strsplit(work, split = ","))
  work<-unlist(strsplit(work, split = ":"))
  daytime<-unlist(strsplit(daytime, split = ","))
  daytime<-unlist(strsplit(daytime, split = ":"))
  
  
  day<-unlist(strsplit(day, split = ","))
  day<-unlist(strsplit(day, split = ":"))
  day_brand<-unlist(strsplit(day_brand, split = ","))
  day_brand<-unlist(strsplit(day_brand, split = ":"))
  month_brand<-unlist(strsplit(month_brand, split = ","))
  month_brand<-unlist(strsplit(month_brand, split = ":"))
  device<-unlist(strsplit(device, split = ","))
  device<-unlist(strsplit(device, split = ":"))
  
  #convert to int
  Patterns_NY$stops_by_day[i]<-list(strtoi(stops_days))
  Patterns_NY$stops_by_each_hour[i]<-list(strtoi(stops_hours))
  
  Patterns_NY$popularity_by_hour_monday[i]<-list(strtoi(pop_mon))
  Patterns_NY$popularity_by_hour_tuesday[i]<-list(strtoi(pop_tue))
  Patterns_NY$popularity_by_hour_wednesday[i]<-list(strtoi(pop_wed))
  Patterns_NY$popularity_by_hour_thursday[i]<-list(strtoi(pop_thr))
  Patterns_NY$popularity_by_hour_friday[i]<-list(strtoi(pop_fri))
  Patterns_NY$popularity_by_hour_saturday[i]<-list(strtoi(pop_sat))
  Patterns_NY$popularity_by_hour_sunday[i]<-list(strtoi(pop_sun))
  
  Patterns_NY$device_home_areas[i]<-list(as.double(dev_h_a))
  Patterns_NY$weekday_device_home_areas[i]<-list(as.double(week))
  Patterns_NY$weekend_device_home_areas[i]<-list(as.double(week_end))
  
  
  Patterns_NY$breakfast_device_home_areas[i]<-list(as.double(breakfast))
  Patterns_NY$lunch_device_home_areas[i]<-list(as.double(lunch))
  Patterns_NY$afternoon_tea_device_home_areas[i]<-list(as.double(tea))
  Patterns_NY$dinner_device_home_areas[i]<-list(as.double(dinner))
  Patterns_NY$nightlife_device_home_areas[i]<-list(as.double(night))
  Patterns_NY$work_hours_device_home_areas[i]<-list(as.double(work))
  Patterns_NY$device_daytime_areas[i]<-list(as.double(daytime))
  
  
  
  Patterns_NY$day_counts[i]<-list(as.double(day[seq(2,length(day),by=2)]))
  Patterns_NY$day_counts_name[i]<-list(day[seq(1,length(day),by=2)])
  
  Patterns_NY$top_same_day_brand[i]<-list(as.double(day_brand[seq(2,length(day_brand),by=2)]))
  Patterns_NY$top_same_day_brand_name[i]<-list(as.character(day_brand[seq(1,length(day_brand),by=2)]))
  
  Patterns_NY$top_same_month_brand[i]<-list(as.double(month_brand[seq(2,length(month_brand),by=2)]))
  Patterns_NY$top_same_month_brand_name[i]<-list(month_brand[seq(1,length(month_brand),by=2)])
  Patterns_NY$device_type[i]<-list(as.double(device[seq(2,length(device),by=2)]))
  Patterns_NY$device_type_name[i]<-list(device[seq(1,length(device),by=2)])
  
}

detach(Patterns_NY)

####################################

### risolviamo gli errori 
attach(patterns_ny)

brand_days <- Patterns_NY$top_same_day_brand
CBG <- Patterns_NY$area
for (i in 1:n){
  if (sum(is.na(brand_days[[i]])) >0)
    print(CBG[i])
}

#[1] "360610106021"
#[1] "360290128001"
#[1] "360290149033"
#[1] "360550144005"
#[1] "360070135002"
#[1] "360894904001"
#[1] "361190077003"
#[1] "361190064001"



ind=which(Patterns_NY$area== "360610106021")
aa<-str_sub(top_same_day_brand[ind], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa<-"1 Hotels:10,D'Agostino:6,Fairfield Inn & Suites by Marriott:5,Community Health Centers Inc:5,Pollo Campero:5,Total Wine & More:5,Fox Rent A Car:5,Cumberland Farms:5,Pacific Sales:5,Baskin Robbins:5,Popeyes Louisiana Kitchen:5,Wegmans Food Markets:5,Haagen Dazs:5,Extra Space Storage:5,Gulf Oil:5,Bond No. 9:5,McDonald's:5,Forever 21:5,ShopRite:5,Westlake Hardware:5"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))


#--------------------------------------------------------------------------------------

ind=which(Patterns_NY$area== "360290128001")
aa<-str_sub(top_same_day_brand[which(area=="360290128001")], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa<-"Tim Hortons:16,7-Eleven:5,United States Postal Service (USPS):4,Marathon:4,Walmart:3,Sunoco:3,Target:2,Olive Garden:2,ALDI:2,The Home Depot:2,Speedway:2,Fisher Auto Parts:2,Dollar General:2,True Value Hardware:2,Tops Friendly Markets:2,Mobil:2,Samuel Son & Co.:2,Wegmans Food Markets:2,A.C. Moore:2,Rite Aid:2"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
list(as.double(aa[seq(2,length(aa),by=2)]))
list(as.character(aa[seq(1,length(aa),by=2)]))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))

#--------------------------------------------------------------------------------------

ind=which(Patterns_NY$area== "360290149033")
aa<-str_sub(top_same_day_brand[which(area=="360290149033")], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa<-"7-Eleven:3,Tim Hortons:3,Walmart:2,Tops Friendly Markets:2,McDonald's:2,Honda:1,Target:1,Olive Garden:1,Lowe's:1,Restaurant Depot:1,T.J. Maxx:1,The Home Depot:1,Speedway:1,Dollar General:1,Mobil:1,Fastrac:1,Moe's Southwest Grill:1,Staples:1,Samuel Son & Co.:1,Kohl's:1"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
list(as.double(aa[seq(2,length(aa),by=2)]))
list(as.character(aa[seq(1,length(aa),by=2)]))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))

#--------------------------------------------------------------------------------------

ind=which(Patterns_NY$area== "360550144005")
aa<-str_sub(top_same_day_brand[which(area=="360550144005")], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa<-"Wendy's:9,Speedway:8,Walmart:6,Wegmans Food Markets:6,The Home Depot:5,Panera Bread:5,Burger King:5,McDonald's:4,The Salvation Army:3,Samuel Son & Co.:3,Dollar Tree:3,LINE-X:2,GameStop:2,True Value Hardware:2,Starbucks:2,Dunn Tire:2,Tim Hortons:2,Kwik Fill:2,Sunoco:2,Dunkin':2"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
list(as.double(aa[seq(2,length(aa),by=2)]))
list(as.character(aa[seq(1,length(aa),by=2)]))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))

#--------------------------------------------------------------------------------------

ind=which(Patterns_NY$area== "360070135002")
aa<-str_sub(top_same_day_brand[which(area=="360070135002")], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa<-"Wendy's:12,McDonald's:6,Walmart:5,Speedway:5,Price Chopper New England:4,Dunkin':4,Weis Markets:3,Domino's Pizza:3,Lowe's:3,United States Postal Service (USPS):3,CVS:3,Target:2,M&T Bank:2,The Home Depot:2,Dollar General:2,Mirabito Energy Products:2,Wegmans Food Markets:2,Burger King:2,FNB Bank N.A.:2,Blaze Pizza:2"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
list(as.double(aa[seq(2,length(aa),by=2)]))
list(as.character(aa[seq(1,length(aa),by=2)]))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))


#--------------------------------------------------------------------------------------

ind=which(Patterns_NY$area== "360894904001")
aa<-str_sub(top_same_day_brand[which(area=="360894904001")], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa
aa<-"Walmart:8,The Cat Rental Store:5,Dollar General:4,Mobil:4,Circle K:4,McDonald's:4,Chevrolet:3,Jreck Subs:3,IGA:3,Big M Supermarkets:3,Hobby Lobby Stores:3,True Value Hardware:3,Price Chopper New England:3,Fastrac:3,Ferrellgas:3,United States Postal Service (USPS):3,Stewart's Shops:3,Sherwin-Williams:3,FNB Bank N.A.:3,Tim Hortons:3"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
list(as.double(aa[seq(2,length(aa),by=2)]))
list(as.character(aa[seq(1,length(aa),by=2)]))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))


#--------------------------------------------------------------------------------------

ind=which(Patterns_NY$area== "361190077003")
aa<-str_sub(top_same_day_brand[which(area=="361190077003")], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa
aa<-"The Cheesecake Factory:3,The Home Depot:3,Mobil:3,Exxon Mobil:3,Western Beef:2,Audi:2,Dodge:2,Fairfield Inn & Suites by Marriott:2,Chrysler:2,Community Health Centers Inc:2,Club Pilates:2,Hannaford Supermarkets:2,The Men's Wearhouse:2,Boys & Girls Clubs of America:2,T.J. Maxx:2,Autopart International:2,SoulCycle:2,Macy's:2,Starbucks:2,Mohegan Sun:2"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
list(as.double(aa[seq(2,length(aa),by=2)]))
list(as.character(aa[seq(1,length(aa),by=2)]))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))

#--------------------------------------------------------------------------------------

ind=which(Patterns_NY$area== "361190064001")
aa<-str_sub(top_same_day_brand[which(area=="361190064001")], 2, -2)
aa<-gsub('\"', "", aa, fixed = TRUE)
aa
aa<-"Dunkin':9,CrossFit:8,Trader Joe's:4,The Home Depot:4,Kohl's:4,Community Health Centers Inc:3,Gap:3,Olive Garden:3,Taco Bell:3,Saks Off Fifth:3,Dollar General:3,Nissan North America:3,Mobil:3,McDonald's:3,CVS:3,CITGO:3,Petco:2,IHOP:2,Western Beef:2,Key Food:2"
aa<-unlist(strsplit(aa, split=","))
aa<-unlist(strsplit(aa, split = ":"))
list(as.double(aa[seq(2,length(aa),by=2)]))
list(as.character(aa[seq(1,length(aa),by=2)]))
Patterns_NY$top_same_day_brand[ind]<-list(as.double(aa[seq(2,length(aa),by=2)]))
Patterns_NY$top_same_day_brand_name[ind]<-list(as.character(aa[seq(1,length(aa),by=2)]))


#new rows to be cancelled because few data available
Patterns_NY <- Patterns_NY[-8029,]

save(Patterns_NY, file = "Patterns_NY.RData")


# Detach names
detach(patterns_ny)
detach(Patterns_NY)
