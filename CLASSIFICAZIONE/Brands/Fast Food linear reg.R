#######################################################
####### FAST FOOD FREQ - LINEAR REGRESSION ############
#######################################################
library(car)

# set working directory data

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/River_Dataset.RData")

#terri
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")

#libraries
library(geosphere)
library(sf)
library(sp)
library(ggplot2)
library(caret)
library(AICcmodavg)

CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]
New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]

attach(New_York_County_no_river)

# build the dataframe
brands<-read.delim(file = "Classificazione brands.txt", header = FALSE, sep=":")
fast_food <- brands[which(brands[,2] == "Fast Food"),1]

#brand_fast_food <-c()
freq_fast_food <- c()
k <-0
for (i in 1:dim(New_York_County_no_river)[1] ){
  k <- which(top_same_month_brand_name[[i]] %in% fast_food)
  if(length(k)!=0){
    #brand_fast_food=append(brand_fast_food,New_York_County_no_river$top_same_month_brand_name [[i]][k])
    freq_fast_food[i] = sum(top_same_month_brand [[i]][k]) ;
  }
}

BRANDS_NY<-data.frame(area,freq = freq_fast_food, raw_stop_counts, raw_device_counts,
                      #stops_by_day, device_home_areas, breakfast_device_home_areas, lunch_device_home_areas, afternoon_tea_device_home_areas, dinner_device_home_areas, nightlife_device_home_areas, work_hours_device_home_areas,
                      distance_from_home, distance_from_primary_daytime_location, 
                      median_dwell, sum_breakfast, sum_lunch, sum_afternoon, 
                      sum_dinner, sum_night)

detach(New_York_County_no_river)
attach(BRANDS_NY)

model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
            distance_from_primary_daytime_location +  median_dwell +  
            sum_breakfast + sum_lunch + sum_afternoon + sum_dinner+ sum_night)
summary(model)

shapiro.test(model$residuals)
x11()
par(mfrow=c(2,2))
plot(model)
rem <- c(36,92,532)
detach(BRANDS_NY)
BRANDS_NY <- BRANDS_NY[-rem,]
attach(BRANDS_NY)
length(freq)
model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  median_dwell +  
              sum_breakfast + sum_lunch + sum_afternoon + sum_dinner+ sum_night)

model1 <- lm(median_dwell ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  freq +  
              sum_breakfast + sum_lunch + sum_afternoon + sum_dinner+ sum_night)
summary(model)
shapiro.test(model$residuals)
x11()
par(mfrow=c(2,2))
plot(model)

summary(model1)
shapiro.test(model1$residuals)
x11()
par(mfrow=c(2,2))
plot(model1)

lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,median_dwell,
                                            distance_from_home, distance_from_primary_daytime_location,
                                            freq, (sum_breakfast+1e-16), (sum_lunch+1e-16), (sum_afternoon+1e-16),
                                            (sum_dinner+1e-16), (sum_night+1e-16)))
lambda_multivariate

BC.stop <- bcPower(raw_stop_counts, lambda_multivariate$lambda[1])
BC.device <- bcPower(raw_device_counts, lambda_multivariate$lambda[2])
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[3])
BC.dist_home <- bcPower(distance_from_home, lambda_multivariate$lambda[4])
BC.dist_primary <- bcPower(distance_from_primary_daytime_location, lambda_multivariate$lambda[5])
BC.freq <- bcPower(median_dwell, lambda_multivariate$lambda[6])
BC.break <- bcPower(sum_breakfast+1e-16, lambda_multivariate$lambda[7])
BC.lunch <- bcPower(sum_lunch+1e-16, lambda_multivariate$lambda[8])
BC.aft <- bcPower(sum_afternoon+1e-16, lambda_multivariate$lambda[9])
BC.dinner <- bcPower(sum_dinner+1e-16, lambda_multivariate$lambda[10])
BC.night <- bcPower(sum_night+1e-16, lambda_multivariate$lambda[11])

mod_multivariate=lm(formula = BC.median ~ BC.stop + BC.device + BC.dist_home +BC.dist_primary
                    + BC.freq + BC.break + BC.lunch + BC.aft+ BC.dinner + BC.night)
summary(mod_multivariate)
shapiro.test(mod_multivariate$residuals)
x11()
par(mfrow=c(2,2))
plot(mod_multivariate)

mod=lm(formula = BC.freq ~ BC.stop + BC.device + BC.dist_home +BC.dist_primary
                    + BC.median + BC.break + BC.lunch + BC.aft+ BC.dinner + BC.night)
summary(mod)
shapiro.test(mod$residuals)
x11()
par(mfrow=c(2,2))
plot(mod)
