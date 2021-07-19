#######################################################
####### FAST FOOD FREQ - LINEAR REGRESSION ############
#######################################################
library(car)
library(MASS)
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
summary(model) #Adjusted R-squared:  0.1913 

shapiro.test(model$residuals) #6.654e-09
x11()
par(mfrow=c(2,2))
plot(model)
rem <- c(36,92,532)
area[rem]
detach(BRANDS_NY)
BRANDS_NY <- BRANDS_NY[-rem,]
attach(BRANDS_NY)
length(freq)
model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  median_dwell +  
              sum_breakfast + sum_lunch + sum_afternoon + sum_dinner+ sum_night)

summary(model) #Adjusted R-squared:  0.2935
shapiro.test(model$residuals) # 1.927e-05
x11()
par(mfrow=c(2,2))
plot(model)
rem <- c(207)
area[rem]
detach(BRANDS_NY)
BRANDS_NY <- BRANDS_NY[-rem,]
attach(BRANDS_NY)

model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  median_dwell +  
              sum_breakfast + sum_lunch + sum_afternoon + sum_dinner+ sum_night)

summary(model) #Adjusted R-squared:  0.3062 
shapiro.test(model$residuals) # 0.000152
#tutte bc tranne freq ... 
lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,median_dwell,
                                            distance_from_home, distance_from_primary_daytime_location,
                                            (sum_breakfast+1e-16), (sum_lunch+1e-16), (sum_afternoon+1e-16),
                                            (sum_dinner+1e-16), (sum_night+1e-16)))
lambda_multivariate

BC.stop <- bcPower(raw_stop_counts, lambda_multivariate$lambda[1])
BC.device <- bcPower(raw_device_counts, lambda_multivariate$lambda[2])
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[3])
BC.dist_home <- bcPower(distance_from_home, lambda_multivariate$lambda[4])
BC.dist_primary <- bcPower(distance_from_primary_daytime_location, lambda_multivariate$lambda[5])
BC.break <- bcPower(sum_breakfast+1e-16, lambda_multivariate$lambda[6])
BC.lunch <- bcPower(sum_lunch+1e-16, lambda_multivariate$lambda[7])
BC.aft <- bcPower(sum_afternoon+1e-16, lambda_multivariate$lambda[8])
BC.dinner <- bcPower(sum_dinner+1e-16, lambda_multivariate$lambda[9])
BC.night <- bcPower(sum_night+1e-16, lambda_multivariate$lambda[10])

mod=lm(freq ~ BC.stop + BC.device + BC.dist_home +BC.dist_primary
                    + BC.median + BC.break + BC.lunch + BC.aft+ BC.dinner + BC.night)
summary(mod) #Adjusted R-squared:  0.3067 
shapiro.test(mod$residuals) #0.0001968
x11()
par(mfrow=c(2,2))
plot(mod)
#plot abbastanza ok .. con freq cambiata era terribile
########################################################## anche freq bc transformed
lambda_multivariate <- powerTransform(cbind(freq, raw_stop_counts, raw_device_counts,median_dwell,
                                            distance_from_home, distance_from_primary_daytime_location,
                                            (sum_breakfast+1e-16), (sum_lunch+1e-16), (sum_afternoon+1e-16),
                                            (sum_dinner+1e-16), (sum_night+1e-16)))
lambda_multivariate

BC.freq <- bcPower(freq, lambda_multivariate$lambda[1])
BC.stop <- bcPower(raw_stop_counts, lambda_multivariate$lambda[2])
BC.device <- bcPower(raw_device_counts, lambda_multivariate$lambda[3])
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[4])
BC.dist_home <- bcPower(distance_from_home, lambda_multivariate$lambda[5])
BC.dist_primary <- bcPower(distance_from_primary_daytime_location, lambda_multivariate$lambda[6])
BC.break <- bcPower(sum_breakfast+1e-16, lambda_multivariate$lambda[7])
BC.lunch <- bcPower(sum_lunch+1e-16, lambda_multivariate$lambda[8])
BC.aft <- bcPower(sum_afternoon+1e-16, lambda_multivariate$lambda[9])
BC.dinner <- bcPower(sum_dinner+1e-16, lambda_multivariate$lambda[10])
BC.night <- bcPower(sum_night+1e-16, lambda_multivariate$lambda[11])

mod=lm(BC.freq ~ BC.stop + BC.device + BC.dist_home +BC.dist_primary
       + BC.median + BC.break + BC.lunch + BC.aft+ BC.dinner + BC.night)
summary(mod) #Adjusted R-squared:  0.3043
shapiro.test(mod$residuals) # 4.971e-07 peggiora ... why?!!
x11()
par(mfrow=c(2,2))
plot(mod) #non bellissimo
################################################################################
#torno al modello senza freq trasformata ma boh
vif(mod)
#eliminate the most collinear regressors
### problema tutti i test dopo valgono credo solo se gaussianity assumption è ok ...

#Hypothesis: BC.stop = 0 BC.device = 0
linearHypothesis(mod,
                 rbind(c(0,1,0,0,0,0,0,0,0,0,0),
                       c(0,0,1,0,0,0,0,0,0,0,0)),
                 c(0,0)) #1.75e-10 *** so significant
#remove  BC.stop because higher one at a time p-value
mod=lm(freq ~ BC.device + BC.dist_home +BC.dist_primary
       + BC.median + BC.break + BC.lunch + BC.aft+ BC.dinner + BC.night)
summary(mod) #i p-value migliorano, Radj rimane praticamente uguale
shapiro.test(mod$residuals) #ppeggiora pvalue
x11()
par(mfrow=c(2,2))
plot(mod)
#Hypothesis: BC.dist_home = 0 BC.dist_primary = 0
linearHypothesis(mod,
                 rbind(c(0,0,1,0,0,0,0,0,0,0),
                       c(0,0,0,1,0,0,0,0,0,0)),
                 c(0,0)) #< 2.2e-16 ***
#remove dist from primary home
mod=lm(freq ~ BC.device + BC.dist_home
       + BC.median + BC.break + BC.lunch + BC.aft+ BC.dinner + BC.night)
summary(mod) #i p-value migliorano, Radj diminuisce
shapiro.test(mod$residuals) # migliora pvalue ma cmq 0.0003566
x11()
par(mfrow=c(2,2))
plot(mod)
#Hypothesis: BC.break/lunch/aft/dinner/night = 0
linearHypothesis(mod,
                 rbind(c(0,0,0,0,1,0,0,0,0),
                       c(0,0,0,0,0,1,0,0,0),
                       c(0,0,0,0,0,0,1,0,0),
                       c(0,0,0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,0,0,1)),
                 c(0,0,0,0,0)) #4.861e-15 *** quindi almeno uno è significativo
# Hypothesis: BC.break = 0 BC.aft = 0 BC.dinner = 0
linearHypothesis(mod,
                 rbind(c(0,0,0,0,1,0,0,0,0),
                       c(0,0,0,0,0,0,1,0,0),
                       c(0,0,0,0,0,0,0,1,0)),
                 c(0,0,0)) # pvalue 0.2387 ->li elimino
mod=lm(freq ~ BC.device + BC.dist_home
       + BC.median + BC.lunch + BC.night)
summary(mod) #i p-value migliorano, Radj diminuisce
shapiro.test(mod$residuals) # migliora pvalue ma cmq 0.0002686
x11()
par(mfrow=c(2,2))
plot(mod)

###########altro modo di fare model selection###################################
### no transformatio bc, backward selection sulla base del pvalue ##############
# in realtà approccio che teoricamente mi convince poco#########################

model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  median_dwell +  
              sum_breakfast + sum_lunch + sum_afternoon + sum_dinner+ sum_night)

summary(model)
shapiro.test(model$residuals) #0.000152

model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  median_dwell +  
              sum_lunch + sum_afternoon + sum_dinner+ sum_night)
summary(model)
shapiro.test(model$residuals) # 0.0001551

model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  median_dwell +  
              sum_lunch + sum_afternoon + sum_night)
summary(model)
shapiro.test(model$residuals) #  0.0001777

model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              distance_from_primary_daytime_location +  median_dwell +  
              sum_lunch + sum_night)
summary(model) #Adjusted R-squared:  0.3053
shapiro.test(model$residuals) # 0.000165

vif(model) #decido di togliere una tra distance from home e dist from primary 
#location, quella con pvalue più alto

model <- lm(freq ~ raw_stop_counts + raw_device_counts + distance_from_home +
              median_dwell +  
              sum_lunch + sum_night)
summary(model) #Adjusted R-squared: 0.2743 
shapiro.test(model$residuals) # 0.0007763

vif(model) #togliamo qualcosa tra raw stops counts e raw device counts (mi aspetto collinearità tra di loro e vif alto)

model <- lm(freq ~ raw_device_counts + distance_from_home +
              median_dwell +  
              sum_lunch + sum_night)
summary(model) # Adjusted R-squared:  0.2674  sta peggiorando ...
shapiro.test(model$residuals) #0.00104 migliorato

lambda_multivariate <- powerTransform(cbind(freq, raw_device_counts,median_dwell,
                                            distance_from_home,
                                            (sum_lunch+1e-16), (sum_afternoon+1e-16),
                                            (sum_night+1e-16)))
lambda_multivariate

BC.freq <- bcPower(median_dwell, lambda_multivariate$lambda[1])
BC.device <- bcPower(raw_device_counts, lambda_multivariate$lambda[2])
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[3])
BC.dist_home <- bcPower(distance_from_home, lambda_multivariate$lambda[4])
BC.lunch <- bcPower(sum_lunch+1e-16, lambda_multivariate$lambda[5])
BC.aft <- bcPower(sum_afternoon+1e-16, lambda_multivariate$lambda[6])
BC.night <- bcPower(sum_night+1e-16, lambda_multivariate$lambda[7])

mod=lm(BC.freq ~ BC.device + BC.dist_home
       + BC.median + BC.lunch + BC.aft + BC.night)
summary(mod) #Adjusted R-squared:  0.9673 ma pvalue peggiorati ...

######################## altro metodo di model selection #######################
# #1. decidiamo di fare il boxcox del modello (univariate) ?? chiedere la differenza

