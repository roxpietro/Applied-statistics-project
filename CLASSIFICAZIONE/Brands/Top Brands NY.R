#############################
#### LOGISTIC REGRESSION ####
#############################

# set working directory data

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/River_Dataset.RData")


#libraries
library(geosphere)
library(sf)
library(sp)
library(ggplot2)
library(caret)

CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]
New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]


# take only the first 3 top same month brand for each CBG
top_brand_ny=c()
freq_top_brand=c()
for (i in 1:dim(New_York_County_no_river)[1] ){
      top_brand_ny=append(top_brand_ny,New_York_County_no_river$top_same_month_brand_name [[i]][1])
      freq_top_brand=append(freq_top_brand,New_York_County_no_river$top_same_month_brand [[i]][1])
  }
  

# build the dataframe
setwd("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/CLASSIFICAZIONE/Brands")
brands<-read.delim(file = "Classificazione brands.txt", header = FALSE, sep=":")

top_category<-c()
for (i in 1:dim(New_York_County_no_river)[1] ){
  if (top_brand_ny[i] %in% brands[,1]) {
    index=which(brands[,1]==top_brand_ny[i])
    top_category=append(top_category,brands[index,2])
  }
  else
    print(top_brand_ny[i])
}

attach(New_York_County_no_river)
BRANDS_NY<-data.frame(area,raw_stop_counts, raw_device_counts,
                      #stops_by_day, device_home_areas, breakfast_device_home_areas, lunch_device_home_areas, afternoon_tea_device_home_areas, dinner_device_home_areas, nightlife_device_home_areas, work_hours_device_home_areas,
                      distance_from_home, distance_from_primary_daytime_location, median_dwell, sum_breakfast, sum_lunch, sum_afternoon, sum_dinner, sum_night, top_brand_ny, top_category, freq_top_brand)

#save(BRANDS_NY,file="Top Brands NYC.RData")
rm(brands)

detach(New_York_County_no_river)
attach(BRANDS_NY)

# Split the data into training and test set
set.seed(123)
training.samples <- BRANDS_NY$top_category %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- BRANDS_NY[training.samples, ]
test.data <- BRANDS_NY[-training.samples, ]

model <- nnet::multinom(top_category ~raw_device_counts+raw_stop_counts+
                          distance_from_home+median_dwell+distance_from_primary_daytime_location+
                          sum_breakfast+sum_afternoon+sum_lunch+sum_dinner+sum_night+ 
                          freq_top_brand, data = train.data)
summary(model)

# Make predictions
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test.data$top_category)

#misclassification error on the training data set
cm <- table(predict(model), train.data$top_category) 
print(cm)
1-sum(diag(cm))/sum(cm) #0.1947608


# pvalue
z <- summary(model)$coefficients/summary(model)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# model selection:
# 1) togliamo raw