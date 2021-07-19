#############################
#### LOGISTIC REGRESSION ####
#############################

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

brands_reg<-BRANDS_NY[,c(2,3,4,5,6,7,8,9,10,11,14)]
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

model1 <- nnet::multinom(top_category ~raw_device_counts+raw_stop_counts+
                          distance_from_home+median_dwell+distance_from_primary_daytime_location+
                          sum_breakfast+sum_afternoon+sum_lunch+sum_dinner+sum_night+ 
                          freq_top_brand, data = train.data)
summary(model1)

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
# 1) togliamo raw_stop_counts
model2 <- nnet::multinom(top_category ~raw_device_counts+
                          distance_from_home+median_dwell+distance_from_primary_daytime_location+
                          sum_breakfast+sum_afternoon+sum_lunch+sum_dinner+sum_night+ 
                          freq_top_brand, data = train.data)
# 2) togliamo raw_device_counts
model3 <- nnet::multinom(top_category ~raw_stop_counts+
                           distance_from_home+median_dwell+distance_from_primary_daytime_location+
                           sum_breakfast+sum_afternoon+sum_lunch+sum_dinner+sum_night+ 
                           freq_top_brand, data = train.data)

# A) Confronto se togliere raw_stops_counts o raw_device_counts
models <- list(model1, model2, model3)

aictab(cand.set = models, modnames = c("model1", "model2", "model3"), sort=TRUE) #scelgo modello 2

# pvalue
z <- summary(model2)$coefficients/summary(model2)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#3) togliamo distance_from_home
model4 <- nnet::multinom(top_category ~raw_device_counts+
                           median_dwell+distance_from_primary_daytime_location+
                           sum_breakfast+sum_afternoon+sum_lunch+sum_dinner+sum_night+ 
                           freq_top_brand, data = train.data)

#4) togliamo distance_from_primary_daytime_location
model5 <- nnet::multinom(top_category ~raw_device_counts+
                           median_dwell+distance_from_home+
                           sum_breakfast+sum_afternoon+sum_lunch+sum_dinner+sum_night+ 
                           freq_top_brand, data = train.data)

#6) togliamo entrambi
model6 <- nnet::multinom(top_category ~raw_device_counts+
                           median_dwell+
                           sum_breakfast+sum_afternoon+sum_lunch+sum_dinner+sum_night+ 
                           freq_top_brand, data = train.data)

# B) Confronto se togliere distance_from_primary_daytime_location o distance_from_home
models <- list(model2, model4, model5, model6)

aictab(cand.set = models, modnames = c("model2", "model4", "model5", "model6"), sort=TRUE) #scelgo modello 2


#------------------------------------------------------------------------------------------------------------
# PCA
pc.data <- princomp(brands_reg, scores=T)
pc.data
summary(pc.data)

# To obtain the rows of the summary:
# standard deviation of the components
pc.data$sd
# proportion of variance explained by each PC
pc.data$sd^2/sum(pc.data$sd^2)
# cumulative proportion of explained variance
cumsum(pc.data$sd^2)/sum(pc.data$sd^2)

# loadings (recall: coefficients of the linear combination of the original 
#           variables that defines each principal component)

load.tour <- pc.data$loadings
load.tour #finds the most meaningful Principal components

# graphical representation of the loadings of the first six principal components
x11()
par(mfcol = c(2,1))
for(i in 1:2) barplot(las=2,load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))
#since we want to do dimensionality reduction, we consider only the first 2/3 directions

# Interpretation of the loadings:
# First PCs: weighted average of the number of nights in 3,4 stars hotel and residences. it gives different means
# Second PCs: contrast between the number of nights in 3 and 4 stars hotel. the two weights have different sign->contrast
# Third PC: residences

# The loadings reflect the previous observation: the first 3 PCs are 
# driven by the variables displaying the highest variability

# Explained variance
x11()
par(mfrow=c(1,2))
plot(pc.data, las=2, main='Principal components')
plot(cumsum(pc.data$sd^2)/sum(pc.data$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data),labels=1:ncol(data),las=2)
#barplot1 -> variances of the original dataset
#barplot -> variances of the first pc

# The first PC explains more than 98% of the total variability. 
# This is due to the masking effect of those 3 variables over the others


#------------- SCATTERPLOT ---------------------

# scores
scores <- pc.data$scores
scores

# Scores
x11()
par(mfrow=c(1,1))
plot(scores[,1],scores[,2],type="n",xlab="pc1",ylab="pc2", asp=1)
text(scores[,1],scores[,2],dimnames(brands_reg)[[1]], cex=0.7)
abline(h=0)
abline(v=0)


# # proviamo a togliere gli ouliers (92 e 532, 36)
# brands_reg<-brands_reg[-c(36,92,532),]
# BRANDS_NY<-BRANDS_NY[-c(36,92,532),]



