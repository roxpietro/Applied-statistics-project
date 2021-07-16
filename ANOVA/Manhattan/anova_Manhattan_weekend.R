# Set Working Directory
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/ANOVA/Manhattan")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/LAB_5/mcshapiro.test.RData")

# Load Dataset
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

attach(CBG_ny_no_river)
attach(New_York_County_no_river)

library(RColorBrewer)

n <- dim(New_York_County_no_river)[1];
g1 <- length(day_counts[[1]]);
coldays <- brewer.pal(n = g1, name = 'Set2');

#NO density
#first analysis: 1- (like before) group stops_by_day in  vs weekend
#                2- _device_home_area vs weekend_device_home_areas summed

#_______________________________________________________________________________
#1 - grouped all Manhattan (NY County ) CBGs MEAN n.of devices in 
#s(work days) (Mon-Fri) and weekend days(Sat-Sun) vectors
dev <- rep(0.0, times = n*2);
group <- c('work day', 'weekend');
days <- rep(group, times = n);

for (i in seq(1,n,2)){
  #j <- match(area[i],CensusBlockGroup);
  tot = c(stops_by_day[[i]][seq(1,30,7)], stops_by_day[[i]][seq(2,30,7)],
          stops_by_day[[i]][seq(3,30,7)], stops_by_day[[i]][seq(4,30,7)],
          stops_by_day[[i]][seq(5,30,7)]);
  dev[i] = mean(tot);
  tot <- c(stops_by_day[[i]][seq(6,30,7)], stops_by_day[[i]][seq(7,30,7)]);
  dev[i+1] <- mean(tot);
  
}
# x11()
png(file = "Manhattan boxplot with stops_by_day.png")
boxplot( dev ~ days, main = "workdays vs weekend")
dev.off()

#qua infatti le medie confrontando le densità sembrano differenti
#rispetto alla varibailità no però direi

#x11()
png(file = "Manhattan barplot with stops_by_day.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=c(coldays[4], coldays[6]),main='Model under H1') #, ylim=c(0,max(dev))

dev.off()

### verify the assumptions:
# 1) normality (univariate) in each group (2 tests)
Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p)
Ps        
# 6.815282e-46 3.155112e-42
# ?! come possibile venga così basso ... cmq direi non gaussiane

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var #10342.040  4781.232
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
#PVALUE BASSO BASSO p-value = 2.2e-16.. QUINDI SIGMA diverse ... anche dal plot si vede tanto è un pb
#vero che hanno diversi numeri di elementi i gruppi 

fit <- aov(dev ~ days)
summary(fit)
# result p-value  0.000435 ***-> not have enough evidence to refuse H0 -> same stops mean

#_______________________________________________________________________________
#2 - anova _device_home_areas vs weekend_device_home_areas summed
# test H0 mean(sum(workday_dha)) = mean(sum(weekend_dha))
# so group visitors from different home according to  vs weekend

#dha = device home area
dev <- rep(0.0, times = n*2);
group <- c('workday', 'weekend');
days <- rep(group, times = n);

for (i in 1:n){
  #j <- match(area[i],CensusBlockGroup);
  dev[2*i -1] <- sum(weekday_device_home_areas[[i]]);
  dev[2*i] <- sum(weekend_device_home_areas[[i]]);
}

x11()
plot(dev)
abline(h=10000)
rem <- which(dev >10000)
k <- which( New_York_County_no_river$area %in% CBG_ny_no_river$CensusBlockGroup[floor(rem/2)])
for(i in 1:length(k)){
  points((2*k[i]):(2*k[i]+1),dev[(2*k[i]):(2*k[i]+1)], col ='red')
}

CBG_ny_no_river$CensusBlockGroup[floor(rem/2)]
#rimuovo outliers
remove <-c()
for (i in 1:length(k))
  remove <- c(remove,(2*k[i]):(2*k[i]+1))
dev<- dev[-remove]
days <- days[-remove]


# x11()
png(file = "Manhattan boxplot with dha.png")
boxplot( dev ~ days, main = "workdays vs weekend")
dev.off()

#x11()
png(file = "Manhattan barplot with dha.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=c(coldays[4], coldays[6]),main='Model under H1') #, ylim=c(0,max(dev))

dev.off()

### verify the assumptions:
# 1) normality (univariate) in each group (2 tests)
Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p)
Ps        
# 9.230507e-44 1.160269e-42
# senza outliers 1.075502e-42 5.192324e-50

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var #238185.67  37182.91
# senza outliers 358881.8 102402.1
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16 so not same variance

fit <- aov(dev ~ days)
summary(fit)
# <2e-16 *** -> refuse H0: different mean (sia con che senza outliers)

#______________________________________________________________________________
#   WITH DENSITY
# Calcolo aree per calcolo density

library(geosphere)

area_cbg=matrix(nrow = 1, ncol = n)
for (i in 1:n) {
  coords=CBG_ny_no_river$geometry[[i]][[1]][[1]]
  area_cbg[i]=areaPolygon(coords);
}

area_cbg=area_cbg/10^6;
#______________________________________________________________________________
#1 - grouped all Manhattan (NY County ) CBGs MEAN n.of devices in 
#workdays(work days) (Mon-Fri) and weekend days(Sat-Sun) vectors
dev <- rep(0.0, times = n*2);
group <- c('workday', 'weekend');
days <- rep(group, times = n);

for (i in seq(1,n,2)){
  tot = c(stops_by_day[[i]][seq(1,30,7)], stops_by_day[[i]][seq(2,30,7)],
          stops_by_day[[i]][seq(3,30,7)], stops_by_day[[i]][seq(4,30,7)],
          stops_by_day[[i]][seq(5,30,7)]);
  dev[i] = mean(tot)/area_cbg[i];
  tot <- c(stops_by_day[[i]][seq(6,30,7)], stops_by_day[[i]][seq(7,30,7)]);
  dev[i+1] <- mean(tot)/area_cbg[i];
  
}
# x11()
png(file = "Manhattan boxplot with density stops_by_day.png")
boxplot( dev ~ days, main = "workdays vs weekend")
dev.off()

#qua infatti le medie confrontando le densità sembrano differenti
#rispetto alla varibailità no però direi

#x11()
png(file = "Manhattan barplot with density stops_by_day.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=c(coldays[4], coldays[6]),main='Model under H1') #, ylim=c(0,max(dev))

dev.off()

### verify the assumptions:
# 1) normality (univariate) in each group (2 tests)
Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p)
Ps        
#4.853195e-59 5.432627e-59
# ?! come possibile venga così basso ... cmq direi non gaussiane

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var #1023478894  516823433
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value = < 2.2e-16
#PVALUE BASSO BASSO ... QUINDI SIGMA diverse ... anche dal plot si vede tanto è un pb
#vero che hanno diversi numeri di elementi i gruppi 

fit <- aov(dev ~ days)
summary(fit)
# result p-value 0.639 -> refuse H0 0.639

#_______________________________________________________________________________
#2 - anova _device_home_areas vs weekend_device_home_areas summed
# test H0 mean(sum(_dha)) = mean(sum(weekend_dha))
# so group visitors from different home according to  vs weekend

#dha = device home area
dev <- rep(0.0, times = n*2);
group <- c('work day', 'weekend');
days <- rep(group, times = n);

for (i in 1:n){
  j <- match(area[i],CensusBlockGroup);
  dev[2*i -1] <- sum(weekday_device_home_areas[[i]]) / area_cbg[j];
  dev[2*i] <- sum(weekend_device_home_areas[[i]])  / area_cbg[j];
}

x11()
plot(dev)
abline(h=2*10^5)
rem <- which(dev >2*10^5)
k <- which( New_York_County_no_river$area %in% CBG_ny_no_river$CensusBlockGroup[floor(rem/2)])
for(i in 1:length(k)){
  points((2*k[i]):(2*k[i]+1),dev[(2*k[i]):(2*k[i]+1)], col ='red')
}

CBG_ny_no_river$CensusBlockGroup[floor(rem/2)]
#rimuovo outliers
remove <-c()
for (i in 1:length(k))
  remove <- c(remove,(2*k[i]):(2*k[i]+1))
dev<- dev[-remove]
days <- days[-remove]

# x11()
png(file = "Manhattan boxplot with density dha.png")
boxplot( dev ~ days, main = "s vs weekend")
dev.off()

#x11()
png(file = "Manhattan barplot with density dha.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=c(coldays[4], coldays[6]),main='Model under H1') #, ylim=c(0,max(dev))

dev.off()

### verify the assumptions:
# 1) normality (univariate) in each group (2 tests)
Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p)
Ps        
#  4.689057e-59 4.986713e-59
#senza outliers 2.040877e-45 6.358536e-49

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var # 31984008762  4549961047
# senza outliers 125018469  29500957
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16 so not same variance

fit <- aov(dev ~ days)
summary(fit)
#  0.0365 * -> refuse H0: different mean
#  <2e-16 *** senza outliers

detach(New_York_County_no_river)
detach(CBG_ny_no_river)

#------------------------------------------ 
# permutation test

# Permutation test:
# Test statistic: F stat
T0 <- summary(fit)[[1]][1,4] #f value dell'anova
T0

# what happens if we permute the data?
nn <- length(dev)
permutazione <- sample(1:nn)
dev_perm <- dev[permutazione]
fit_perm <- aov(dev_perm ~ days)
summary(fit_perm)

plot(factor(days), dev_perm, xlab='treat',col=rainbow(2),main='Permuted Data')


# CMC to estimate the p-value
B <- 10000 # Number of permutations
T_stat <- numeric(B) 
n <- dim(dev_perm)[1]

for(perm in 1:B){
  # Permutation:
  permutazione <- sample(1:nn)
  dev_perm <- dev[permutazione]
  fit_perm <- aov(dev_perm ~ days)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

layout(1)
hist(T_stat)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val
# we reject the null hypothesis
