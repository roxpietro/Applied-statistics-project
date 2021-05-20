# Set Working Directory
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/anova")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/LAB_5/mcshapiro.test.RData")

# Load Dataset
load("~/Documenti/UNIVERSITA/STAT APP/progetto/New York County.RData")
load("~/Documenti/UNIVERSITA/STAT APP/progetto/Cyber.RData")

attach(sub_patt)
attach(census_blocks_ny)

library(RColorBrewer)

n <- dim(sub_patt)[1];
g1 <- length(day_counts[[1]]);
coldays <- brewer.pal(n = g1, name = 'Set2');

#NO density
#first analysis: 1- (like before) group stops_by_day in weekday vs weekend
#                2- weekday_device_home_area vs weekend_device_home_areas summed

#_______________________________________________________________________________
#1 - grouped all Manhattan (NY County ) CBGs MEAN n.of devices in 
#weekdays(work days) (Mon-Fri) and weekend days(Sat-Sun) vectors
dev <- rep(0.0, times = n*2);
group <- c('weekday', 'weekend');
days <- rep(group, times = n);

for (i in 1:n){
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
boxplot( dev ~ days, main = "weekdays vs weekend")
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
# 4.482263e-58 4.821560e-51
# ?! come possibile venga così basso ... cmq direi non gaussiane

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
#PVALUE BASSO BASSO ... QUINDI SIGMA diverse ... anche dal plot si vede tanto è un pb
#vero che hanno diversi numeri di elementi i gruppi 

fit <- aov(dev ~ days)
summary(fit)
# result p-value 0.431 -> not have enough evidence to refuse H0 -> same stops mean

#_______________________________________________________________________________
#2 - anova weekday_device_home_areas vs weekend_device_home_areas summed
# test H0 mean(sum(weekday_dha)) = mean(sum(weekend_dha))
# so group visitors from different home according to weekday vs weekend

#dha = device home area
dev <- rep(0.0, times = n*2);
group <- c('weekday', 'weekend');
days <- rep(group, times = n);

for (i in 1:n){
  #j <- match(area[i],CensusBlockGroup);
  dev[2*i -1] <- sum(weekday_device_home_areas[[i]]);
  dev[2*i] <- sum(weekend_device_home_areas[[i]]);
}

# x11()
png(file = "Manhattan boxplot with dha.png")
boxplot( dev ~ days, main = "weekdays vs weekend")
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
# 2.837032e-56 2.685911e-57

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16 so not same variance

fit <- aov(dev ~ days)
summary(fit)
# <2e-16 *** -> refuse H0: different mean

#______________________________________________________________________________
#   WITH DENSITY
# Calcolo aree per calcolo density

library(geosphere)

area_cbg=matrix(nrow = 1, ncol = 15463)
for (i in 1:15463) {
  coords=census_blocks_ny$geometry[[i]][[1]][[1]]
  area_cbg[i]=areaPolygon(coords);
}

area_cbg=area_cbg/10^6;
#______________________________________________________________________________
#1 - grouped all Manhattan (NY County ) CBGs MEAN n.of devices in 
#weekdays(work days) (Mon-Fri) and weekend days(Sat-Sun) vectors
dev <- rep(0.0, times = n*2);
group <- c('weekday', 'weekend');
days <- rep(group, times = n);

for (i in 1:n){
  j <- match(area[i],CensusBlockGroup);
  tot = c(stops_by_day[[i]][seq(1,30,7)], stops_by_day[[i]][seq(2,30,7)],
          stops_by_day[[i]][seq(3,30,7)], stops_by_day[[i]][seq(4,30,7)],
          stops_by_day[[i]][seq(5,30,7)]);
  dev[i] = mean(tot)/area_cbg[j];
  tot <- c(stops_by_day[[i]][seq(6,30,7)], stops_by_day[[i]][seq(7,30,7)]);
  dev[i+1] <- mean(tot)/area_cbg[j];
  
}
# x11()
png(file = "Manhattan boxplot with density stops_by_day.png")
boxplot( dev ~ days, main = "weekdays vs weekend")
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
#7.467253e-52 1.775820e-60
# ?! come possibile venga così basso ... cmq direi non gaussiane

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16
#PVALUE BASSO BASSO ... QUINDI SIGMA diverse ... anche dal plot si vede tanto è un pb
#vero che hanno diversi numeri di elementi i gruppi 

fit <- aov(dev ~ days)
summary(fit)
# result p-value 0.39 -> not have enough evidence to refuse H0 -> same stops mean
# strano che sono più alti visivamente quelli del weekend

#_______________________________________________________________________________
#2 - anova weekday_device_home_areas vs weekend_device_home_areas summed
# test H0 mean(sum(weekday_dha)) = mean(sum(weekend_dha))
# so group visitors from different home according to weekday vs weekend

#dha = device home area
dev <- rep(0.0, times = n*2);
group <- c('weekday', 'weekend');
days <- rep(group, times = n);

for (i in 1:n){
  j <- match(area[i],CensusBlockGroup);
  dev[2*i -1] <- sum(weekday_device_home_areas[[i]]) / area_cbg[j];
  dev[2*i] <- sum(weekend_device_home_areas[[i]])  / area_cbg[j];
}

# x11()
png(file = "Manhattan boxplot with density dha.png")
boxplot( dev ~ days, main = "weekdays vs weekend")
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
#  1.944004e-60 2.085112e-60

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16 so not same variance

fit <- aov(dev ~ days)
summary(fit)
# 0.036 * -> refuse H0: different mean


detach(sub_patt)
