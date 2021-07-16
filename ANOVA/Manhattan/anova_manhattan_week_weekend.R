#######################################
### anova manhattan week vs weekend ###
#######################################

library(sp) 
library(sf) 


#-------------------------------------------------------
# anova with density

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
#terri
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")

New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]
CBG_ny_no_river=CBG_ny_no_river[order(CBG_ny_no_river$CensusBlockGroup),]

#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("viridis")
library("viridis")

g1 <-5
g2 <- 2;
coldays <- brewer.pal(n = 7, name = 'Set2');
colcountyNY <-brewer.pal(n = 7, name = 'Accent'); 

n <- dim( New_York_County_no_river)[1];
attach( New_York_County_no_river)
attach(CBG_ny_no_river)


library(geosphere)

area_cbg=c()
#par(mfrow=c(4,5))
for (i in 1:1092) {
  #points= CBG_ny_no_river$geometry[[i]][[1]][[1]][,1], CBG_ny_no_river$geometry[[i]][[1]][[1]][,2]
  coords= CBG_ny_no_river$geometry[[i]][[1]][[1]]
  area_cbg[i]=areaPolygon(coords)
}

area_cbg=area_cbg/10^6;



dev <- rep(0.0, times = 2*n);
group <- c('work day', 'weekend');
days <- rep(group, times = n);
k=1
for (i in seq(1,n)){
  tot = c(stops_by_day[[i]][seq(1,30,7)], stops_by_day[[i]][seq(2,30,7)],
          stops_by_day[[i]][seq(3,30,7)], stops_by_day[[i]][seq(4,30,7)],
          stops_by_day[[i]][seq(5,30,7)]);
  
  dev[k] = mean(tot)/area_cbg[i];
  k=k+1
  tot <- c(stops_by_day[[i]][seq(6,30,7)], stops_by_day[[i]][seq(7,30,7)]);
  dev[k] <- mean(tot)/area_cbg[i];
  k=k+1
  
}

remove <- which(dev > 10^4)
index<-floor(remove/2)
print(New_York_County_no_river$area[index])

x11()
plot(dev)
abline(h=3*10^4)
par(new=TRUE)
points(remove,dev[remove], col ='red',xlim=c(0,2300))

k<-index
rem<-c()
for(i in 1:length(k)){
  points((2*k[i]):(2*k[i]+1),dev[(2*k[i]):(2*k[i]+1)], col ='red')
  rem<-c(rem,(2*k[i]):(2*k[i]+1) )
}


#x11()
png(file = "Manhattan boxplot food hours.png")
boxplot( dev ~ days, main = "food hours")
dev.off()



# proviamo a togliere questi outliers
dev<-dev[-rem]
days<-days[-rem]

plot(dev, col=(factor(days)))


x11()
#png(file = "boxplot_work.png")
boxplot( (dev) ~ days, main = "weekend vs work days")
dev.off()

#qua infatti le medie confrontando le densità sembrano differenti
#ma pvalue alto
x11()
#png(file = "barplot_work.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=c(coldays[4], coldays[6]),main='Model under H1') #, ylim=c(0,max(dev))
#plot fino ai picchi
x11()
par(mfrow=c(1,2))
barplot(rep(mean(dev),7), names.arg=levels(days),ylim=c(0,max(dev)),
        las=2, col='grey85', main='Model under H0') 
barplot(tapply(dev, days, mean), names.arg=levels(days),ylim=c(0,max(dev)),
        las=2, col=c(coldays[4], coldays[6]), main='Model under H1')

dev.off()

Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p)
Ps        

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var #12021732  2350414

x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16 senza outliers... QUINDI SIGMA diverse ... anche dal plot si vede tanto è un pb
# pvalue 0.482 con outliers
fit <- aov(dev ~ days)
summary(fit) 



#------------------------------------------ 
# permutation test

# Permutation test:
# Test statistic: F stat
T0 <- summary(fit)[[1]][1,4] #f value dell'anova
T0

# what happens if we permute the data?
permutazione <- sample(1:2160)
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
  permutazione <- sample(1:2160)
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


#-----------------------------------------------------------------
# anova without density

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")

load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")


New_York_County_no_river=New_York_County_no_river[order(New_York_County_no_river$area),]

#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("viridis")
library("viridis")

g1 <-5
g2 <- 2;
coldays <- brewer.pal(n = 7, name = 'Set2');
colcountyNY <-brewer.pal(n = 7, name = 'Accent'); 

n <- dim( New_York_County_no_river)[1];
attach( New_York_County_no_river)


dev <- rep(0.0, times = 2*n);
group <- c('work day', 'weekend');
days <- rep(group, times = n);
k=1
for (i in seq(1,n)){
  tot = c(stops_by_day[[i]][seq(1,30,7)], stops_by_day[[i]][seq(2,30,7)],
          stops_by_day[[i]][seq(3,30,7)], stops_by_day[[i]][seq(4,30,7)],
          stops_by_day[[i]][seq(5,30,7)]);
  
  dev[k] = mean(tot)
  k=k+1
  tot <- c(stops_by_day[[i]][seq(6,30,7)], stops_by_day[[i]][seq(7,30,7)]);
  dev[k] <- mean(tot)
  k=k+1
  
}

x11()
plot(dev)

remove <- which(dev > 2000)
index<-floor(remove/2)
print(New_York_County_no_river$area[index])

x11()
plot(dev)
abline(h=2000)
par(new=TRUE)
points(remove,dev[remove], col ='red',xlim=c(0,2300))

k<-index
rem<-c()
for(i in 1:length(k)){
  points((2*k[i]):(2*k[i]+1),dev[(2*k[i]):(2*k[i]+1)], col ='red')
  rem<-c(rem,(2*k[i]):(2*k[i]+1) )
}


#x11()
#png(file = "Manhattan boxplot food hours.png")
boxplot( dev ~ days, main = "food hours")
dev.off()



# proviamo a togliere questi outliers
dev<-dev[-rem]
days<-days[-rem]

plot(dev, col=(factor(days)))


x11()
#png(file = "boxplot_work.png")
boxplot( (dev) ~ days, main = "weekend vs work days")
dev.off()

#qua infatti le medie confrontando le densità sembrano differenti
#ma pvalue alto
x11()
#png(file = "barplot_work.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=c(coldays[4], coldays[6]),main='Model under H1') #, ylim=c(0,max(dev))
#plot fino ai picchi
x11()
par(mfrow=c(1,2))
barplot(rep(mean(dev),7), names.arg=levels(days),ylim=c(0,max(dev)),
        las=2, col='grey85', main='Model under H0') 
barplot(tapply(dev, days, mean), names.arg=levels(days),ylim=c(0,max(dev)),
        las=2, col=c(coldays[4], coldays[6]), main='Model under H1')

dev.off()

Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p)
Ps        

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]));
Var #12021732  2350414

x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16 senza outliers... QUINDI SIGMA diverse ... anche dal plot si vede tanto è un pb
# pvalue 0.482 con outliers
fit <- aov(dev ~ days)
summary(fit) 



#------------------------------------------ 
# permutation test

# Permutation test:
# Test statistic: F stat
T0 <- summary(fit)[[1]][1,4] #f value dell'anova
T0

# what happens if we permute the data?
permutazione <- sample(1:2176)
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
  permutazione <- sample(1:2176)
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

