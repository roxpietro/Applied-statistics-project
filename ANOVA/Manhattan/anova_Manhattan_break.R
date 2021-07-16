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
g1 <- 5;
col_food_hours <-brewer.pal(n = g1, name = 'Accent'); 

#5 GROUPS
# breakfast_device_home_areas -> breakfast
# lunch_device_home_areas -> lunch
# afternoon_tea_device_home_areas ->tea
# dinner_device_home_areas -> dinner
# nightlife_device_home_areas -> night

dev <- rep(0.0, times = n*g1);
group <- c('breakfast', 'lunch', 'tea', 'dinner', 'night');
days <- rep(group, times = n);

for (i in 1:n){
  dev[i*5 - 4] <- sum(breakfast_device_home_areas[[i]]);
  dev[i*5 - 3] <- sum(lunch_device_home_areas[[i]]);
  dev[i*5 - 2] <- sum(afternoon_tea_device_home_areas[[i]]);
  dev[i*5 - 1] <- sum(dinner_device_home_areas[[i]]);
  dev[i*5] <- sum(nightlife_device_home_areas[[i]]);
}

x11()
plot(dev)
abline(h=5000)
rem <- which(dev >5000)
k <- which( New_York_County_no_river$area %in% CBG_ny_no_river$CensusBlockGroup[floor(rem/5)])
for(i in 1:3){
  points((5*k[i]):(5*k[i]+5),dev[(5*k[i]):(5*k[i]+5)], col ='red')
}

CBG_ny_no_river$CensusBlockGroup[floor(rem/5)]
dev[rem]
#x11()
png(file = "Manhattan boxplot food hours.png")
boxplot( dev ~ days, main = "food hours")
dev.off()

#x11()
png(file = "Manhattan barplot food hours.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0 - food hours') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=col_food_hours,main='Model under H1 - food hours') #, ylim=c(0,max(dev))

dev.off()
### verify the assumptions:
# 1) normality (univariate) in each group (5 tests)
Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p,
        shapiro.test(dev[ days==group[3] ])$p,
        shapiro.test(dev[ days==group[4] ])$p,
        shapiro.test(dev[ days==group[5] ])$p)
Ps        
# 1.139160e-52 2.376162e-51 8.842586e-57 5.225849e-57 5.152134e-56

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]),
         var(dev[ days==group[3] ]),
         var(dev[ days==group[4] ]),
         var(dev[ days==group[5] ]));
Var #146360.16 175660.43 285537.62 588992.21  85361.97
x11()
plot(Var, ylim = c(0, max(Var)), xlab=levels(days))   
# group DINNER ha varianza molto più grande

# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
# p-value < 2.2e-16

fit <- aov(dev~days)
summary(fit)
# p-value 4.61e-15 ***: H0 refused -> different mean
#______________________________________________________________________________
#   WITH DENSITY
# Calcolo aree per calcolo density

library(geosphere)

area_cbg=matrix(nrow = 1, ncol = n)
for (i in 1:n) {
  coords=geometry[[i]][[1]][[1]]
  area_cbg[i]=areaPolygon(coords);
}
area_cbg=area_cbg/10^6;

dev <- rep(0.0, times = n*g1);
group <- c('breakfast', 'lunch', 'tea', 'dinner', 'night');
days <- rep(group, times = n);

for (i in 1:n){
  dev[i*5 - 4] <- sum(breakfast_device_home_areas[[i]]);
  dev[i*5 - 3] <- sum(lunch_device_home_areas[[i]]);
  dev[i*5 - 2] <- sum(afternoon_tea_device_home_areas[[i]]);
  dev[i*5 - 1] <- sum(dinner_device_home_areas[[i]]);
  dev[i*5 ] <- sum(nightlife_device_home_areas[[i]]);
  dev[i:i+5] <- dev[i:i+5] / area_cbg[i];
}

#x11()
png(file = "Manhattan boxplot density food hours.png")
boxplot( dev ~ days, main = "density food hours")
dev.off()

#x11()
png(file = "Manhattan barplot density food hours.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0 - density food hours') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=col_food_hours,main='Model under H1 - density food hours') #, ylim=c(0,max(dev))

dev.off()
### verify the assumptions:
# 1) normality (univariate) in each group (5 tests)
Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p,
        shapiro.test(dev[ days==group[3] ])$p,
        shapiro.test(dev[ days==group[4] ])$p,
        shapiro.test(dev[ days==group[5] ])$p)
Ps        
# 3.525285e-59 2.356852e-54 1.232153e-58 2.677464e-58 2.138138e-58

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]),
         var(dev[ days==group[3] ]),
         var(dev[ days==group[4] ]),
         var(dev[ days==group[5] ]));
Var #20304061906    96333127   495773316   365064256   111602807
x11()
plot(Var, ylim = c(0, max(Var)))   
# group DINNER ha varianza molto più grande

# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
#p-value < 2.2e-16

fit <- aov(dev ~ days)
summary(fit)
# p-value 0.43 azzzz
detach(New_York_County_no_river);
detach(CBG_ny_no_river)
