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
# 1.318687e-53 3.526374e-52 6.155367e-58 3.729458e-58 4.168776e-57

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]),
         var(dev[ days==group[3] ]),
         var(dev[ days==group[4] ]),
         var(dev[ days==group[5] ]));
Var
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
# p-value 1.1e-14 *** : H0 refused -> different mean
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

dev <- rep(0.0, times = n*g1);
group <- c('breakfast', 'lunch', 'tea', 'dinner', 'night');
days <- rep(group, times = n);

for (i in 1:n){
  j <- match(area[i],CensusBlockGroup);
  dev[i*5 - 4] <- sum(breakfast_device_home_areas[[i]]);
  dev[i*5 - 3] <- sum(lunch_device_home_areas[[i]]);
  dev[i*5 - 2] <- sum(afternoon_tea_device_home_areas[[i]]);
  dev[i*5 - 1] <- sum(dinner_device_home_areas[[i]]);
  dev[i*5 ] <- sum(nightlife_device_home_areas[[i]]);
  dev[i:i+5] <- dev[i:i+5] / area_cbg[j];
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
# 1.744527e-55 2.053486e-57 2.813993e-59 2.809825e-60 9.789645e-55

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==group[1] ]),
         var(dev[ days==group[2] ]),
         var(dev[ days==group[3] ]),
         var(dev[ days==group[4] ]),
         var(dev[ days==group[5] ]));
Var
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
# p-value 0.0925 .  -> mh ... H0 si può rifiutare fino a livello 0.09 altrimenti sotto no
detach(sub_patt);
detach(census_blocks_ny)
