# Set Working Directory
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/anova")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/LAB_5/mcshapiro.test.RData")

# Load Dataset
load("~/Documenti/UNIVERSITA/STAT APP/progetto/Patterns_NY.RData")
load("~/Documenti/UNIVERSITA/STAT APP/progetto/Cyber.RData")

attach(Patterns_NY)
attach(census_blocks_ny)

library(RColorBrewer)

n <- dim(Patterns_NY)[1];
g1 <- 2;
g2 <- 62;

#  NO density
#_______________________________________________________________________________
#2ways anova with treatment1= "weekday, weekend" and treatment2 = county
list_county=c(County[[1]]);
for (i in 2:15446){
  list_county=c(list_county, County[[i]] );
  list_county = unique(list_county);
}


dev <- rep(0.0, n*g1);
treat1 <- c('weekday', 'weekend');
days <- rep(treat1, times = n); #info on treat1
county = rep(0, times = n*2); #info on treat2
county_days = rep(0, times = n*2); #info on treat1 + treat2

for(i in 1: n){
  j <- match(area[i],CensusBlockGroup);
  
  dev[i*2 - 1] <- sum(weekday_device_home_areas[[i]]);
  dev[i*2]     <- sum(weekend_device_home_areas[[i]]);
  county[((i*2)-1):(i*2)] <- rep(County[j], times = 2);
  county_days[((i*2)-1):(i*2)] <- paste(County[j], treat1[1:2]);
  
}

#transform in factor var
days <- factor(days);
county <- factor(county);
county_days <- factor(county_days);

M <- mean(dev);
Mdays <- tapply(dev, days, mean);
Mcounty <- tapply(dev, county, mean);
Mday_county <- tapply(dev, county_days, mean);

g <- length(levels(days)); #number of days
b <- length(levels(county)); #number of counties
n1 <- length(dev)/(g*b)

#graphical analysis
#x11()
png(file = "2wayanova work_county.png")
par(mfrow=c(3,1),las=2)
barplot(rep(M, g * b), names.arg=levels(county_days),las=2, col='grey85', main='No factor')
barplot(rep(Mdays,times=b), names.arg=levels(county_days), 
        las=2, col='grey85', main='Only Fact. weekday vs weekend')
barplot(rep(Mcounty,each=g), names.arg=levels(county_days),
        las = 2, col=rep(c('darkgreen','orange'),times=g), main='Only Fact. County')
dev.off()
### Verify the assumptions
# 1) normality (multivariate) in each group (62*2 test ...) -> Ps
Ps <- c();
for (i in 1:(g*b)){
  Ps[i] <- shapiro.test(dev[ county_days==levels(county_days)[i] ])$p;
}
#troppe assurdo ...
Ps
jj <- which(Ps > 0.05);
length(jj) #conta test con pvalue sopra 5%
Ps[jj]
county_days[jj] #  Essex County weekday  Essex County weekend  Nassau County weekday Nassau County weekend

# 2) homogeneity of the covariance (qualitatively)
bartlett.test(dev, county_days) # p-value < 2.2e-16
bartlett.test(dev, county)      # p-value < 2.2e-16
bartlett.test(dev, days)        # p-value < 2.2e-16

fit.aov2.int <- aov(dev ~ days + county + days:county)
summary.aov(fit.aov2.int)
#days            1 8.597e+08 859744666 1716.312  < 2e-16 ***
#county         61 7.861e+08  12886882   25.726  < 2e-16 ***
#days:county    61 6.155e+07   1008992    2.014 4.81e-06 ***

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

dev <- rep(0.0, n*g1);
treat1 <- c('weekday', 'weekend');
days <- rep(treat1, times = n); #info on treat1
county = rep(0, times = n*2); #info on treat2
county_days = rep(0, times = n*2); #info on treat1 + treat2

for(i in 1: n){
  j <- match(area[i],CensusBlockGroup);
  
  dev[i*2 - 1] <- sum(weekday_device_home_areas[[i]]) / area_cbg[j];
  dev[i*2]     <- sum(weekend_device_home_areas[[i]]) / area_cbg[j];
  county[((i*2)-1):(i*2)] <- rep(County[j], times = 2);
  county_days[((i*2)-1):(i*2)] <- paste(County[j], treat1[1:2]);
  
}

#transform in factor var
days <- factor(days);
county <- factor(county);
county_days <- factor(county_days);

M <- mean(dev);
Mdays <- tapply(dev, days, mean);
Mcounty <- tapply(dev, county, mean);
Mday_county <- tapply(dev, county_days, mean);

g <- length(levels(days)); #number of days
b <- length(levels(county)); #number of counties
n1 <- length(dev)/(g*b)

#graphical analysis
#x11()
png(file = "2wayanova density work_county.png")
par(mfrow=c(3,1),las=2)
barplot(rep(M, g * b), names.arg=levels(county_days),las=2, col='grey85', main='No factor')
barplot(rep(Mdays,times=b), names.arg=levels(county_days), 
        las=2, col='grey85', main='Only Fact. weekday vs weekend')
barplot(rep(Mcounty,each=g), names.arg=levels(county_days),
        las = 2, col=rep(c('darkgreen','orange'),times=g), main='Only Fact. County')
dev.off()
### Verify the assumptions
# 1) normality (multivariate) in each group (62*2 test ...) -> Ps
Ps <- c();
for (i in 1:(g*b)){
  Ps[i] <- shapiro.test(dev[ county_days==levels(county_days)[i] ])$p;
}

Ps
jj <- which(Ps > 0.05);
length(jj) #conta test con pvalue sopra 5%
Ps[jj]
county_days[jj] #  Essex County weekday  Essex County weekend

# 2) homogeneity of the covariance (qualitatively)
bartlett.test(dev, county_days) # p-value < 2.2e-16
bartlett.test(dev, county)      # p-value < 2.2e-16
bartlett.test(dev, days)        # p-value < 2.2e-16

fit.aov2.int <- aov(dev ~ days + county + days:county)
summary.aov(fit.aov2.int)

# days            1 4.709e+10 4.709e+10  35.433 2.67e-09 ***
# county         61 4.686e+11 7.682e+09   5.780  < 2e-16 ***
# days:county    61 7.370e+10 1.208e+09   0.909    0.676 

fit.aov2.int <- aov(dev ~ days + county)
summary.aov(fit.aov2.int)
# days            1 4.709e+10 4.709e+10  35.439 2.66e-09 ***
# county         61 4.686e+11 7.682e+09   5.781  < 2e-16 ***

detach(Patterns_NY)
detach(census_blocks_ny)
