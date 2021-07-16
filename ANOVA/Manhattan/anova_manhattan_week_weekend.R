### anova manhattan week vs weekend ###

load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")

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

area_cbg=matrix(nrow = 1, ncol = 1092)
#par(mfrow=c(4,5))
for (i in 1:1092) {
  #points= CBG_ny_no_river$geometry[[i]][[1]][[1]][,1], CBG_ny_no_river$geometry[[i]][[1]][[1]][,2]
  coords= CBG_ny_no_river$geometry[[i]][[1]][[1]]
  area_cbg[i]=areaPolygon(coords)
}

area_cbg=area_cbg/10^6;

dev <- c();
tot <- 0; #conterà quanti cbg della county sta trovando
settimana <- c('1mon', '2tue', '3wen', '4thu', '5fri','6sat', '7sun');

for(i in 1: n){                 #loop on CBGs
  j <- match(area[i],CensusBlockGroup); #estrae gli indici delle cbg in PatternNY corrispondenti ai CBG nel dataframe  CBG_ny_no_river
  if(County[j] == "New York County"){ #list_county[l]
    tot <- tot + 1;
    lun <- stops_by_day[[i]][seq(1,30,7)];
    mar <- stops_by_day[[i]][seq(2,30,7)];
    mer <- stops_by_day[[i]][seq(3,30,7)];
    gio <- stops_by_day[[i]][seq(4,30,7)];
    ven <- stops_by_day[[i]][seq(5,30,7)];
    sab <- stops_by_day[[i]][seq(6,30,7)];
    dom <- stops_by_day[[i]][seq(7,30,7)];
    dev[(tot*7 - 6): (tot*7)] <- c(mean(lun)/area_cbg[j], mean(mar)/area_cbg[j], 
                                   mean(mer)/area_cbg[j], mean(gio)/area_cbg[j],
                                   mean(ven)/area_cbg[j], mean(sab)/area_cbg[j],
                                   mean(dom)/area_cbg[j]);
  }
}


days <- rep(settimana, times = tot);
#days <- factor(days);

dev <- rep(0.0, times = n);
group <- c('work day', 'weekend');
days <- rep(group, times = n/2);

for (i in seq(1,n,2)){
  j <- match(area[i],CensusBlockGroup);
  tot = c(stops_by_day[[i]][seq(1,30,7)], stops_by_day[[i]][seq(2,30,7)],
          stops_by_day[[i]][seq(3,30,7)], stops_by_day[[i]][seq(4,30,7)],
          stops_by_day[[i]][seq(5,30,7)]);
  dev[i] = mean(tot)/area_cbg[i];
  tot <- c(stops_by_day[[i]][seq(6,30,7)], stops_by_day[[i]][seq(7,30,7)]);
  dev[i+1] <- mean(tot)/area_cbg[j];
  
}

remove <- which(dev > 3*10^4)
plot(dev, xlim=c(0,1100))
abline(h=3*10^4)
par(new=TRUE)
points(remove,dev[remove], col ='red',xlim=c(0,2300))


# proviamo a togliere questi outliers

dev<-dev[-remove]
days<-days[-remove]

plot(dev, xlim=c(0,1100), col=(factor(days)))


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
# p-value < 2.2e-16... QUINDI SIGMA diverse ... anche dal plot si vede tanto è un pb

fit <- aov(dev ~ days)
summary(fit) # 0.495
