#descriptive analysis

# Set Working Directory
setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/anova")

load("/home/terri/Documenti/UNIVERSITA/STAT APP/LAB_5/mcshapiro.test.RData")
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("viridis")
library("viridis")

# Load Dataset
load("~/Documenti/UNIVERSITA/STAT APP/progetto/Patterns_NY.RData")
load("~/Documenti/UNIVERSITA/STAT APP/progetto/Cyber.RData")

attach(Patterns_NY)
attach(census_blocks_ny)


n <- dim(Patterns_NY)[1];
g1 <- length(Patterns_NY$day_counts[[1]]);
g2 <- 2;
coldays <- brewer.pal(n = g1, name = 'Set2');
colcountyNY <-brewer.pal(n = g1, name = 'Accent'); 

#_______________________________________________________________________________
#1st: grouped all CBGs MEAN n. of devices in 7 groups corresponding to week days (Mon-Sun)
dev <- rep(0.0, times = n*7);
settimana <- c('1mon', '2tue', '3wen', '4thu', '5fri','6sat', '7sun');
days <- rep(settimana, times = n);

#QUESTO SAREBBE CORRETTO PER FARE ANOVA MA DATAFRAME NON SI RIESCE A CREARE
for(i in 1: n){
  #cbg[i] <- Patterns_NY$area[[i]];
  lun <- Patterns_NY$stops_by_day[[i]][seq(1,30,7)];
  mar <- Patterns_NY$stops_by_day[[i]][seq(2,30,7)];
  mer <- Patterns_NY$stops_by_day[[i]][seq(3,30,7)];
  gio <- Patterns_NY$stops_by_day[[i]][seq(4,30,7)];
  ven <- Patterns_NY$stops_by_day[[i]][seq(5,30,7)];
  sab <- Patterns_NY$stops_by_day[[i]][seq(6,30,7)];
  dom <- Patterns_NY$stops_by_day[[i]][seq(7,30,7)];

  dev[(i*7 - 6): (i*7)] <- c(mean(lun), mean(mar), mean(mer), mean(gio), mean(ven), mean(sab), mean(dom));
  }


# first_data  <- data.frame(
#   mean_dev = dev,
#   day = days
# ) 
#DISASTRI  AFRE IL DATA FRAME, R IMPALLATO

#QUESTO DOVREBBE FARE BOXPLOT UGUALE, IL DATA FRAME SI CREA MA NON VA BENE PER ANOVA COME FORMATO
# lunm = rep(0, times = n);
# marm= rep(0, times = n);
# merm= rep(0, times = n);
# giom = rep(0, times = n);
# venm = rep(0, times = n);
# sabm = rep(0, times = n);
# domm = rep(0, times = n);
# 
# for(i in 1: n){
#   #cbg[i] <- Patterns_NY$area[[i]];
#   lun <- Patterns_NY$stops_by_day[[i]][seq(1,30,7)];
#   mar <- Patterns_NY$stops_by_day[[i]][seq(2,30,7)];
#   mer <- Patterns_NY$stops_by_day[[i]][seq(3,30,7)];
#   gio <- Patterns_NY$stops_by_day[[i]][seq(4,30,7)];
#   ven <- Patterns_NY$stops_by_day[[i]][seq(5,30,7)];
#   sab <- Patterns_NY$stops_by_day[[i]][seq(6,30,7)];
#   dom <- Patterns_NY$stops_by_day[[i]][seq(7,30,7)];
#   
#   lunm[i] <- mean(lun);
#   marm[i] <- mean(mar);
#   merm[i] <- mean(mer);
#   giom[i] <- mean(gio);
#   venm[i] <- mean(ven);
#   sabm[i] <- mean(sab);
#   domm[i] <- mean(dom);
# }
# 
# first_data <-data.frame(
#   #CBG = cbg,
#   LUNmean = lunm,
#   MARmean = marm,
#   MERmean = merm,
#   GIOmean = giom,
#   VENmean = venm,
#   SABmean = sabm,
#   DOMmean = domm
# )

# save(first_data, file = "first_data.RData")

x11()
boxplot( dev ~ days, main = "boxplot GROUPS = 7 week days")

#confronta le medie, ma plottando non fino al massimo
#x11()
png(file = "anova_days.png")
par(mfrow=c(1,2), las =2)
barplot(rep(mean(dev),7), names.arg=evels(days),
        col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
         col=coldays,main='Model under H1') #, ylim=c(0,max(dev))

dev.off()

#, se confrontata con alcuni picchi la differenza sembra trascurabile
#... eppure anova pvalue basso, quindi forse è affidabile la figura prima
#x11()
png(file = "anova_days_uptomax.png")
par(mfrow=c(1,2) , las = 2)
barplot(rep(mean(dev),7), names.arg=levels(days), ylim=c(0,max(dev)),
        las=2, col='grey85', main='Model under H0') 
barplot(tapply(dev, days, mean), names.arg=levels(days),ylim=c(0,max(dev)),
        las=2, col=coldays,main='Model under H1')

dev.off()

### verify the assumptions:
# 1) normality (univariate) in each group (6 tests)
Ps <- c(shapiro.test(dev[ days==settimana[1] ])$p,
        shapiro.test(dev[ days==settimana[2] ])$p,
        shapiro.test(dev[ days==settimana[3] ])$p,
        shapiro.test(dev[ days==settimana[4] ])$p,
        shapiro.test(dev[ days==settimana[5] ])$p,
        shapiro.test(dev[ days==settimana[6] ])$p,
        shapiro.test(dev[ days==settimana[7] ])$p);
Ps #NON VA PERCHÈ ABBIAMO TROPPE STAT UNITS

# 2) same covariance structure (= same sigma^2)
Var <- c(var(dev[ days==settimana[1] ]),
         var(dev[ days==settimana[2] ]),
         var(dev[ days==settimana[3] ]),
         var(dev[ days==settimana[4] ]),
         var(dev[ days==settimana[5] ]),
         var(dev[ days==settimana[6] ]),
         var(dev[ days==settimana[7] ]));
Var
x11()
plot(Var, ylim = c(0, max(Var)))         
# test of homogeneity of variances
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
bartlett.test(dev, days)
#PVALUE BASSO BASSO ... QUINDI SIGMA diverse ...

fit <- aov(dev ~ days)
summary(fit)
#QUA VIENE PAVLUE BASSO 
#MA LE ASSUNZIONI NON SONO VERIFICATE BENE ...
#- gaussianity ->troppi stat units
#- bartlett test anche se "graficamente" sembrava buono, cmq var troppo alte e pvalue bassissimo

#_______________________________________________________________________________

#2nd: grouped all CBGs MEAN n.of devices in work days (Mon-Fri) and ween-end days(Sat-Sun)
cbg <- rep(0, times = n)
work <- rep(0, times = n)
weekend <- rep(0, times = n)
tot <- 0.0

#SBAGLIATO PER AOV
# for(i in 1:n){
#   cbg[i] <- Patterns_NY$area[[i]];
#   tot = c(Patterns_NY$stops_by_day[[i]][seq(1,30,7)], Patterns_NY$stops_by_day[[i]][seq(2,30,7)],
#           Patterns_NY$stops_by_day[[i]][seq(3,30,7)], Patterns_NY$stops_by_day[[i]][seq(4,30,7)],
#           Patterns_NY$stops_by_day[[i]][seq(5,30,7)]);
#   work[i] = mean(tot);
#   
#   tot <- c(Patterns_NY$stops_by_day[[i]][seq(6,30,7)], Patterns_NY$stops_by_day[[i]][seq(7,30,7)]);
#   weekend[i] <- mean(tot);
# }
# 
# second_data <- data.frame(
#   WORKmean = work,
#   WEEKENDmean = weekend
# )

#vectors (compatible with aov function)
dev <- rep(0.0, times = n*2);
group <- c('work day', 'weekend');
days <- rep(group, times = n);

for (i in 1:n){
  tot = c(Patterns_NY$stops_by_day[[i]][seq(1,30,7)], Patterns_NY$stops_by_day[[i]][seq(2,30,7)],
          Patterns_NY$stops_by_day[[i]][seq(3,30,7)], Patterns_NY$stops_by_day[[i]][seq(4,30,7)],
          Patterns_NY$stops_by_day[[i]][seq(5,30,7)]);
  dev[i] = mean(tot);
  tot <- c(Patterns_NY$stops_by_day[[i]][seq(6,30,7)], Patterns_NY$stops_by_day[[i]][seq(7,30,7)]);
  dev[i+1] <- mean(tot);
  
}

# second_data <- data.frame(
#   mean_dev = dev,
#   day = days
# )
#save(second_data, file = "second_data.RData")

#SEMBRA IMPALLARSI MA NON SERVE CREARE IL DATA FRAME ...

#x11()
png(file = "boxplot_work.png")
boxplot( dev ~ days, main = "weekend vs work days")
dev.off()

#qua infatti le medie non sembrano particolarmente differenti
#anche non plottando fino ai picchi -> confermato da pvalue alto
#x11()
png(file = "barplot_work.png")
par(mfrow=c(1,2), las = 2)
barplot(rep(mean(dev),2), names.arg=levels(days),
        las=2, col='grey85', main='Model under H0') # ylim=c(0,max(dev))
barplot(tapply(dev, days, mean), names.arg=levels(days),
        las=2, col=c(coldays[4], coldays[6]),main='Model under H1') #, ylim=c(0,max(dev))

dev.off()

#plot fino ai picchi
x11()
par(mfrow=c(1,2))
barplot(rep(mean(dev),7), names.arg=levels(days),ylim=c(0,max(dev)),
        las=2, col='grey85', main='Model under H0') 
barplot(tapply(dev, days, mean), names.arg=levels(days),ylim=c(0,max(dev)),
        las=2, col=rainbow(7),main='Model under H1')

dev.off()

Ps <- c(shapiro.test(dev[ days==group[1] ])$p,
        shapiro.test(dev[ days==group[2] ])$p)
Ps        
#- gaussianity ->troppi stat units

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
#PVALUE BASSO BASSO ... QUINDI SIGMA diverse ...

fit <- aov(dev ~ days)
summary(fit)
#MI SEMBRA PVALUE ALTO -> NO EVIDENCE TO REFUSE H0 
#(assunzioni no buone, cmq però già visivamente ce lo aspettavamo)

# attach(first_data)
# x11()
# boxplot(first_data) #bleaaaaah
# attach(second_data)
# x11()
# boxplot(second_data) #bleaaaaah

#_______________________________________________________________________________

#3rd: for each CBG (so the statistical unit is the single day in the month) 
#possible problem: n small 
#groups = 7 week days
x11()
par (mfrow=c(1,2))

for (i in 1:2){
  lun <- Patterns_NY$stops_by_day[[i]][seq(1,30,7)];
  mar <- Patterns_NY$stops_by_day[[i]][seq(2,30,7)];
  mer <- c(Patterns_NY$stops_by_day[[i]][seq(3,30,7)], NA);
  gio <- c(Patterns_NY$stops_by_day[[i]][seq(4,30,7)], NA);
  ven <- c(Patterns_NY$stops_by_day[[i]][seq(5,30,7)], NA);
  sab <- c(Patterns_NY$stops_by_day[[i]][seq(6,30,7)], NA);
  dom <- c(Patterns_NY$stops_by_day[[i]][seq(7,30,7)], NA);
  
  CBGi <- data.frame(
    LUN = lun,
    MAR = mar,
    MER = mer,
    GIO = gio,
    VEN = ven,
    SAB = sab,
    DOM = dom
  )
  
  save(CBGi, file = "CBGi.RData")
  
  boxplot(CBGi, main = (paste('boxplot CBG', i)));
  
}
#DA FINIRE CORREGGERE NON SAPPIAMO SE VALGA LA PENA
#_______________________________________________________________________________
#4th: for each county, mean number of devices grouped according to 
# alternatives: . 2ways anova(with group1= 7 days and group2 = county)
#               . 1way anova(only group days, for each county)
#               . 1way anova(only group county, or only a subset of counties like the city of NY)
list_county=c(County[[1]]);
for (i in 2:15446){
  list_county=c(list_county, County[[i]] );
  list_county = unique(list_county);
}

dev <- rep(0.0, n*7);
settimana <- c('1mon', '2tue', '3wen', '4thu', '5fri','6sat', '7sun');
days <- rep(settimana, times = n);
dev_county = rep(0, times = n*7);
county_days = rep(0, times = n*7);

for(i in 1: n){
   lun <- Patterns_NY$stops_by_day[[i]][seq(1,30,7)];
   mar <- Patterns_NY$stops_by_day[[i]][seq(2,30,7)];
   mer <- Patterns_NY$stops_by_day[[i]][seq(3,30,7)];
   gio <- Patterns_NY$stops_by_day[[i]][seq(4,30,7)];
   ven <- Patterns_NY$stops_by_day[[i]][seq(5,30,7)];
   sab <- Patterns_NY$stops_by_day[[i]][seq(6,30,7)];
   dom <- Patterns_NY$stops_by_day[[i]][seq(7,30,7)];
   
  dev[(i*7 - 6): (i*7)] <- c(mean(lun), mean(mar), mean(mer), mean(gio), mean(ven), mean(sab), mean(dom));
  j <- match(area[i],CensusBlockGroup);
  dev_county[((i*7)-6):(i*7)] <- rep(County[j], times = 7);
  county_days[((i*7)-6):(i*7)] <- paste(settimana[1:7], County[j]);
}

#transform in factor var
days <- factor(days);
dev_county <- factor(dev_county);
county_days <- factor(county_days);
# day_county <- data.frame(
#   mean_dev = dev,
#   county = dev_county,
#   day = days
# )
#non funziona perch? richiede troppa memoria : Errore: cannot allocate vector of size 845 Kb

#save(day_county, file = "day_county.RData")

M <- mean(dev);
Mdays <- tapply(dev, days, mean);
Mcounty <- tapply(dev, dev_county, mean);
Mday_county <- tapply(dev, county_days, mean);

g <- length(levels(days)); #number of days
b <- length(levels(dev_county)); #number of counties
n1 <- length(dev)/(g*b)

#graphical analysis: vengono male per pb risoluzione
x11()
par(mfrow=c(3,1),las=2)
barplot(rep(M, g * b), names.arg=levels(county_days),las=2, col='grey85', main='No factor')
barplot(rep(Mdays,each=b), names.arg=levels(county_days), 
        las=2, col='grey85', main='Only Fact. Days')
barplot(rep(Mcounty,times=g), names.arg=levels(county_days), ylim=c(0,24),
        las = 2, col=rep(rainbow(62),times=g), main='Only Fact. County')

### Verify the assumptions
# 1) normality (multivariate) in each group (62*7 test ...) -> Ps
Ps <- c();
for (i in 1:(g*b)){
  Ps[i] <- shapiro.test(dev[ county_days==levels(county_days)[i] ])$p;
}
#troppe assurdo ...
Ps
jj <- which(Ps > 0.05);
length(jj) #conta test con pvalue sopra 5% -> in cui non c'è abbastanza info per rifiutare gaussianità fino a livello 5%
Ps[jj]
county_days[jj]

# 2) homogeneity of the covariance (qualitatively)
bartlett.test(dev, county_days)
bartlett.test(dev, dev_county)
#vengono tutti pvalue bassisimi ... strano ma cmq brutto

fit.aov2.int <- aov(dev ~ days + dev_county + days:dev_county)
summary.aov(fit.aov2.int)

#riga 475-509 lab 7 ha riassunto su conclusioni dal test
#INCREDIBLY (...MH AIUTO) high pvalue on the interaction ->additive model
#=> remove the interaction term and estimate the model without interaction

fit.aov2.ad <- aov(dev ~ days + dev_county)
summary.aov(fit.aov2.ad)

fit <- aov(dev ~ dev_county)
summary(fit)

#_______________________________________________________________________________
#analyse city of NY
NY_counties <- c('Bronx County', 'New York County', 'Kings County', 'Queens County', 'Richmond County');

dev <- c();
dev_county = c();
tot <- 0;
county_days = c();
settimana <- c('1mon', '2tue', '3wen', '4thu', '5fri','6sat', '7sun');

for(i in 1: n){
  j <- match(area[i],CensusBlockGroup); #estrae gli indici delle cbg in PatternNY corrispondenti ai CBG nel dataframe census_blocks_ny
  for (l in 1: length(NY_counties)){
    if(County[j] == NY_counties[l]){
      tot <- tot + 1;
      dev_county[((tot*7)-6):(tot*7)] <- rep(NY_counties[l], times = 7);
      lun <- Patterns_NY$stops_by_day[[i]][seq(1,30,7)];
      mar <- Patterns_NY$stops_by_day[[i]][seq(2,30,7)];
      mer <- Patterns_NY$stops_by_day[[i]][seq(3,30,7)];
      gio <- Patterns_NY$stops_by_day[[i]][seq(4,30,7)];
      ven <- Patterns_NY$stops_by_day[[i]][seq(5,30,7)];
      sab <- Patterns_NY$stops_by_day[[i]][seq(6,30,7)];
      dom <- Patterns_NY$stops_by_day[[i]][seq(7,30,7)];
      dev[(tot*7 - 6): (tot*7)] <- c(mean(lun), mean(mar), mean(mer), mean(gio), mean(ven), mean(sab), mean(dom));
      county_days[((tot*7)-6):(tot*7)] <- paste(settimana[1:7], County[j]);
    }
  }
}

days <- rep(settimana, times = tot);

days <- factor(days);
dev_county <- factor(dev_county);
county_days <- factor(county_days);

M <- mean(dev);
Mdays <- tapply(dev, days, mean);
Mcounty <- tapply(dev, dev_county, mean);
Mday_county <- tapply(dev, county_days, mean);

g <- length(levels(days)); #number of days
b <- length(levels(dev_county)); #number of counties
n1 <- length(dev)/(g*b);


#graphical analysis
#x11()
png(file = "NY_nofactor.png")
par(mfrow=c(2,1))
barplot(rep(M,g * b), names.arg=c('','','mon', '','','','','tue', '','','','','wed', '','','','','thu', '','','','','fri', '','','','','sat', '','','','','sun', '','')
, col = 'grey85', main='No factor')
dev.off()
#x11()
png(file = "NY_days.png")
par(mfrow=c(2,1),las=2)
barplot(rep(Mdays,each=b), names.arg=c('','','monday', '','','','','tuesday', '','','','','wednesday', '','','','','thursday', '','','','','friday', '','','','','saturday', '','','','','sunday', '','')
,
        col= rep(coldays[1:7],each=b), main='Only Fact. Days.')
dev.off()
# per fare la legenda comoda
x11()
par(mfrow=c(2,1),las=2)
barplot(c(rep(Mcounty,times=g-4), rep(0, times = 4*5)), names.arg=c('','','monday', '','','','','tuesday', '','','','','wednesday', '','','','','thursday', '','','','','friday', '','','','','saturday', '','','','','sunday', '','')
,
        col=rep(colcountyNY[1:5],times=g), legend = levels(dev_county), main='Only Fact. NY Counties')
#x11()
png(file = "NY_counties.png")
par(mfrow=c(2,1))
barplot(rep(Mcounty,times=g), names.arg=c('','','mon', '','','','','tue', '','','','','wed', '','','','','thu', '','','','','fri', '','','','','sat', '','','','','sun', '','')
,
        col=rep(colcountyNY[1:5], times = g), main='Only Fact. NY Counties')
dev.off()
mm <-c()
for(i in 1:g){
  for (j in 1:b){
    mm[(i-1)*b+j] <- Mdays[i]+Mcounty[j]-M;
  }
}
# x11()
# png(file = "NY_additive.png")
# par(mfrow=c(2,1),las=2)
# barplot(mm, names.arg=levels(county_days), density=rep(10,4), angle=135, 
#        col = rep(viridis(7), each = b), border = colcountyNY[1:5],main='Additivie model Days+County NY') 
# #qua coloro allo stesso modo i giorni, bordo indicica la county
# 
# dev.off()
png(file = "NY_additive.png")
par(mfrow=c(2,1))
barplot(mm, names.arg=c('','','mon', '','','','','tue', '','','','','wed', '','','','','thu', '','','','','fri', '','','','','sat', '','','','','sun', '','')
, 
        col = rep(colcountyNY[1:5], each = b),main='Additivie model Days+County NY') 
#qua coloro allo stesso modo i giorni, bordo indicica la county

dev.off()
#x11()
png(file = "NY_interac.png")
par(mfrow=c(2,1))
barplot(Mday_county,  names.arg=c('','','mon', '','','','','tue', '','','','','wed', '','','','','thu', '','','','','fri', '','','','','sat', '','','','','sun', '','')
,
        col=rep(colcountyNY[1:5], each = b), main='Model with Interact. Days+County NY')
dev.off()
x11()
boxplot(dev ~ days)

x11()
boxplot(dev ~ dev_county)

### Verify the assumptions
# 1) normality (multivariate) in each group (62*7 test ...) -> Ps
Ps <- c();
for (i in 1:(g*b)){
  Ps[i] <- shapiro.test(dev[ county_days==levels(county_days)[i] ])$p;
}
#troppe assurdo ...
Ps
jj <- which(Ps > 0.05);
length(jj) #conta test con pvalue sopra 5% -> in cui non c'è abbastanza info per rifiutare gaussianità fino a livello 5%
Ps[jj]
county_days[jj]
#PRIMA TRA I JJ DAVA ALCUNI DI NY ... ADESSO 0 WHY?

# 2) homogeneity of the covariance (qualitatively)
bartlett.test(dev, county_days)
bartlett.test(dev, dev_county)
#vengono tutti pvalue bassisimi ... strano ma cmq brutto


fit <- aov2.int <- aov(dev ~ days + dev_county + days:dev_county)
summary(fit)
#=> remove the interaction term and estimate the model without interaction
fit.aov2.ad <- aov(dev ~ days + dev_county)
summary.aov(fit.aov2.ad)

fit <- aov(dev ~ dev_county)
summary(fit)

# contea <- 'NY'
# save(CBGi, file = paste(contea,'.Rdata', sep=""))
# save(CBGi, file = paste('BRONX','.Rdata'))
# 
# x11()
# par (mfrow=c(1,2))
# load("~/Documenti/UNIVERSITA/STAT APP/progetto/BRONX .Rdata")
# boxplot(CBGi)
# nome <- paste(contea,'.Rdata', sep="")
# nome <- paste('~/Documenti/UNIVERSITA/STAT APP/progetto/', nome, sep="")
# load(nome)
# boxplot(CBGi)


detach(Patterns_NY)
detach(census_blocks_ny)
