#descriptive analysis

setwd("C:/Users/filippo/Desktop/Università/Magistrale/Applied Statistics/Progetto")
load("C:/Users/filippo/Desktop/Università/Magistrale/Applied Statistics/Progetto/Patterns_NY.RData")
load("C:/Users/filippo/Desktop/Università/Magistrale/Applied Statistics/Progetto/Cyber_Capital.RData")

attach(Patterns_NY)
attach(census_blocks_ny)


n <- dim(Patterns_NY)[1];
g1 <- length(Patterns_NY$day_counts[[1]]);
g2 <- 2;

#1st: grouped all CBGs MEAN n. of devices in 7 groups corresponding to week days (Mon-Sun)
lunm = replicate(n, 0);
marm= replicate(n, 0);
merm= replicate(n, 0);
giom = replicate(n, 0);
venm = replicate(n, 0);
sabm = replicate(n, 0);
domm = replicate(n, 0);
dev = replicate(n*7, 0);
dev_county = replicate(n*7, 0);

for(i in 1: n){
  #cbg[i] <- Patterns_NY$area[[i]];
  lun <- Patterns_NY$stops_by_day[[i]][seq(1,30,7)];
  mar <- Patterns_NY$stops_by_day[[i]][seq(2,30,7)];
  mer <- Patterns_NY$stops_by_day[[i]][seq(3,30,7)];
  gio <- Patterns_NY$stops_by_day[[i]][seq(4,30,7)];
  ven <- Patterns_NY$stops_by_day[[i]][seq(5,30,7)];
  sab <- Patterns_NY$stops_by_day[[i]][seq(6,30,7)];
  dom <- Patterns_NY$stops_by_day[[i]][seq(7,30,7)];
  
  lunm[i] <- mean(lun);
  marm[i] <- mean(mar);
  merm[i] <- mean(mer);
  giom[i] <- mean(gio);
  venm[i] <- mean(ven);
  sabm[i] <- mean(sab);
  domm[i] <- mean(dom);
  
  dev[((i*7)-6):(i*7)] <- c(lunm[i], marm[i], merm[i], giom[i], venm[i], sabm[i], domm[i]);
  j <- match(area[i],CensusBlockGroup);
  dev_county[((i*7)-6):(i*7)] <- replicate(7,County[j]);
}

first_data <-data.frame(
  #CBG = cbg,
  LUNmean = lunm,
  MARmean = marm,
  MERmean = merm,
  GIOmean = giom,
  VENmean = venm,
  SABmean = sabm,
  DOMmean = domm
)

settimana <- c('mon', 'tue', 'wen', 'thu', 'fri', 'sat', 'sun');
days <- replicate(n,settimana);

# day_county <- data.frame(
#   mean_dev = dev,
#   county = dev_county,
#   day = days
# )
#non funziona perchè richiede troppa memoria : Errore: cannot allocate vector of size 845 Kb
save(first_data, file = "first_data.RData")

#PROVEREI PRIMA DI VEDERE SE C'è UN MODO PER FARE QUESTO MACRO GIGA DATAFRAME(CHE MAGARI NON PORTA A NULA ...)
#A FARE ANOVA SOLO SUI GRUPPI GIORNI, SALVANDO SOLO I CBG DI 1 COUNTY (COSì COME PENSAVAMO PRIMA)

#2nd: grouped all CBGs MEAN n.of devices in work days (Mon-Fri) and ween-end days(Sat-Sun)

cbg <- replicate(n, 0)
work <- replicate(n, 0)
weekend <- replicate(n, 0)
tot <- 0.0

for(i in 1:n){
  cbg[i] <- Patterns_NY$area[[i]];
  tot = c(Patterns_NY$stops_by_day[[i]][seq(1,30,7)], Patterns_NY$stops_by_day[[i]][seq(2,30,7)],
          Patterns_NY$stops_by_day[[i]][seq(3,30,7)], Patterns_NY$stops_by_day[[i]][seq(4,30,7)],
          Patterns_NY$stops_by_day[[i]][seq(5,30,7)]);
  work[i] = mean(tot);
  
  tot <- c(Patterns_NY$stops_by_day[[i]][seq(6,30,7)], Patterns_NY$stops_by_day[[i]][seq(7,30,7)]);
  weekend[i] <- mean(tot);
}

second_data <- data.frame(
  WORKmean = work,
  WEEKENDmean = weekend
)
save(second_data, file = "second_data.RData")

attach(first_data)
x11()
boxplot(first_data) #bleaaaaah
attach(second_data)
x11()
boxplot(second_data) #bleaaaaah

#for each CBG groups week days
x11()
par (mfrow=c(5,2))

for (i in 1:10){
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



#4th: for each contea, n.devices grouped in 7 days of the week
contea <- data.frame(
  LUN = lun,
  MAR = mar,
  MER = mer,
  GIO = gio,
  VEN = ven,
  SAB = sab,
  DOM = dom
)

list_county=c(County[[1]]);
for (i in 2:15446){
  list_county=c(list_county, County[[i]] );
  list_county = unique(list_county);
}

contea <- 'NY'
save(CBGi, file = paste(contea,'.Rdata', sep=""))
save(CBGi, file = paste('BRONX','.Rdata'))

x11()
par (mfrow=c(1,2))
load("~/Documenti/UNIVERSITA/STAT APP/progetto/BRONX .Rdata")
boxplot(CBGi)
nome <- paste(contea,'.Rdata', sep="")
nome <- paste('~/Documenti/UNIVERSITA/STAT APP/progetto/', nome, sep="")
load(nome)
boxplot(CBGi)


detach(Patterns_NY)
detach(census_blocks_ny)