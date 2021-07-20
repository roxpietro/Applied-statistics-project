setwd("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/ANALISI FUNZIONALE STOPS")

load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/NYC_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/CBG_NY_no_river.RData")
load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/River_Dataset.RData")

New_York_County_no_river= New_York_County_no_river[order( New_York_County_no_river$area),]
CBG_ny_no_river<-CBG_ny_no_river[order( CBG_ny_no_river$CensusBlockGroup),]
attach( New_York_County_no_river)

stops<-matrix(nrow = 1092, ncol=30*24)

for (i in 1:dim( New_York_County_no_river)[1]) {
  stops[i,]<-stops_by_each_hour[[i]]
}

detach( New_York_County_no_river)

stops<-stops[,505:672]

stops<-t(stops)
colnames(stops)<- New_York_County_no_river$area
x11()
matplot(stops,type='l', main = "22/06 - 28/06")
#saltiamo smoothing per ora ...

# outliers
out=c(92,532)
cbg_out=New_York_County_no_river[out,1]
layout(1)
x11()
matplot(stops,type='l')
lines(stops[,92],lwd=4, col=2) 
lines(stops[,532],lwd=4, col=1) 

out1 <- New_York_County_no_river[out[1],]
out2 <- New_York_County_no_river[out[2],]

days<-matrix(nrow = 7, ncol=24)
days[1,]<-out1$popularity_by_hour_monday[[1]]
days[2,]<-out1$popularity_by_hour_tuesday[[1]]
days[3,]<-out1$popularity_by_hour_wednesday[[1]]
days[4,]<-out1$popularity_by_hour_thursday[[1]]
days[5,]<-out1$popularity_by_hour_friday[[1]]
days[6,]<-out1$popularity_by_hour_saturday[[1]]
days[7,]<-out1$popularity_by_hour_sunday[[1]]
days<-t(days)
my_col <- rainbow(7)
x11()
matplot(days,type='l', col = my_col) #7 giorni settimanali, già fatta la media dentro popularity ...
legend("topleft", legend= c('mon', 'tue','wed', 'thu', 'fri', 'sat', 'sun'), fill = my_col)

lun <- matrix(nrow = 5, ncol=24)
for (i in 1:5){
  inizio <- seq(1,720,24*7)[i]
  fine <- seq(24,720,24*7)[i]
  lun[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
lun[4,]
mar <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(25,720,24*7)[i]
  fine <- seq(48,720,24*7)[i]
  mar[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
mer <- matrix(nrow = 4, ncol=24)
for (i in 1:5){
  inizio <- seq(49,720,24*7)[i]
  fine <- seq(72,720,24*7)[i]
  mer[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
gio <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(73,720,24*7)[i]
  fine <- seq(96,720,24*7)[i]
  gio[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
ven <- matrix(nrow = 4, ncol=24)
for (i in 1:5){
  inizio <- seq(97,720,24*7)[i]
  fine <- seq(120,720,24*7)[i]
  ven[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
sab <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(121,720,24*7)[i]
  fine <- seq(144,720,24*7)[i]
  sab[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}
dom <- matrix(nrow = 4, ncol=24)
for (i in 1:4){
  inizio <- seq(145,720,24*7)[i]
  fine <- seq(168,720,24*7)[i]
  dom[i,] <- out1$stops_by_each_hour[[1]][inizio:fine]
}

lun <- t(lun)
mar <- t(mar)
mer <- t(mer)
gio <- t(gio)
ven <- t(ven)
sab <- t(sab)
dom <- t(dom)

library(RColorBrewer)
col = brewer.pal(n = 8, name = 'YlOrRd')
x11()
matplot(lun,type='l', col = col[3:8], lwd =2)

col = brewer.pal(n = 8, name = 'YlGnBu')
x11()
matplot(mar,type='l', col = col[4:8],  lwd =2)

col = brewer.pal(n = 8, name = 'YlGn')
x11()
matplot(mer,type='l', col = col[4:8], lwd=2)

col = brewer.pal(n = 8, name = 'RdPu')
x11()
matplot(gio,type='l', col = col[4:8], lwd=2)

col = brewer.pal(n = 8, name = 'OrRd')
x11()
matplot(ven,type='l', col = col[4:8], lwd=2)

col = brewer.pal(n = 8, name = 'Greens')
x11()
matplot(sab,type='l', col = col[4:8], lwd=2)

col = brewer.pal(n = 8, name = 'Blues')
x11()
matplot(dom,type='l', col = col[4:8], lwd=2)
detach(New_York_County_no_river)