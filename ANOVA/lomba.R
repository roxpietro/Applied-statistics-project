setwd("C:/Users/filippo/Desktop/Università/Magistrale/Applied Statistics/Progetto")
load("C:/Users/filippo/Desktop/Università/Magistrale/Applied Statistics/Progetto/Patterns_NY.RData")
attach(Patterns_NY)
attach(census_blocks_ny)

lun_med = c();
mar_med = c();
mer_med = c();
gio_med = c();
ven_med = c();
sab_med = c();
dom_med = c();
for (i in 1:15446) {
  lun_sum = 0.0;
  for (j in 0:4) {
    lun_sum = lun_sum + Patterns_NY$stops_by_day[[i]][j*7+1];
  }
  lun_sum = lun_sum / 5.0;
  lun_med = c(lun_med, lun_sum);
  
  mar_sum = 0.0;
  for (j in 0:4) {
    mar_sum = mar_sum + Patterns_NY$stops_by_day[[i]][j*7+2];
  }
  mar_sum = mar_sum / 5.0;
  mar_med = c(mar_med, mar_sum);
  
  mer_sum = 0.0;
  for (j in 0:3) {
    mer_sum = mer_sum + Patterns_NY$stops_by_day[[i]][j*7+3];
  }
  mer_sum = mer_sum / 4.0;
  mer_med = c(mer_med, mer_sum);
  
  gio_sum = 0.0;
  for (j in 0:3) {
    gio_sum = gio_sum + Patterns_NY$stops_by_day[[i]][j*7+4];
  }
  gio_sum = gio_sum / 4.0;
  gio_med = c(gio_med, gio_sum);
  
  ven_sum = 0.0;
  for (j in 0:3) {
    ven_sum = ven_sum + Patterns_NY$stops_by_day[[i]][j*7+5];
  }
  ven_sum = ven_sum / 4.0;
  ven_med = c(ven_med, ven_sum);
  
  sab_sum = 0.0;
  for (j in 0:3) {
    sab_sum = sab_sum + Patterns_NY$stops_by_day[[i]][j*7+6];
  }
  sab_sum = sab_sum / 4.0;
  sab_med = c(sab_med, sab_sum);
  
  dom_sum = 0.0;
  for (j in 0:3) {
    dom_sum = dom_sum + Patterns_NY$stops_by_day[[i]][j*7+7];
  }
  dom_sum = dom_sum / 4.0;
  dom_med = c(dom_med, dom_sum);
}



first_data <-data.frame(
  CBG = Patterns_NY$area,
  LUNmean = lun_med,
  MARmean = mar_med,
  MERmean = mer_med,
  GIOmean = gio_med,
  VENmean = ven_med,
  SABmean = sab_med,
  DOMmean = dom_med
)


list_county=c(County[[1]]);
for (i in 2:15446){
  list_county=c(list_county, County[[i]] );
  list_county = unique(list_county);
}
