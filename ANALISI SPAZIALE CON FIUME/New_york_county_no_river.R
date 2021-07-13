### NYC senza river ###

# Fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA 


track_code_river <- c("027500", "025500", "024700", "024100", "023700", "023300", "022900", "022500", "022301", "021900","021100","020500", "019900", "019500",
                      "019100", "018700", "018300", "017900", "017500", "017100","016700", "016300", "015900", "015500", "015100", "013500", "012900", "011700", "009900", "007900", "007500", "006900", "003700",
                      "003900", "031703", "031704","031900", "000500", "000900","000700", "001502", "001501", "002500", "000800","000600","000201", "000202", "001001", "001002", "002000", "002400", "004400",
                      "006000", "006200", "008601", "008602", "023801", "010601","010602","011600", "012400", "013200", "013600", "023802", "015200","017800", "019200", "024200", "021000", "023600", "024302", "031100",
                      "029900", "029700", "028700", "022302", "016200", "024000")

CBG_river <-paste("36061", track_code_river, sep = "")
CBG_river <-paste(CBG_river, "0", sep = "")

remove=c()
k=1
for (i in 1:dim(New_York_County)[1]) {
  if (New_York_County$area[i] %in% CBG_river) {
    remove[k]=i
    k=k+1
  }
    
}

New_York_County_no_river<-New_York_County[-remove,]
save (New_York_County_no_river, file="NYC_no_river.RData")

