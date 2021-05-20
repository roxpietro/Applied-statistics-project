load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Prog/Applied-statistics-project/DATASET/Conversione dal dataset originale ad adesso/Patterns_NY.RData")
attach(Patterns_NY)


#---------------------------------------------------------------

weekday_home=integer(dim(Patterns_NY)[1])

for (i in 1:dim(Patterns_NY)[1]) {
  for (j in 1: dim (Patterns_NY) [1] ){
    if( is.na( weekday_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      weekday_home[i]=weekend_home[i] + weekday_device_home_areas [[j]] [area[i]]
    }
  }
}

save(weekday_home, file="weekday_home_tot.rda")

#---------------------------------------------------------------

#---------------------------------------------------------------

weekend_home=integer(dim(Patterns_NY)[1])

for (i in 1:dim(Patterns_NY)[1]) {
  for (j in 1: dim (Patterns_NY) [1] ){
    if( is.na( weekend_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      weekend_home[i]=weekend_home[i] + weekend_device_home_areas [[j]] [area[i]]
    }
  }
}

save(weekend_home, file="weekend_home_tot.rda")

#---------------------------------------------------------------

breakfast=integer(dim(Patterns_NY)[1])

for (i in 1:dim(Patterns_NY)[1]) {
  for (j in 1: dim (Patterns_NY) [1] ){
    if( is.na( breakfast_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      breakfast[i]=breakfast[i] + breakfast_device_home_areas [[j]] [area[i]]
    }
  }
}

save(breakfast,file= "breakfast_tot.rda")

#----------------------------------------------------------------

lunch=integer(dim(Patterns_NY)[1])

for (i in 1:dim(Patterns_NY)[1]) {
  for (j in 1: dim (Patterns_NY) [1] ){
    if( is.na( lunch_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      lunch[i]=lunch[i] + lunch_device_home_areas [[j]] [area[i]]
    }
  }
}

save(lunch,file= "lunch_tot.rda")

#------------------------------------------------------------------

tea=integer(dim(Patterns_NY)[1])

for (i in 1:dim(Patterns_NY)[1]) {
  for (j in 1: dim (Patterns_NY) [1] ){
    if( is.na( afternoon_tea_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      tea[i]=tea[i] + afternoon_tea_device_home_areas [[j]] [area[i]]
    }
  }
}

save(tea, file="tea_tot.rda")

#------------------------------------------------------------------

dinner=integer(dim(Patterns_NY)[1])

for (i in 1:dim(Patterns_NY)[1]) {
  for (j in 1: dim (Patterns_NY) [1] ){
    if( is.na( dinner_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      dinner[i]=dinner[i] + dinner_device_home_areas [[j]] [area[i]]
    }
  }
}

save(dinner,file= "dinner_tot.rda")

#-------------------------------------------------------------------

night=integer(dim(Patterns_NY)[1])

for (i in 1:dim(Patterns_NY)[1]) {
  for (j in 1: dim (Patterns_NY) [1] ){
    if( is.na( nightlife_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      night[i]=night[i] + nightlife_device_home_areas [[j]] [area[i]]
    }
  }
}

save(night, file="night_tot.rda")

