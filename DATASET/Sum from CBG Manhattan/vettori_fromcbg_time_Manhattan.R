load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Prog/Applied-statistics-project/DATASET/Data frame county/New York County.RData")
attach(New_York_County)

#---------------------------------------------------------------

weekend_home=integer(dim(New_York_County)[1])

for (i in 1:dim(New_York_County)[1]) {
  for (j in 1: dim (New_York_County) [1] ){
    if( is.na( weekend_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      weekend_home[i]=weekend_home[i] + weekend_device_home_areas [[j]] [area[i]]
    }
  }
}

save(weekend_home, file="weekend_home.rda")

#---------------------------------------------------------------

breakfast=integer(dim(New_York_County)[1])

for (i in 1:dim(New_York_County)[1]) {
  for (j in 1: dim (New_York_County) [1] ){
    if( is.na( breakfast_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      breakfast[i]=breakfast[i] + breakfast_device_home_areas [[j]] [area[i]]
    }
  }
}

save(breakfast,file= "breakfast.rda")

#----------------------------------------------------------------

lunch=integer(dim(New_York_County)[1])

for (i in 1:dim(New_York_County)[1]) {
  for (j in 1: dim (New_York_County) [1] ){
    if( is.na( lunch_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      lunch[i]=lunch[i] + lunch_device_home_areas [[j]] [area[i]]
    }
  }
}

save(lunch,file= "lunch.rda")

#------------------------------------------------------------------

tea=integer(dim(New_York_County)[1])

for (i in 1:dim(New_York_County)[1]) {
  for (j in 1: dim (New_York_County) [1] ){
    if( is.na( afternoon_tea_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      tea[i]=tea[i] + afternoon_tea_device_home_areas [[j]] [area[i]]
    }
  }
}

save(tea, file="tea.rda")

#------------------------------------------------------------------

dinner=integer(dim(New_York_County)[1])

for (i in 1:dim(New_York_County)[1]) {
  for (j in 1: dim (New_York_County) [1] ){
    if( is.na( dinner_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      dinner[i]=dinner[i] + dinner_device_home_areas [[j]] [area[i]]
    }
  }
}

save(dinner,file= "dinner.rda")

#-------------------------------------------------------------------

night=integer(dim(New_York_County)[1])

for (i in 1:dim(New_York_County)[1]) {
  for (j in 1: dim (New_York_County) [1] ){
    if( is.na( nightlife_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
      night[i]=night[i] + nightlife_device_home_areas [[j]] [area[i]]
    }
  }
}

save(night, file="night.rda")

detach(New_York_County)
