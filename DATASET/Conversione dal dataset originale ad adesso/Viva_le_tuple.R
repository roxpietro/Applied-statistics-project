# Set Working Directory
setwd("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto")

# Load Dataset
load("Patterns_NY.RData")


attach(Patterns_NY)


for (i in 1:15446){
    if (length(device_home_areas[[i]])!=0){
      disp=seq(1,length(device_home_areas[[i]]),by=2)
      par=seq(2,length(device_home_areas[[i]]),by=2)
      n_devices<-c(device_home_areas[[i]][par])
      CBG<-c(device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$device_home_areas[[i]]<-n_devices
    }

  if (length(weekday_device_home_areas[[i]])!=0){
      disp=seq(1,length(weekday_device_home_areas[[i]]),by=2)
      par=seq(2,length(weekday_device_home_areas[[i]]),by=2)
      n_devices<-c(weekday_device_home_areas[[i]][par])
      CBG<-c(weekday_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$weekday_device_home_areas[[i]]<-n_devices
  }

  if (length(weekend_device_home_areas[[i]])!=0){
      disp=seq(1,length(weekend_device_home_areas[[i]]),by=2)
      par=seq(2,length(weekend_device_home_areas[[i]]),by=2)
      n_devices<-c(weekend_device_home_areas[[i]][par])
      CBG<-c(weekend_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$weekend_device_home_areas[[i]]<-n_devices
    }

  if (length(breakfast_device_home_areas[[i]])!=0){
      disp=seq(1,length(breakfast_device_home_areas[[i]]),by=2)
      par=seq(2,length(breakfast_device_home_areas[[i]]),by=2)
      n_devices<-c(breakfast_device_home_areas[[i]][par])
      CBG<-c(breakfast_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$breakfast_device_home_areas[[i]]<-n_devices
    }

  if (length(lunch_device_home_areas[[i]])!=0){
      disp=seq(1,length(lunch_device_home_areas[[i]]),by=2)
      par=seq(2,length(lunch_device_home_areas[[i]]),by=2)
      n_devices<-c(lunch_device_home_areas[[i]][par])
      CBG<-c(lunch_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$lunch_device_home_areas[[i]]<-n_devices
    }

  if (length(afternoon_tea_device_home_areas[[i]])!=0){
      disp=seq(1,length(afternoon_tea_device_home_areas[[i]]),by=2)
      par=seq(2,length(afternoon_tea_device_home_areas[[i]]),by=2)
      n_devices<-c(afternoon_tea_device_home_areas[[i]][par])
      CBG<-c(afternoon_tea_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$afternoon_tea_device_home_areas[[i]]<-n_devices
    }

  if (length(dinner_device_home_areas[[i]])!=0){
      disp=seq(1,length(dinner_device_home_areas[[i]]),by=2)
      par=seq(2,length(dinner_device_home_areas[[i]]),by=2)
      n_devices<-c(dinner_device_home_areas[[i]][par])
      CBG<-c(dinner_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$dinner_device_home_areas[[i]]<-n_devices
    }

  if (length(nightlife_device_home_areas[[i]])!=0){
      disp=seq(1,length(nightlife_device_home_areas[[i]]),by=2)
      par=seq(2,length(nightlife_device_home_areas[[i]]),by=2)
      n_devices<-c(nightlife_device_home_areas[[i]][par])
      CBG<-c(nightlife_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$nightlife_device_home_areas[[i]]<-n_devices
    }
 
  if (length(work_hours_device_home_areas[[i]])!=0){
      disp=seq(1,length(work_hours_device_home_areas[[i]]),by=2)
      par=seq(2,length(work_hours_device_home_areas[[i]]),by=2)
      n_devices<-c(work_hours_device_home_areas[[i]][par])
      CBG<-c(work_hours_device_home_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$work_hours_device_home_areas[[i]]<-n_devices
  }

  if (length(device_daytime_areas[[i]])!=0){
      disp=seq(1,length(device_daytime_areas[[i]]),by=2)
      par=seq(2,length(device_daytime_areas[[i]]),by=2)
      n_devices<-c(device_daytime_areas[[i]][par])
      CBG<-c(device_daytime_areas[[i]][disp])
      names(n_devices)<-CBG
      Patterns_NY$device_daytime_areas[[i]]<-n_devices
    }
}
save(Patterns_NY, file = "Patterns_NY.RData")
detach(Patterns_NY)
