

# Loading libraries
library(car)

# Loading  
load("New York County.RData")
NY_county <- sub_patt
rm(sub_patt)
attach(NY_county)

LM_1=data.frame(median_dwell,raw_stop_counts,raw_device_counts,distance_from_home,distance_from_primary_daytime_location)

detach(NY_county)
attach(LM_1)


## vediamo come sono distribuiti i dati (male)
pairs(LM_1)


##facciamo un linear model solo con le prime due variabili (risultati molto bassi)
mod=lm(formula = median_dwell ~ raw_stop_counts + raw_device_counts)
summary(mod)


##Dobbiamo trovare una trasformazione che renda il modello più lineare

#1. decidiamo di fare il boxcox del modello (univariate)
b=boxcox(mod)
best_lambda_ind=which.max(b$y)
lambda=b$x[best_lambda_ind]

box_cox <- function(x,lambda)
{
  if(lambda!=0)
    return((x^lambda-1)/lambda)
  return(log(x))
}

new_raw_stop_counts = box_cox(raw_stop_counts,lambda)
new_raw_device_counts = box_cox(raw_device_counts,lambda)
new_median_dwell = box_cox(median_dwell,lambda)

mod_boxcox=lm(formula = new_median_dwell ~ new_raw_stop_counts + new_raw_device_counts)
summary(mod_boxcox) #0.39


#2. Proviamo a vedere se cambia qualcosa con il Powertransformation di ogni variabile (univariate)

lambda.raw_stop <- powerTransform(raw_stop_counts)
lambda.raw_device <- powerTransform(raw_device_counts)
lambda.median <- powerTransform(median_dwell)

bc.raw_stop <- bcPower(raw_stop_counts, lambda.raw_stop$lambda)
bc.raw_device <- bcPower(raw_device_counts, lambda.raw_device$lambda)
bc.median <- bcPower(median_dwell, lambda.median$lambda)

mod_power=lm(formula = bc.median ~ bc.raw_stop + bc.raw_device)
summary(mod_power)

mod_power=lm(formula = median_dwell ~ bc.raw_stop + bc.raw_device)
summary(mod_power) #0.29

mod_power=lm(formula = bc.median ~ raw_stop_counts + raw_device_counts)
summary(mod_power) #0.09

mod_power=lm(formula = bc.median ~ bc.raw_stop + bc.raw_device)
summary(mod_power) #0.33

#il miglior risultato ci viene con il boxcox e abbiamo R^2=0.39

#3. Multivariate case
lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,median_dwell))

BC.stop <- bcPower(raw_stop_counts, lambda_multivariate$lambda[1])
BC.device <- bcPower(raw_device_counts, lambda_multivariate$lambda[2])
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[3])

mod_multivariate=lm(formula = BC.median ~ BC.stop + BC.device)
summary(mod_multivariate)
# 
# 
# #questo ultimo metodo è quello che mi da un R^2 maggiore 

#4. Provo con questo metodo ad aggiungere features

lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,distance_from_home,distance_from_primary_daytime_location,median_dwell))

BC.stop <- bcPower(raw_stop_counts, 0)
BC.device <- bcPower(raw_device_counts, 0) #capire se tenere il reale o metterlo a zero
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[5])
BC.home <- bcPower(distance_from_home, 0)
BC.primary <- bcPower(distance_from_primary_daytime_location, 0)

mod_multivariate_complete=lm(formula = BC.median ~ BC.stop + BC.device + BC.home)
summary(mod_multivariate_complete) #0.62

#le ultime due features sono correlate come avevamo visto dal pairs quindi a priori dovremmo togliene una delle 2. 
#abbiamo verificato che se togliamo l'ultima variabile R^2 non cambia di molto quindi possiamo toglierla

####dobbiamo fare un test statistico che ci dica che le 2 variabili siano uguali

##togliamo gli outlier 
# 
# #1. leverages
# lev=hatvalues(mod_multivariate_complete)
# x11()
# plot( mod_multivariate_complete$fitted.values, lev,xlab='Fitted values', ylab = "Leverages", main = "Plot of Leverages", pch = 16, col = 'black' )
# p=mod_multivariate_complete$rank
# n=dim(LM_1)[1]
# abline( h = 2 * p/n, lty = 2, col = 'red' )
# watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
# watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
# points( mod_multivariate_complete$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'blue', pch = 16 )
# 
# mod_multivariate_complete_lev = lm(formula = BC.median ~ BC.stop + BC.device + BC.home, subset = ( lev < 2*p/n ) )
# summary(mod_multivariate_complete_lev) #mi sembra che togliamo un sacco di dati e non cambia R^2, sempre 0.61
# 
# #serve per vedere se togliendo i lev cambia di molto a lui viene 22% (che è tanto)
# abs((mod_multivariate_complete$coefficients-mod_multivariate_complete_lev$coefficients)/mod_multivariate_complete$coefficients)
# 
# colors = rep( 'black', nrow(LM_1) )
# colors[ watchout_ids_lev ] = 'blue'
# pairs( LM_1 , pch=16,col=colors,cex=1 )
# 
# #da qua sembra che togliere i leverages non servi a nulla però c'è qualcosa che non torna facendo il pairs
# #perchè quelli che sono evidentemente outliers non vengono tolti
# 
# 
# #2. facciamo la cook distance
# Cdist=cooks.distance(mod_multivariate_complete)
# watchout_ids_Cdist=which(Cdist>4/(n-p))
# watch_Cdist=Cdist[watchout_ids_Cdist]
# watch_Cdist
# id_to_keep=!(1:n %in% watchout_ids_Cdist)
# 
# colors = rep( 'black', nrow(LM_1) )
# colors[ watchout_ids_Cdist ] = 'orange'
# pairs( LM_1, pch=16,col=colors,cex=1+0.5*as.numeric(colors!='black') )
# #simile a lev
# 
# plot(mod_multivariate_complete$fitted.values,Cdist,pch=16,xlab='Fitted values',ylab='Cooks distance', main='Cooks distance')
# points(mod_multivariate_complete$fitted.values[watchout_ids_Cdist],Cdist[watchout_ids_Cdist],col='orange',pch=16)
# 
# mod_multivariate_complete_cook=lm(formula = BC.median ~ BC.stop + BC.device + BC.home, LM_1[id_to_keep,])
# abs((mod_multivariate_complete$coefficients-mod_multivariate_complete_cook$coefficients)/mod_multivariate_complete$coefficients)
# summary(mod_multivariate_complete_cook)
# #non cambia assolutamente nulla
# 


weekday_home=integer(dim(Patterns_NY)[1])
 
for (i in 1:dim(Patterns_NY)[1]) {
     for (j in 1: dim (Patterns_NY) [1] ){
         if( is.na( weekday_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
           weekday_home[i]=weekday_home[i] + weekday_device_home_areas [[j]] [area[i]]
         }
     }
 }
   





