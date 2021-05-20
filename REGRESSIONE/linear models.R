# REGRESSIONE su Manhattan

# Load Libraries
library(car)

# Load dataset and build dataframe
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Prog/Applied-statistics-project/DATASET/Data frame county/New York County.RData")
attach(New_York_County)

LM_1=data.frame(median_dwell,raw_stop_counts,raw_device_counts,distance_from_home,distance_from_primary_daytime_location)
LM_1<-LM_1[-92,]

detach(NY_county)
attach(LM_1)


# vediamo come sono distribuiti i dati
pairs(LM_1) # fanno sboccare

######################### ESEMPI DI REGRESSIONE ###################################

# facciamo un linear model solo con le prime due variabili (risultati molto bassi)
mod=lm(formula = median_dwell ~ raw_stop_counts + raw_device_counts)
summary(mod)


## Dobbiamo trovare una trasformazione che renda il modello più lineare

# 1. decidiamo di fare il boxcox del modello (univariate)
#---------------------------------------------------------------------------------------
b=boxCox(mod)
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
summary(mod_boxcox) #0.38

#---------------------------------------------------------------------------------------------------------
#2. Proviamo a vedere se cambia qualcosa con il Powertransformation di ogni variabile (univariate)
#---------------------------------------------------------------------------------------------------------
lambda.raw_stop <- powerTransform(raw_stop_counts)
lambda.raw_device <- powerTransform(raw_device_counts)
lambda.median <- powerTransform(median_dwell)

bc.raw_stop <- bcPower(raw_stop_counts, lambda.raw_stop$lambda)
bc.raw_device <- bcPower(raw_device_counts, lambda.raw_device$lambda)
bc.median <- bcPower(median_dwell, lambda.median$lambda)

mod_power=lm(formula = bc.median ~ bc.raw_stop + bc.raw_device)
summary(mod_power) #0.475

mod_power=lm(formula = median_dwell ~ bc.raw_stop + bc.raw_device)
summary(mod_power) #0.496

mod_power=lm(formula = bc.median ~ raw_stop_counts + raw_device_counts)
summary(mod_power) #0.03
#------------------------------------------------------------------------------------------------------------
#3. Multivariate case
#------------------------------------------------------------------------------------------------------------
lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,median_dwell))
lambda_multivariate

BC.stop <- bcPower(raw_stop_counts, 0)
BC.device <- bcPower(raw_device_counts, 0)
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[3])

mod_multivariate=lm(formula = BC.median ~ BC.stop + BC.device)
summary(mod_multivariate) #0.69

vif(mod_multivariate) # non c'è collinearità
 
# #questo ultimo metodo è quello che mi da un R^2 maggiore 
# da capire il significato di coeff positivi e negativi
#------------------------------------------------------------------------------------------------------------
#4. Provo con l'ultimo modello aggiungendo features
#------------------------------------------------------------------------------------------------------------
lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,distance_from_home,distance_from_primary_daytime_location,median_dwell))
lambda_multivariate
BC.stop <- bcPower(raw_stop_counts, 0)
BC.device <- bcPower(raw_device_counts, 0)
BC.home <- bcPower(distance_from_home, lambda_multivariate$lambda[3])
BC.primary <- bcPower(distance_from_primary_daytime_location, lambda_multivariate$lambda[4])
BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[5])

mod_multivariate_complete=lm(formula = BC.median ~ BC.stop + BC.device + BC.home + BC.primary)
summary(mod_multivariate_complete) #0.69

# Vediamo che le ultime due features non sembrano essere significative. dal pairs avevamo visto che sono correlate. 
# Risolviamo il problema della collinearità

vif(mod_multivariate_complete) # infatti abbiamo vif = 36, da 10 in poi sono collineari





















#------------------------------------------------------------------------------------------------------------
# 1 approccio: PCA - PC4 spiega il modello, da chiedere a mara.
#------------------------------------------------------------------------------------------------------------
LM.PCA <- princomp(cbind(BC.stop,BC.device,BC.home,BC.primary), scores=TRUE)
summary(LM.PCA)
LM.PCA$load

PC1 <- LM.PCA$scores[,1]
PC2 <- LM.PCA$scores[,2]
PC3 <- LM.PCA$scores[,3]
PC4 <- LM.PCA$scores[,4]
# first component explains all data more or less
# scores are used as new betas

# Now we estimate the model by inserting the PCs instead of the 
# original regressors 
# Model: y = b0 + b1*PC1+ b2*PC2 + eps, eps~N(0,sigma^2)
lm.pca1 <- lm(BC.median ~ PC4)

summary(lm.pca1) 

#--------------------------------------------------------------------------------------------------------------
# 2 approccio: 

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
   





