

# Loading libraries
library(car)

# Loading 

# fra
load("C:/Users/franc/Desktop/PoliMI/Anno Accademico 2020-2021/Applied Statistics/Progetto/Applied-statistics-project/DATASET/Data frame county/New York County.RData") # FRA 

# terri
#load("/home/terri/Documenti/UNIVERSITA/STAT APP/progetto/gitcode/Applied-statistics-project/DATASET/Data frame county/New York County.RData") #TERRI

New_York_County<-New_York_County[-92,]
attach(New_York_County)

LM_1=data.frame(median_dwell,raw_stop_counts,raw_device_counts,distance_from_home,distance_from_primary_daytime_location)

detach(New_York_County)
attach(LM_1)


## vediamo come sono distribuiti i dati 
pairs(LM_1) #(male)

################## PRIMO MODELLO CON 2 FEATURES ################################

# # facciamo un linear model solo con le prime due variabili (risultati molto bassi)
# mod=lm(formula = median_dwell ~ raw_stop_counts + raw_device_counts)
# summary(mod)
# 
# # Dobbiamo trovare una trasformazione che renda il modello pi? lineare
# 
# #-------------------------------------------------------------------------------
# #1. decidiamo di fare il boxcox del modello (univariate)
# #-------------------------------------------------------------------------------
# 
# b=boxCox(mod)
# best_lambda_ind=which.max(b$y)
# lambda=b$x[best_lambda_ind]
# 
# box_cox <- function(x,lambda)
# {
#   if(lambda!=0)
#     return((x^lambda-1)/lambda)
#   return(log(x))
# }
# 
# new_raw_stop_counts = box_cox(raw_stop_counts,lambda)
# new_raw_device_counts = box_cox(raw_device_counts,lambda)
# new_median_dwell = box_cox(median_dwell,lambda)
# 
# mod_boxcox=lm(formula = new_median_dwell ~ new_raw_stop_counts + new_raw_device_counts)
# summary(mod_boxcox) #0.38
# 
# #-------------------------------------------------------------------------------
# #2. Proviamo a vedere se cambia qualcosa con il Powertransformation di ogni variabile (univariate)
# #--------------------------------------------------------------------------------
# lambda.raw_stop <- powerTransform(raw_stop_counts)
# lambda.raw_device <- powerTransform(raw_device_counts)
# lambda.median <- powerTransform(median_dwell)
# 
# bc.raw_stop <- bcPower(raw_stop_counts, lambda.raw_stop$lambda)
# bc.raw_device <- bcPower(raw_device_counts, lambda.raw_device$lambda)
# bc.median <- bcPower(median_dwell, lambda.median$lambda)
# 
# mod_power=lm(formula = bc.median ~ bc.raw_stop + bc.raw_device)
# summary(mod_power) #0.475
# 
# mod_power=lm(formula = median_dwell ~ bc.raw_stop + bc.raw_device)
# summary(mod_power) #0.496
# 
# mod_power=lm(formula = bc.median ~ raw_stop_counts + raw_device_counts)
# summary(mod_power) #0.03
# 
# mod_power=lm(formula = bc.median ~ bc.raw_stop + bc.raw_device)
# summary(mod_power) #0.478
# 
# #----------------------------------------------------------------------------
# #3. Multivariate case
# #----------------------------------------------------------------------------
# lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,median_dwell))
# lambda_multivariate
# 
# BC.stop <- bcPower(raw_stop_counts, 0)
# BC.device <- bcPower(raw_device_counts, 0)
# BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[3])
# 
# mod_multivariate=lm(formula = BC.median ~ BC.stop + BC.device)
# summary(mod_multivariate) #0.692
# 
# ######################################### SECONDO MODELLO: AGGIUNGO FEATURES ##########################################
# 
# #4. Provo con questo metodo ad aggiungere features
# 
# lambda_multivariate <- powerTransform(cbind(raw_stop_counts, raw_device_counts,distance_from_home,distance_from_primary_daytime_location,median_dwell))
# lambda_multivariate
# 
# BC.stop <- bcPower(raw_stop_counts, 0)
# BC.device <- bcPower(raw_device_counts, 0)
# BC.home <- bcPower(distance_from_home, lambda_multivariate$lambda[3])
# BC.primary <- bcPower(distance_from_primary_daytime_location, lambda_multivariate$lambda[4])
# BC.median <- bcPower(median_dwell, lambda_multivariate$lambda[5])
# 
# mod_multivariate_complete=lm(formula = BC.median ~ BC.stop + BC.device + BC.primary + BC.home)
# summary(mod_multivariate_complete) #0.6932
# 
# vif(mod_multivariate_complete) # c'è collinearità
# 
# LM_2 <- data.frame(BC.stop,BC.device ,BC.home,BC.primary,median_dwell )
# pairs(LM_2)
# 
# par(mfrow=c(2,2))
# plot(mod_multivariate_complete)
# 
# shapiro.test(residuals(mod_multivariate_complete))


#-------------------------------------------------------------------
# FEATURE SELECTION
#--------------------------------------------------------------------



#-------------------------------------------------------------------------
# proviamo a vedere se riusciamo a predirre nuove median_dwell con CV
#-------------------------------------------------------------------------
k <- 10
library(leaps)

set.seed(1)
folds <- sample(1:k,nrow(LM_2),replace=TRUE)
folds
table(folds)

# function that performs the prediction for regsubsets
predict.regsubsets <- function(object,newdata,id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

cv.errors <- matrix(NA,k,4, dimnames=list(NULL, paste(1:4)))
for(j in 1:k){
  best.fit <- regsubsets(median_dwell~.,data=LM_2[folds!=j,],nvmax=4)
  for(i in 1:4){
    pred <- predict(best.fit,LM_2[folds==j,],id=i)
    cv.errors[j,i] <- mean( (LM_2$median_dwell[folds==j]-pred)^2 )
  }
}
cv.errors
root.mean.cv.errors <- sqrt(apply(cv.errors,2,mean)) # average over the columns
root.mean.cv.errors

plot(root.mean.cv.errors,type='b')

which.min(root.mean.cv.errors)
points(which.min(root.mean.cv.errors),root.mean.cv.errors[which.min(root.mean.cv.errors)], col='red',pch=19)

# estimation on the full dataset
reg.best <- regsubsets(median_dwell~.,data=LM_2, nvmax=4)
coef(reg.best,1)





#---------------------------------------------------------------------------------------
# Facciamo PCA per vedere se eliminiamo la collinearita' -> EVITIAMO
#---------------------------------------------------------------------------------------

# model.pca <- princomp(cbind(BC.home,BC.primary,BC.stop,BC.device), scores=TRUE)
# summary(model.pca)
# model.pca$load
# 
# sp1.pc <- model.pca$scores[,1]
# sp2.pc <- model.pca$scores[,2]
# sp3.pc <- model.pca$scores[,3]
# sp4.pc <- model.pca$scores[,4]
# # scores are used as new betas
# 
# # Now we estimate the model by inserting the PCs instead of the 
# # original regressors 
# # Model: y = b0 + b1*PC1+ b2*PC2 + eps, eps~N(0,sigma^2)
# fm.pc <- lm(BC.median ~ sp1.pc + sp2.pc + sp3.pc +  sp4.pc)
# 
# summary(fm.pc) 

#--------------------------------------------------------------------------------------------
# TRASFORMIAMO I DATI GUARDANDO LA RELAZIONE TRA MEDIAN_DWELL E LE FEATURES
#--------------------------------------------------------------------------------------------

x11()
plot(raw_stop_counts, median_dwell, xlim = c(0,20000))
plot(raw_device_counts, median_dwell, xlim = c(0,10000))
plot(distance_from_home, median_dwell, xlim = c(0,20000))
plot(distance_from_primary_daytime_location, median_dwell, xlim = c(0,20000))



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
# #serve per vedere se togliendo i lev cambia di molto a lui viene 22% (che ? tanto)
# abs((mod_multivariate_complete$coefficients-mod_multivariate_complete_lev$coefficients)/mod_multivariate_complete$coefficients)
# 
# colors = rep( 'black', nrow(LM_1) )
# colors[ watchout_ids_lev ] = 'blue'
# pairs( LM_1 , pch=16,col=colors,cex=1 )
# 
# #da qua sembra che togliere i leverages non servi a nulla per? c'? qualcosa che non torna facendo il pairs
# #perch? quelli che sono evidentemente outliers non vengono tolti
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

# 
# weekday_home=integer(dim(Patterns_NY)[1])
#  
# for (i in 1:dim(Patterns_NY)[1]) {
#      for (j in 1: dim (Patterns_NY) [1] ){
#          if( is.na( weekday_device_home_areas [[j]] [area[i]] ) == FALSE  ) {
#            weekday_home[i]=weekday_home[i] + weekday_device_home_areas [[j]] [area[i]]
#          }
#      }
#  }
#    





