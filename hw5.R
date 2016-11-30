wine_training <- read.csv('wine-training-data.csv')
str(wine_training)
summary(wine_training)

#get count of NAs: 6359
sum(!complete.cases(wine_training))

#remove index - not needed
wine_training <- wine_training[,-1]

#Exploration


nrow(wine_training)

#Function to get Summary Values
col_summary <- function(col){
  std_summary=summary(col,na.rm=T)
  col_summary=data.frame(mean=std_summary[4], median=std_summary[3], sd=sd(col,na.rm=T),
                         min=std_summary[1],max=std_summary[6],FirstQ=std_summary[2],
                         ThridQ=std_summary[5],Missing=sum(is.na(col)),row.names = NULL)
  
}
#Print Summary
summary_cols <- t(sapply(wine_training,col_summary))
print(summary_cols)


#Check Correlations
#Use log of count for corrlation
round(cor(cbind(log1p(wine_training$TARGET),wine_training), use="pairwise"),2)


#Check if missing value could be used to predict
par(mfrow=c(2,4))
for (i in c(5,6,7,8,10,11,12,15)) {
  #Plot Crash Amount
  boxplot(wine_training$TARGET~is.na(wine_training[,i])
          ,main="Log Count - By Missing value",
          xlab=colnames(wine_training)[i], ylab="Count")
  
  
}

#Transforms
#use mice package to find pattern - mice assumes that the data are 'missing at random'
library(mice)
library(VIM)

#not adviseable to use imputation on categorical predictors, like STARS - subsetting the data for NAs on STARS
wine_training_sub <- wine_training[,-15]

#this shows 8675 observations that are complete, ~68% of the training dataset
md.pattern(wine_training_sub)
#plot
missing_data <- aggr(wine_training_sub, numbers=TRUE, sorVars=TRUE, cex.axis=.8, gap=3,
                     labels=names(wine_training_sub), ylab=c("Missing data","Pattern"))
#imputation, WARNING this takes about 10 minutes to run
imputed_wine <- mice(wine_training_sub, m=5, maxit = 50, method = 'pmm', seed = 500)
#completed dataset
completed_wine <- complete(imputed_wine,1)
completed_wine$STARS <- cbind(wine_training$STARS)



#Check relationship between target and variables
par(mfrow=c(2,4))
for (i in 2:9) {
  #Bin values and compute mean count
  bin_div=round((max(completed_wine[,i],na.rm = T)- 
                   min(completed_wine[,i],na.rm = T))/23,4)
  bins=round(completed_wine[,i]/bin_div,0)*bin_div
  mean_count=aggregate(wine_training$TARGET,list(bins),mean,na.rm=T)
  #Plot Mean Count Against bin
  barplot(mean_count$x,names.arg = mean_count$Group.1,
          main="Mean Count",
          xlab=colnames(completed_wine)[i], ylab="Mean Count")
  
  
}

par(mfrow=c(2,3))
for (i in 10:15) {
  #Bin values and compute mean count
  #We don't need to bins for stars and LabelAppel as they are categorical 
  if (!(i %in% c(13,15))){ 
    bin_div <- round((max(completed_wine[,i],na.rm = T) - 
                     min(completed_wine[,i],na.rm = T))/23,4)
  } else {
      bin_div <- 1
  }
  
  bins=round(completed_wine[,i]/bin_div,0)*bin_div
  mean_count=aggregate(wine_training$TARGET,list(bins),mean,na.rm=T)
  
  #Plot Mean count against min
  barplot(mean_count$x,names.arg = mean_count$Group.1,
          main="Mean Count",
          xlab=colnames(completed_wine)[i], ylab="Mean Count")
  
  
}

#variable plots
attach(completed_wine)
par(mfrow=c(4,4))
plot(FixedAcidity,TARGET, main="FixedAcidity")
plot(VolatileAcidity,TARGET, main="VolatileAcidity")
plot(CitricAcid,TARGET, main="CitricAcid")
plot(ResidualSugar,TARGET, main="ResidualSugar")
plot(Chlorides,TARGET, main="Chlorides")
plot(FreeSulfurDioxide,TARGET, main="FreeSulfurDioxide")
plot(Density,TARGET, main="Density")
plot(pH,TARGET, main="pH")
plot(Sulphates,TARGET, main="Sulphates")
plot(Alcohol,TARGET, main="Alcohol")
plot(LabelAppeal,TARGET, main="LabelAppeal")
plot(AcidIndex,TARGET, main="AcidIndex")
plot(STARS,TARGET, main="STARS")

#Code missing stars as -1 and convert to categorical value
stars_missing=is.na(wine_training$STARS)
completed_wine$STARS[stars_missing]=-1
completed_wine$STARS=as.factor(completed_wine$STARS)
completed_wine$LabelAppeal=as.factor(completed_wine$LabelAppeal)


#all variable model - doesn't look great - high variance
allvarmodel <- glm(TARGET ~ ., family=poisson, completed_wine)
summary(allvarmodel)

#Next steps, let's try some analysis to determine the "best" variables to use in a model
#Let's try variable based on exploratory analysis 
pois_model1 = glm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity + Alcohol, 
                  data=completed_wine,family=poisson)
summary(pois_model1)

pois_model2 = glm(TARGET ~ (STARS + LabelAppeal)^2 + AcidIndex + VolatileAcidity + Alcohol, 
                  data=completed_wine, family=poisson)
summary(pois_model2)

#As we have significant number of entries with STARS missing let's try 
#     separate models for data with stars and data without stars value
pois_star_model=glm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity + Alcohol, 
                    data=completed_wine[!stars_missing,], family=poisson)
summary(pois_star_model)
pois_nostar_model=glm(TARGET ~ LabelAppeal + AcidIndex + VolatileAcidity + Alcohol + pH , 
                             data=completed_wine[stars_missing,-15], family=poisson)
summary(pois_nostar_model)

#As it is simple to compare two models with one models by AIC let's check errors
sqrt(mean((pois_model2$fitted.values[starts_missing]-wine_training$TARGET[stars_missing])^2))
sqrt(mean((pois_nostar_model$fitted.values-wine_training$TARGET[stars_missing])^2))

sqrt(mean((pois_model2$fitted.values[!starts_missing]-wine_training$TARGET[!stars_missing])^2))
sqrt(mean((pois_star_model$fitted.values-wine_training$TARGET[!stars_missing])^2))

###############  Negative Binomial Models ###################

library(MASS)
allvar_nb = glm(TARGET ~ ., family=negative.binomial(1), completed_wine)
summary(allvar_nb)


nb_model1 = glm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity + Alcohol, 
                  data=completed_wine,family=negative.binomial(1))
summary(nb_model1)

nb_model2 = glm(TARGET ~ (STARS + LabelAppeal)^2 + AcidIndex + VolatileAcidity + Alcohol, 
                  data=completed_wine, family=negative.binomial(1))
summary(nb_model2)

#As we have significant number of entries with STARS missing let's try 
#     separate models for data with stars and data without stars value
nb_star_model=glm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity + Alcohol, 
                    data=completed_wine[!stars_missing,], family=negative.binomial(1))
summary(nb_star_model)
nb_nostar_model=glm(TARGET ~ LabelAppeal + AcidIndex + VolatileAcidity + Alcohol + pH , 
                      data=completed_wine[stars_missing,-15], family=negative.binomial(1))
summary(nb_nostar_model)

#As it is simple to compare two models with one models by AIC let's check errors
sqrt(mean((nb_model2$fitted.values[starts_missing]-wine_training$TARGET[stars_missing])^2))
sqrt(mean((nb_nostar_model$fitted.values-wine_training$TARGET[stars_missing])^2))

sqrt(mean((nb_model2$fitted.values[!starts_missing]-wine_training$TARGET[!stars_missing])^2))
sqrt(mean((nb_star_model$fitted.values-wine_training$TARGET[!stars_missing])^2))

#multiple linear regression models

mls <- lm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity + Alcohol, data=completed_wine)
summary(mls1)

mls1 <- lm(TARGET ~ STARS + LabelAppeal + AcidIndex + VolatileAcidity + Alcohol, data=completed_wine[!stars_missing,])
summary(mls1)

mls2 <- lm(TARGET ~ LabelAppeal + AcidIndex + VolatileAcidity + Alcohol, data=completed_wine[stars_missing,-15])
summary(mls2)



############## Make Prediction ############

wine_eval=read.csv("wine-evaluation-data.csv")
#Impute Missing Values
imputed_wine_eval <- mice(wine_eval[,c(-1,-2,-16)], m=5, maxit = 50, method = 'pmm', seed = 500)
#completed dataset
completed_wine_eval <- complete(imputed_wine_eval,1)
completed_wine_eval$STARS <- cbind(wine_eval$STARS)
stars_missing_eval=is.na(wine_eval$STARS)
completed_wine_eval$STARS[stars_missing_eval]=-1
completed_wine_eval$STARS=as.factor(completed_wine_eval$STARS)
completed_wine_eval$LabelAppeal=as.factor(completed_wine_eval$LabelAppeal)

wine_predictions=NULL
wine_predictions=data.frame(IN=wine_eval$IN,TARGET=0)
#Use selected models to make preidction
predict_stars_notmissing=predict(pois_star_model,
                                 newdata=completed_wine_eval[!stars_missing_eval,],
                                 type="response")
predict_stars_missing=predict(nb_nostar_model,
                              newdata=completed_wine_eval[stars_missing_eval,],
                              type="response")
wine_predictions$TARGET[!stars_missing_eval]=round(predict_stars_notmissing)
wine_predictions$TARGET[stars_missing_eval]=round(predict_stars_missing)
wine_predictions=as.data.frame(wine_predictions)
write.csv(wine_predictions,"wine_prediction.csv",row.names = F)
