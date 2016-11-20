wine_training <- read.csv('wine-training-data.csv')
str(wine_training)
summary(wine_training)

#get count of NAs: 6359
sum(!complete.cases(wine_training))

#remove index - not needed
wine_training <- wine_training[,-1]

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


#all variable model - doesn't look great - high variance
allvarmodel <- glm(TARGET ~ ., family=poisson, completed_wine)
summary(allvarmodel)

#Next steps, let's try some analysis to determine the "best" variables to use in a model
