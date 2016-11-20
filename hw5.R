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


#all variable model - doesn't look great - high variance
allvarmodel <- glm(TARGET ~ ., family=poisson, completed_wine)
summary(allvarmodel)

#Next steps, let's try some analysis to determine the "best" variables to use in a model
