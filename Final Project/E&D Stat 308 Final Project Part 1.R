#install.packages('dplyr')
#install.packages("tidyverse")
#install.packages("tibble")

library(dplyr)
library(tidyverse)
library(tibble)

#Crime Data set used: https://archive.ics.uci.edu/ml/datasets/Communities+and+Crime
#Crime Data set has 128 columns, and 1994 Instances
#Note raw Crime Data file does NOT contain headers for features

#With this Crime Data set we intende to analysize correlative features for community crimes
#Through mutliple and uni linear regression, we hope to find the predicitve and corrlative features of community crime

#Questions we seek to answer:
#1. How do unemployment increase the rate of crime?
#2. Do financial factors affect the prevalence of crime?
#3. How does race play into crime rates?

#---------------------------------------------------------------------------------------

#Define Column Names
com_header = c('state',
               'county',
               'community',
               'communityname',
               'fold',
               'population',
               'householdsize',
               'racepctblack',
               'racePctWhite',
               'racePctAsian',
               'racePctHisp',
               'agePct12t21',
               'agePct12t29',
               'agePct16t24',
               'agePct65up',
               'numbUrban',
               'pctUrban',
               'medIncome',
               'pctWWage',
               'pctWFarmSelf',
               'pctWInvInc',
               'pctWSocSec',
               'pctWPubAsst',
               'pctWRetire',
               'medFamInc',
               'perCapInc',
               'whitePerCap',
               'blackPerCap',
               'indianPerCap',
               'AsianPerCap',
               'OtherPerCap',
               'HispPerCap',
               'NumUnderPov',
               'PctPopUnderPov',
               'PctLess9thGrade',
               'PctNotHSGrad',
               'PctBSorMore',
               'PctUnemployed',
               'PctEmploy',
               'PctEmplManu',
               'PctEmplProfServ',
               'PctOccupManu',
               'PctOccupMgmtProf',
               'MalePctDivorce',
               'MalePctNevMarr',
               'FemalePctDiv',
               'TotalPctDiv',
               'PersPerFam',
               'PctFam2Par',
               'PctKids2Par',
               'PctYoungKids2Par',
               'PctTeen2Par',
               'PctWorkMomYoungKids',
               'PctWorkMom',
               'NumIlleg',
               'PctIlleg',
               'NumImmig',
               'PctImmigRecent',
               'PctImmigRec5',
               'PctImmigRec8',
               'PctImmigRec10',
               'PctRecentImmig',
               'PctRecImmig5',
               'PctRecImmig8',
               'PctRecImmig10',
               'PctSpeakEnglOnly',
               'PctNotSpeakEnglWell',
               'PctLargHouseFam',
               'PctLargHouseOccup',
               'PersPerOccupHous',
               'PersPerOwnOccHous',
               'PersPerRentOccHous',
               'PctPersOwnOccup',
               'PctPersDenseHous',
               'PctHousLess3BR',
               'MedNumBR',
               'HousVacant',
               'PctHousOccup',
               'PctHousOwnOcc',
               'PctVacantBoarded',
               'PctVacMore6Mos',
               'MedYrHousBuilt',
               'PctHousNoPhone',
               'PctWOFullPlumb',
               'OwnOccLowQuart',
               'OwnOccMedVal',
               'OwnOccHiQuart',
               'RentLowQ',
               'RentMedian',
               'RentHighQ',
               'MedRent',
               'MedRentPctHousInc',
               'MedOwnCostPctInc',
               'MedOwnCostPctIncNoMtg',
               'NumInShelters',
               'NumStreet',
               'PctForeignBorn',
               'PctBornSameState',
               'PctSameHouse85',
               'PctSameCity85',
               'PctSameState85',
               'LemasSwornFT',
               'LemasSwFTPerPop',
               'LemasSwFTFieldOps',
               'LemasSwFTFieldPerPop',
               'LemasTotalReq',
               'LemasTotReqPerPop',
               'PolicReqPerOffic',
               'PolicPerPop',
               'RacialMatchCommPol',
               'PctPolicWhite',
               'PctPolicBlack',
               'PctPolicHisp',
               'PctPolicAsian',
               'PctPolicMinor',
               'OfficAssgnDrugUnits',
               'NumKindsDrugsSeiz',
               'PolicAveOTWorked',
               'LandArea',
               'PopDens',
               'PctUsePubTrans',
               'PolicCars',
               'PolicOperBudg',
               'LemasPctPolicOnPatr',
               'LemasGangUnitDeploy',
               'LemasPctOfficDrugUn',
               'PolicBudgPerPop',
               'ViolentCrimesPerPop')

# Import the Crime Data set

import_com_dat = read.table("C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Final Project\\communities.data", sep = ",", header = FALSE, col.names = com_header)

#Composed data set with chosen/necessary variables------------------------------------------------------------------------------------
#Our data frame will be composed of the following given variables:
###'state', 'raceCatWhite','pctUrban', 'perCapInc', 'CatPopUnderPov', 'whitePerCap', 
###''PctUnemployed','PctFam2Par','PctImmigRec5','PctVacMore6Mos','MedRent',
###''NumStreet','PctPolicWhite','PopDens','ViolentCrimesPerPop'


#Create data frame composed of chosen columns (refer to keep_header)
keep_header = c('state', 'racePctWhite','pctUrban', 'perCapInc', 'PctPopUnderPov', 'whitePerCap', 'PctUnemployed','PctFam2Par','PctImmigRec5','PctVacMore6Mos','MedRent','NumStreet','PctPolicWhite','PopDens','ViolentCrimesPerPop')
del_header = com_header[-pmatch(keep_header,com_header)] #del_header = keep_header - com_header
com_dat = select(import_com_dat, -del_header) #Create new data frame with chosen ('selected') columns


#Create Categorical Variables
com_dat = add_column(mutate(com_dat, CatPopUnderPov = if_else(com_dat$PctPopUnderPov >= .5, 1, 0, missing = NULL)))#If above 50% label 1, else 0
com_dat = add_column(mutate(com_dat, raceCatWhite = if_else(com_dat$racePctWhite >= .5, 1, 0, missing = NULL))) #If above 50% label 1, else 0

#Final Data Frame
keep_header = c('state', 'raceCatWhite','pctUrban', 'perCapInc', 'CatPopUnderPov', 'whitePerCap', 'PctUnemployed','PctFam2Par','PctImmigRec5','PctVacMore6Mos','MedRent','NumStreet','PctPolicWhite','PopDens','ViolentCrimesPerPop')
com_header = colnames(com_dat)
del_header = com_header[-pmatch(keep_header,com_header)] #del_header = keep_header - com_header
com_dat = select(com_dat, -del_header) #Create new data frame with chosen ('selected') columns

#Ordered Data Frame
com_dat1 = com_dat[order(com_dat$PctUnemployed),]
com_dat2 = com_dat[order(com_dat$perCapInc),]
com_dat3 = com_dat[order(com_dat$MedRent),]
com_dat4 = com_dat[order(com_dat$whitePerCap),]
com_dat5 = com_dat[order(com_dat$PctPolicWhite),]

#Only have to plot and anlysize linear regression for one

#Compute Statistical analysis of data - Linear Regression and Hypothesis Tests

confintline = function(X,Sx,Yhat,n, Xbar,RMSE,CL){
  alpha = 1 - CL
  CILB=Yhat - qt(1-alpha/2,n-2)*RMSE*sqrt(1/n+(X-Xbar)^2/((n-1)*Sx^2))
  CIUB = Yhat + qt(1-alpha/2,n-2)*RMSE*sqrt(1/n+(X-Xbar)^2/((n-1)*Sx^2))
  return(cbind(CILB,CIUB))
}

predintline = function(X,Sx,Yhat,n, Xbar,RMSE,CL){
  alpha = 1 - CL
  PILB=Yhat - qt(1-alpha/2,n-2)*RMSE*sqrt(1+1/n+(X-Xbar)^2/((n-1)*Sx^2))
  PIUB = Yhat + qt(1-alpha/2,n-2)*RMSE*sqrt(1+1/n+(X-Xbar)^2/((n-1)*Sx^2))
  return(cbind(PILB,PIUB))
}

#Unemploymet vs Crime Analysis
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~com_dat$PctUnemployed)
unempl_fit = lm(com_dat1$ViolentCrimesPerPop~com_dat1$PctUnemployed) #Compute linear regression
summary(unempl_fit)
abline(unempl_fit, col= 'red')  #Plot fit line
#Analyize Residual & SS data
par(mfrow = c(1,3))
plot(unempl_fit$residuals~predict(unempl_fit))
hist(unempl_fit$residuals)
qqnorm(unempl_fit$residuals)
anova(unempl_fit)

#Confidence Interval for Unemploymet vs Crime Analysis
par(mfrow = c(1,1))
meanUnempl = mean(com_dat1$PctUnemployed)
sdUnempl = sd(com_dat1$PctUnemployed)
n = length(com_dat1$PctUnemployed)
RMSE = sqrt((1/(n-2)*sum(unempl_fit$residuals^2)))
X = seq(min(com_dat1$PctUnemployed), max(com_dat1$PctUnemployed), length.out = 20)
Ypredict = unempl_fit$coefficients[1] + unempl_fit$coefficients[2]*X
ci = confintline(X, sdUnempl, Ypredict, n, meanUnempl, RMSE, 0.95)
plot(com_dat$ViolentCrimesPerPop~com_dat1$PctUnemployed)
abline(unempl_fit)

lines(X, ci[,"CILB"],lty = "dashed", col = "red")
lines(X, ci[,"CIUB"],lty = "dashed", col = "red")


#exp model: Unemploymet vs Crime Analysis 
lnCrime = log(com_dat1$ViolentCrimesPerPop + 2)
exp_unempl_fit = lm(lnCrime~com_dat1$PctUnemployed) #Compute linear regression
summary(exp_unempl_fit)
par(mfrow = c(1,1))
plot(lnCrime~com_dat1$PctUnemployed)
abline(exp_unempl_fit, col= 'red') #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(exp_unempl_fit$residuals~predict(exp_unempl_fit))
hist(exp_unempl_fit$residuals)
qqnorm(exp_unempl_fit$residuals)
par(mfrow = c(1,1))
anova(exp_unempl_fit)

#Plot the fit line on the original data
expCrimes = exp(predict(exp_unempl_fit))-2
plot(com_dat1$ViolentCrimesPerPop~com_dat1$PctUnemployed)
lines(expCrimes~com_dat1$PctUnemployed, col = 'red')

#Confidence Interval for exp model: Unemploymet vs Crime Analysis 
par(mfrow = c(1,1))
meanUnempl = mean(com_dat1$PctUnemployed)
sdUnempl = sd(com_dat1$PctUnemployed)
n = length(com_dat1$PctUnemployed)
RMSE = sqrt((1/(n-2)*sum(exp_unempl_fit$residuals^2)))
X = seq(min(com_dat1$PctUnemployed), max(com_dat1$PctUnemployed), length.out = 20)
Ypredict = exp_unempl_fit$coefficients[1] + exp_unempl_fit$coefficients[2]*X
ci = exp(confintline(X, sdUnempl, Ypredict, n, meanUnempl, RMSE, 0.95))-2

expCrimes = exp(predict(exp_unempl_fit))-2
plot(com_dat1$ViolentCrimesPerPop~com_dat1$PctUnemployed)
lines(expCrimes~com_dat1$PctUnemployed, col = 'red')
print(ci)

lines(X, ci[,"CILB"],lty = "dashed", col = "blue")
lines(X, ci[,"CIUB"],lty = "dashed", col = "blue")

#arcsin sqr root model: Unemploymet vs Crime Analysis 
asCrime = asin(sqrt(com_dat1$ViolentCrimesPerPop ))
print(lnCrime)
as_unempl_fit = lm(asCrime~com_dat1$PctUnemployed) #Compute linear regression
summary(as_unempl_fit)
par(mfrow = c(1,1))
plot(asCrime~com_dat1$PctUnemployed)
abline(as_unempl_fit, col= 'red') #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(as_unempl_fit$residuals~predict(as_unempl_fit))
hist(as_unempl_fit$residuals)
qqnorm(as_unempl_fit$residuals)
par(mfrow = c(1,1))
anova(as_unempl_fit)

#Plot the fit line on the original data
sinCrimes = sin(predict(as_unempl_fit))^2
plot(com_dat1$ViolentCrimesPerPop~com_dat1$PctUnemployed)
lines(sinCrimes~com_dat1$PctUnemployed, col = 'red')

#Confidence Interval for arcsin sqr root model: Unemploymet vs Crime Analysis 
par(mfrow = c(1,1))
meanUnempl = mean(com_dat1$PctUnemployed)
sdUnempl = sd(com_dat1$PctUnemployed)
n = length(com_dat1$PctUnemployed)
RMSE = sqrt((1/(n-2)*sum(as_unempl_fit$residuals^2)))
X = seq(min(com_dat1$PctUnemployed), max(com_dat1$PctUnemployed), length.out = 20)
Ypredict = as_unempl_fit$coefficients[1] + as_unempl_fit$coefficients[2]*X
ci = sin(confintline(X, sdUnempl, Ypredict, n, meanUnempl, RMSE, 0.95))^2

sinCrimes = sin(predict(as_unempl_fit))^2
plot(com_dat1$ViolentCrimesPerPop~com_dat1$PctUnemployed)
lines(sinCrimes~com_dat1$PctUnemployed, col = 'red')

lines(X, ci[,"CILB"],lty = "dashed", col = "blue")
lines(X, ci[,"CIUB"],lty = "dashed", col = "blue")

#Fincial vs Crime Analysis
#perCapInc
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~com_dat$perCapInc)
perCapInc_fit = lm(com_dat$ViolentCrimesPerPop~com_dat$perCapInc) #Compute linear regression
summary(perCapInc_fit)
abline(perCapInc_fit, col= 'red')  #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(perCapInc_fit$residuals~predict(perCapInc_fit))
hist(perCapInc_fit$residuals)
qqnorm(perCapInc_fit$residuals)
par(mfrow = c(1,1))
anova(perCapInc_fit)


#arcsin(ViolentCrimesPerPop) vs perCapINc
asCrime = asin(sqrt(com_dat2$ViolentCrimesPerPop))
par(mfrow = c(1,1))
plot(asCrime~com_dat2$perCapInc)
as_perCap_fit = lm(asCrime~com_dat2$perCapInc) #Compute linear regression
summary(as_perCap_fit)
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(as_perCap_fit$residuals~predict(as_perCap_fit))
hist(as_perCap_fit$residuals)
qqnorm(as_perCap_fit$residuals)
par(mfrow = c(1,1))
anova(as_perCap_fit)


#Plot the fit line on the original data
sinCrimes = sin(predict(as_perCap_fit))^2
plot(com_dat2$ViolentCrimesPerPop~com_dat2$perCapInc)
lines(sinCrimes~com_dat2$perCapInc, col = 'red')

#MedRent
par(mfrow = c(1,1))
plot(com_dat3$ViolentCrimesPerPop~com_dat3$MedRent)
rent_fit = lm(com_dat3$ViolentCrimesPerPop~com_dat3$MedRent) #Compute linear regression
summary(rent_fit)
abline(rent_fit, col= 'red')  #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(rent_fit$residuals~predict(rent_fit))
hist(rent_fit$residuals)
qqnorm(rent_fit$residuals)
anova(rent_fit)

#arcsin(ViolentCrimesPerPop) vs MedRent
asCrime = asin(sqrt(com_dat3$ViolentCrimesPerPop))
par(mfrow = c(1,1))
plot(exp_crimes~com_dat3$MedRent)
as_rent_fit = lm(asCrime~com_dat3$MedRent) #Compute linear regression
summary(as_rent_fit)
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(as_rent_fit$residuals~predict(as_rent_fit))
hist(as_rent_fit$residuals)
qqnorm(as_rent_fit$residuals)
par(mfrow = c(1,1))
anova(as_rent_fit)


#Plot the fit line on the original data
sinCrimes = sin(predict(as_rent_fit))^2
plot(com_dat3$ViolentCrimesPerPop~com_dat3$MedRent)
lines(sinCrimes~com_dat3$MedRent, col = 'red')

#Race factors vs Crime: White population statistics
#whitePerCap
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~com_dat$whitePerCap)
W_perCap_fit = lm(com_dat$ViolentCrimesPerPop~com_dat$whitePerCap) #Compute linear regression
summary(W_perCap_fit)
abline(W_perCap_fit, col= 'red')  #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(W_perCap_fit$residuals~predict(W_perCap_fit))
hist(W_perCap_fit$residuals)
qqnorm(W_perCap_fit$residuals)
anova(W_perCap_fit)

#arcsin(ViolentCrimesPerPop) vs WhitePerCap
asCrime = asin(sqrt(com_dat4$ViolentCrimesPerPop))
par(mfrow = c(1,1))
plot(exp_crimes~com_dat4$whitePerCap)
asW_perCap_fit = lm(asCrime~com_dat4$whitePerCap) #Compute linear regression
summary(asW_perCap_fit)
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(asW_perCap_fit$residuals~predict(asW_perCap_fit))
hist(asW_perCap_fit$residuals)
qqnorm(asW_perCap_fit$residuals)
par(mfrow = c(1,1))
anova(asW_perCap_fit)


#Plot the fit line on the original data
sinCrimes = sin(predict(asW_perCap_fit))^2
plot(com_dat4$ViolentCrimesPerPop~com_dat4$whitePerCap)
lines(sinCrimes~com_dat4$whitePerCap, col = 'red')


#PctPolicWhite
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~com_dat$PctPolicWhite)
W_pol_fit = lm(com_dat$ViolentCrimesPerPop~com_dat$PctPolicWhite) #Compute linear regression
summary(W_pol_fit)
abline(W_pol_fit, col= 'red')  #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(W_pol_fit$residuals~predict(W_pol_fit))
hist(W_pol_fit$residuals)
qqnorm(W_pol_fit$residuals)
anova(W_pol_fit)

#arcsin(ViolentCrimesPerPop) vs PctPolicWhite
asCrime = asin(sqrt(com_dat5$ViolentCrimesPerPop))
par(mfrow = c(1,1))
plot(exp_crimes~com_dat5$PctPolicWhite)
asW_pol_fit = lm(asCrime~com_dat5$PctPolicWhite) #Compute linear regression
summary(asW_pol_fit)
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(asW_pol_fit$residuals~predict(asW_pol_fit))
hist(asW_pol_fit$residuals)
qqnorm(asW_pol_fit$residuals)
par(mfrow = c(1,1))
anova(asW_pol_fit)


#Plot the fit line on the original data
sinCrimes = sin(predict(asW_pol_fit))^2
plot(com_dat5$ViolentCrimesPerPop~com_dat5$PctPolicWhite)
lines(sinCrimes~com_dat5$PctPolicWhite, col = 'red')

