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
del_header
com_dat = select(import_com_dat, -del_header) #Create new data frame with chosen ('selected') columns
com_dat

#Create Categorical Variables
com_dat = add_column(mutate(com_dat, CatPopUnderPov = if_else(com_dat$PctPopUnderPov >= .5, 1, 0, missing = NULL)))#If above 50% label 1, else 0
com_dat = add_column(mutate(com_dat, raceCatWhite = if_else(com_dat$racePctWhite >= .5, 1, 0, missing = NULL))) #If above 50% label 1, else 0
com_dat

#Final Data Frame
keep_header = c('state', 'raceCatWhite','pctUrban', 'perCapInc', 'CatPopUnderPov', 'whitePerCap', 'PctUnemployed','PctFam2Par','PctImmigRec5','PctVacMore6Mos','MedRent','NumStreet','PctPolicWhite','PopDens','ViolentCrimesPerPop')
com_header = colnames(com_dat)
del_header = com_header[-pmatch(keep_header,com_header)] #del_header = keep_header - com_header
del_header
com_dat = select(com_dat, -del_header) #Create new data frame with chosen ('selected') columns
com_dat

#Compute Statistical analysis of data - Linear Regression and Hypothesis Tests

#Unemploymet vs Crime Analysis
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~com_dat$PctUnemployed)
unempl_fit = lm(com_dat$ViolentCrimesPerPop~com_dat$PctUnemployed) #Compute linear regression
summary(unempl_fit)
abline(unempl_fit, col= 'red')  #Plot fit line
#Analyize Residual & SS data
par(mfrow = c(1,3))
plot(unempl_fit$residuals~predict(unempl_fit))
hist(unempl_fit$residuals)
qqnorm(unempl_fit$residuals)
anova(unempl_fit)

#exp(Unemploymet) vs Crime Analysisplot(com_dat$ViolentCrimesPerPop~com_dat$PctUnemployed)
#The fit line is not changing ...
exp_unempl = exp(com_dat$PctUnemployed)
exp_unempl_fit = lm(com_dat$ViolentCrimesPerPop~exp_unempl) #Compute linear regression
summary(exp_unempl_fit)
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~exp_unempl)
abline(exp_unempl_fit, col= 'red') #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(exp_unempl_fit$residuals~predict(exp_unempl_fit))
hist(exp_unempl_fit$residuals)
qqnorm(exp_unempl_fit$residuals)
par(mfrow = c(1,1))
anova(exp_unempl_fit)
#CHECK ABOVE w/ Dr. Perry

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

#exp(perCapInc)
exp_perCapInc = exp(com_dat$perCapInc)
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~exp_perCapInc)
dexp_perCapInc_fit = lm(com_dat$ViolentCrimesPerPop~d_exp_perCapInc) #Compute linear regression
summary(dexp_perCapInc_fit)
abline(dexp_perCapInc_fit, col= 'red')  #Plot fit line
#Analyize Residual & SS  data
par(mfrow = c(1,3))
plot(dexp_perCapInc_fit$residuals~predict(dexp_perCapInc_fit))
hist(dexp_perCapInc_fit$residuals)
qqnorm(dexp_perCapInc_fit$residuals)
par(mfrow = c(1,1))
anova(dexp_perCapInc_fit)

#exp(ViolentCrimesPerPop)
exp_crimes = exp(com_dat$perCapInc)
par(mfrow = c(1,1))
plot(exp_crimes~com_dat$perCapInc)
dexp_crimes_fit = lm(exp_crimes~com_dat$perCapInc) #Compute linear regression
summary(dexp_crimes_fit)
abline(dexp_crimes_fit, col= 'red')  #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(dexp_crimes_fit$residuals~predict(dexp_crimes_fit))
hist(dexp_crimes_fit$residuals)
qqnorm(dexp_crimes_fit$residuals)
par(mfrow = c(1,1))
anova(dexp_crimes_fit)

#MedRentre
par(mfrow = c(1,1))
plot(com_dat$ViolentCrimesPerPop~com_dat$MedRent)
rent_fit = lm(com_dat$ViolentCrimesPerPop~com_dat$MedRent) #Compute linear regression
summary(rent_fit)
abline(rent_fit, col= 'red')  #Plot fit line
#Analyize Residual  & SS data
par(mfrow = c(1,3))
plot(rent_fit$residuals~predict(rent_fit))
hist(rent_fit$residuals)
qqnorm(rent_fit$residuals)
anova(rent_fit)

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
