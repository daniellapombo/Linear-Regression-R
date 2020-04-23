#install.packages('dplyr')
#install.packages("tidyverse")
#install.packages("tibble")
install.packages("MASS") 

library(dplyr)
library(tidyverse)
library(tibble)
library(car)
library(MASS)

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

#Clean data values particularly for PctPokiceWhite
pctPo = as.numeric(gsub('?', NA, com_dat$PctPolicWhite, fixed = TRUE))
pctPo0 = na.omit(pctPo)
print(pctPo0)
meanPctPol = mean(pctPo0)
print(meanPctPol)
com_dat$PctPolicWhite = replace_na(data = pctPo, meanPctPol)
print(com_dat$PctPolicWhite)
print(mean(com_dat$PctPolicWhite))


#Variables:
#Categorical: #'state', 'raceCatWhite','CatPopUnderPov', 
#Continous: 'pctUrban', 'perCapInc', 'whitePerCap', 'PctUnemployed',
#'PctFam2Par','PctImmigRec5','PctVacMore6Mos','MedRent','NumStreet',
#'PctPolicWhite','PopDens',
#Response: 'ViolentCrimesPerPop'

pairs(~com_dat$state + com_dat$raceCatWhite + com_dat$CatPopUnderPov + com_dat$pctUrban + 
        com_dat$perCapInc + com_dat$whitePerCap + com_dat$PctUnemployed + com_dat$PctFam2Par 
      + com_dat$PctImmigRec5 + com_dat$PctVacMore6Mos + com_dat$MedRent + com_dat$NumStreet
      + com_dat$PctPolicWhite + com_dat$PopDens) #Diagram of all variables

round(cor(data.frame(com_dat$state + com_dat$raceCatWhite + com_dat$CatPopUnderPov + com_dat$pctUrban + 
                       com_dat$perCapInc + com_dat$whitePerCap + com_dat$PctUnemployed + com_dat$PctFam2Par 
                       + com_dat$PctImmigRec5 + com_dat$PctVacMore6Mos + com_dat$MedRent + com_dat$NumStreet
                       + com_dat$PctPolicWhite + com_dat$PopDens)), 3) #Correlation Matrix
head(com_dat)


full_model = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                  perCapInc + whitePerCap + PctUnemployed + PctFam2Par 
                  + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                  + PctPolicWhite + PopDens, data = com_dat)


summary(full_model)
anova(full_model)

hist(full_model$residuals)
vif(full_model)

par(mfrow = c(1,1))
plot(full_model$residuals~predict(full_model))
rstudent = rstudent(full_model)
plot(rstudent~predict(full_model))
abline(-2,0)
abline(2,0)

#get rid of outliers in data
n = length(com_dat$ViolentCrimesPerPop)
k = 14
leverage = hat(model.matrix(full_model))
cutoff = 2*(k+1)/n
plot(leverage)
abline(cutoff,0)

cd = cooks.distance(full_model)
plot(cd)
abline(cutoff,0)

print(cd)
com_dat['cd'] = cd

head(com_dat)

print(cutoff)

outliers = filter(com_dat, cd > cutoff)
print(outliers)
print(length(outliers$cd))

keep_values = filter(com_dat, cd < cutoff)
print(keep_values)
print(length(keep_values$cd))

#Full model without outliers
clean_fullModel = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                  perCapInc + whitePerCap + PctUnemployed + PctFam2Par 
                + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                + PctPolicWhite + PopDens, data = keep_values)


summary(clean_fullModel)
anova(clean_fullModel)

hist(clean_fullModel$residuals)
vif(clean_fullModel)

par(mfrow = c(1,1))
plot(clean_fullModel$residuals~predict(clean_fullModel))
rstudent = rstudent(clean_fullModel)
plot(rstudent~predict(clean_fullModel))
abline(-2,0)
abline(2,0)

#Model Selection
fit = lm(ViolentCrimesPerPop~1,data = keep_values)  #creates just an intercept model
step <- stepAIC(fit, scope = ViolentCrimesPerPop~state + raceCatWhite + CatPopUnderPov + pctUrban + 
                  perCapInc + whitePerCap + PctUnemployed + PctFam2Par 
                + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                + PctPolicWhite + PopDens,direction="forward")

step$anova # display results



#Analysis of Corlinearity
pairs(~keep_values$state + keep_values$raceCatWhite + keep_values$CatPopUnderPov + keep_values$pctUrban + 
        keep_values$perCapInc + keep_values$whitePerCap + keep_values$PctUnemployed + keep_values$PctFam2Par 
      + keep_values$PctImmigRec5 + keep_values$PctVacMore6Mos + keep_values$MedRent + keep_values$NumStreet
      + keep_values$PctPolicWhite + keep_values$PopDens) #Diagram of all variables

round(cor(data.frame(keep_values$state + keep_values$raceCatWhite + keep_values$CatPopUnderPov + keep_values$pctUrban + 
                       keep_values$perCapInc + keep_values$whitePerCap + keep_values$PctUnemployed + keep_values$PctFam2Par 
                     + keep_values$PctImmigRec5 + keep_values$PctVacMore6Mos + keep_values$MedRent + keep_values$NumStreet
                     + keep_values$PctPolicWhite + keep_values$PopDens)), 3) #Correlation Matrix

subfit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
              pctUrban + state + PctUnemployed  + PctPolicWhite + MedRent + 
              PopDens + perCapInc + whitePerCap, data = keep_values)

summary(subfit)
vif(subfit)

subfit1 = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
               pctUrban + state + PctUnemployed  + PctPolicWhite + MedRent + 
               PopDens + whitePerCap, data = keep_values) 

summary(subfit1)
anova(subfit1)

subfit2 = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
               pctUrban + state + PctUnemployed  + PctPolicWhite + MedRent + 
               PopDens + perCapInc , data = keep_values)
summary(subfit2)
anova(subfit2)


#Interactio Terms
interModel_fit = lm(ViolentCrimesPerPop ~ PctFam2Par + raceCatWhite + NumStreet + 
                  pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                  PopDens + perCapInc +  
                  PctFam2Par*(raceCatWhite + NumStreet + pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                PopDens + perCapInc) +
                  raceCatWhite*(PctFam2Par + NumStreet +pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                    PopDens + perCapInc) +
                    NumStreet*(PctFam2Par + raceCatWhite + pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                 PopDens + perCapInc) +
                    pctUrban*(PctFam2Par + raceCatWhite + NumStreet + state + PctUnemployed + PctPolicWhite + MedRent + 
                                PopDens + perCapInc) +
                    state*(PctFam2Par + raceCatWhite + NumStreet + pctUrban + PctUnemployed + PctPolicWhite + MedRent + 
                             PopDens + perCapInc  ) +
                    PctUnemployed*(PctFam2Par + raceCatWhite + NumStreet + pctUrban + state  + PctPolicWhite + MedRent + 
                                     PopDens + perCapInc ) +
                    PctPolicWhite*(PctFam2Par + raceCatWhite + NumStreet +  pctUrban + state + PctUnemployed  + MedRent + 
                                     PopDens + perCapInc ) +
                    MedRent*(PctFam2Par + raceCatWhite + NumStreet + pctUrban + state + PctUnemployed + PctPolicWhite + 
                               PopDens + perCapInc  ) +
                    PopDens*( PctFam2Par + raceCatWhite + NumStreet +  pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                perCapInc) +
                    perCapInc*(PctFam2Par + raceCatWhite + NumStreet + 
                                 pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                 PopDens ) , data = keep_values)

summary(interModel_fit)
anova(interModel_fit)

#Find best model with Interaction Terms
inter_step <- stepAIC(fit, scope = ViolentCrimesPerPop ~ PctFam2Par + raceCatWhite + NumStreet + 
                        pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                        PopDens + perCapInc +  
                        PctFam2Par*(raceCatWhite + NumStreet + pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                      PopDens + perCapInc) +
                        raceCatWhite*(PctFam2Par + NumStreet +pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                        PopDens + perCapInc) +
                        NumStreet*(PctFam2Par + raceCatWhite + pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                     PopDens + perCapInc) +
                        pctUrban*(PctFam2Par + raceCatWhite + NumStreet + state + PctUnemployed + PctPolicWhite + MedRent + 
                                    PopDens + perCapInc) +
                        state*(PctFam2Par + raceCatWhite + NumStreet + pctUrban + PctUnemployed + PctPolicWhite + MedRent + 
                                 PopDens + perCapInc  ) +
                        PctUnemployed*(PctFam2Par + raceCatWhite + NumStreet + pctUrban + state  + PctPolicWhite + MedRent + 
                                         PopDens + perCapInc ) +
                        PctPolicWhite*(PctFam2Par + raceCatWhite + NumStreet +  pctUrban + state + PctUnemployed  + MedRent + 
                                         PopDens + perCapInc ) +
                        MedRent*(PctFam2Par + raceCatWhite + NumStreet + pctUrban + state + PctUnemployed + PctPolicWhite + 
                                   PopDens + perCapInc  ) +
                        PopDens*( PctFam2Par + raceCatWhite + NumStreet +  pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                    perCapInc) +
                        perCapInc*(PctFam2Par + raceCatWhite + NumStreet + 
                                     pctUrban + state + PctUnemployed + PctPolicWhite + MedRent + 
                                     PopDens ),direction="forward")

inter_step$anova # display results

#Model Selection
dmodel_fit = lm(ViolentCrimesPerPop ~ PctFam2Par + raceCatWhite + NumStreet + 
                  pctUrban + state + PctUnemployed + MedRent + PopDens + raceCatWhite:pctUrban + 
                  PctFam2Par:state + pctUrban:PctUnemployed + state:PopDens + 
                  PctFam2Par:raceCatWhite + NumStreet:MedRent + pctUrban:PopDens + 
                  PctUnemployed:MedRent + PctFam2Par:MedRent + PctFam2Par:pctUrban + 
                  NumStreet:PctUnemployed + PctFam2Par:PctUnemployed + MedRent:PopDens + 
                  PctFam2Par:PopDens, data = keep_values)

summary(dmodel_fit)
anova(dmodel_fit)
vif(dmodel_fit)



SSR_pctUrbanPctUnemployed = 0.275 
SSR_PctFam2ParraceCatWhite =  0.169 
SSR_pctUrbanPopDens =  0.100  
SSR_PctFam2ParPctUnemployed = 0.070  

SSR_subset=  SSR_pctUrbanPctUnemployed + SSR_PctFam2ParraceCatWhite + SSR_pctUrbanPopDens + SSR_PctFam2ParPctUnemployed 

P = 4

res_df =  1964

MSE = 0.019
SSE = 37.910
  
f = (SSR_subset/P)/MSE
f
pval = 1-pf(f, P, res_df)
pval


f1 = SSR_pctUrbanPctUnemployed/MSE
f1
pval1 = 1 - pf(f1, 1, res_df)
pval1

f2 = SSR_PctFam2ParraceCatWhite/MSE
f2
pval2 = 1 - pf(f2, 1, res_df)
pval2

f3 = SSR_pctUrbanPopDens/MSE
f3
pval3 = 1 - pf(f3, 1, res_df)
pval3


f4 = SSR_PctFam2ParPctUnemployed /MSE
f4
pval4 = 1 - pf(f4, 1, res_df)
pval4


d1model_fit = lm(ViolentCrimesPerPop ~ PctFam2Par + raceCatWhite + NumStreet + 
                  pctUrban + state + PctUnemployed + MedRent + PopDens + raceCatWhite:pctUrban + 
                  PctFam2Par:state + pctUrban:PctUnemployed + state:PopDens + 
                  PctFam2Par:raceCatWhite + NumStreet:MedRent + pctUrban:PopDens + 
                  PctUnemployed:MedRent + PctFam2Par:MedRent + PctFam2Par:pctUrban + 
                  NumStreet:PctUnemployed + MedRent:PopDens + 
                  PctFam2Par:PopDens, data = keep_values)

summary(d1model_fit)
anova(d1model_fit)

SS_pctUrbanPctUnemployed = 0.275
SS_PctFam2ParraceCatWhite =  0.169
SS_pctUrbanPopDens  =  0.100

SSE = 37.945
MSE = 0.019

df_ = 1965

f1 = SS_pctUrbanPctUnemployed/MSE
f1
pval1 = 1-pf(f1, 1, df_)
pval1

f2 = SS_PctFam2ParraceCatWhite/MSE
f2
pval2 = 1-pf(f2, 1, df_)
pval2

f3 = SS_pctUrbanPopDens /MSE
f3
pval3 = 1-pf(f3, 1, df_)
pval3






#Analysis of Corlinearity & Transformations
pairs(~PctFam2Par + raceCatWhite + NumStreet + 
        pctUrban + state + PctUnemployed + MedRent + PopDens + raceCatWhite:pctUrban + 
        PctFam2Par:state + pctUrban:PctUnemployed + state:PopDens + 
        PctFam2Par:raceCatWhite + NumStreet:MedRent + pctUrban:PopDens + 
        PctUnemployed:MedRent + PctFam2Par:MedRent + PctFam2Par:pctUrban + 
        NumStreet:PctUnemployed  + MedRent:PopDens + 
        PctFam2Par:PopDens) #Diagram of all variables

round(cor(data.frame( PctFam2Par + raceCatWhite + NumStreet + 
                        pctUrban + state + PctUnemployed + MedRent + PopDens + raceCatWhite:pctUrban + 
                        PctFam2Par:state + pctUrban:PctUnemployed + state:PopDens + 
                        PctFam2Par:raceCatWhite + NumStreet:MedRent + pctUrban:PopDens + 
                        PctUnemployed:MedRent + PctFam2Par:MedRent + PctFam2Par:pctUrban + 
                        NumStreet:PctUnemployed  + MedRent:PopDens + 
                        PctFam2Par:PopDens)), 3) #Correlation Matrix
