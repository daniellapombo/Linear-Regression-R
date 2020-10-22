#install.packages('dplyr')
#install.packages("tidyverse")
#install.packages("tibble")
#install.packages("MASS") 

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
print(head(del_header))
com_dat = dplyr::select(import_com_dat, -all_of(del_header)) #Create new data frame with chosen ('selected') columns
print(head(com_dat))
print(head(c(com_dat$PctPopUnderPov,com_dat$racePctWhite)))

#Create Categorical Variables
com_dat = add_column(mutate(com_dat, CatPopUnderPov = if_else(com_dat$PctPopUnderPov >= .5, 1, 0, missing = NULL)))#If above 50% label 1, else 0
com_dat = add_column(mutate(com_dat, raceCatWhite = if_else(com_dat$racePctWhite >= .5, 1, 0, missing = NULL))) #If above 50% label 1, else 0

#Final Data Frame
keep_header = c('state', 'raceCatWhite','pctUrban', 'perCapInc', 'CatPopUnderPov', 'whitePerCap', 'PctUnemployed','PctFam2Par','PctImmigRec5','PctVacMore6Mos','MedRent','NumStreet','PctPolicWhite','PopDens','ViolentCrimesPerPop')
com_header = colnames(com_dat)
del_header_ = com_header[-pmatch(keep_header,com_header)] #del_header = keep_header - com_header
com_dat = dplyr::select(com_dat, -all_of(del_header_)) #Create new data frame with chosen ('selected') columns

#Clean data values particularly for PctPokiceWhite
pctPo = as.numeric(gsub('?', NA, com_dat$PctPolicWhite, fixed = TRUE))
pctPo0 = na.omit(pctPo)
print(pctPo0)
meanPctPol = mean(pctPo0)
print(meanPctPol)
com_dat$PctPolicWhite = replace_na(data = pctPo, meanPctPol)
print(com_dat$PctPolicWhite)
print(mean(com_dat$PctPolicWhite))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#The data is composed of the following variables which were cleaned (no NA's):
  #Variables:
    #Categorical: #'state', 'raceCatWhite','CatPopUnderPov', 
    #Continous: 'pctUrban', 'perCapInc', 'whitePerCap', 'PctUnemployed',
      #'PctFam2Par','PctImmigRec5','PctVacMore6Mos','MedRent','NumStreet',
      #'PctPolicWhite','PopDens',
      #Response: 'ViolentCrimesPerPop'

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Analyize the data as is (gain an understanding the of the data before model selection):
  #1. Check for collinearity 
  #2. Check for Noramality, Constant Variance and Linearity assumptions
  #3. Check for outliers & influential points

#Collinearity analysis
pairs(~ com_dat$ViolentCrimesPerPop + com_dat$state + com_dat$raceCatWhite + com_dat$CatPopUnderPov + com_dat$pctUrban + 
        com_dat$perCapInc + com_dat$whitePerCap + com_dat$PctUnemployed + com_dat$PctFam2Par 
      + com_dat$PctImmigRec5 + com_dat$PctVacMore6Mos + com_dat$MedRent + com_dat$NumStreet
      + com_dat$PctPolicWhite + com_dat$PopDens) #Diagram of all variables

round(cor(data.frame(com_dat$ViolentCrimesPerPop + com_dat$state + com_dat$raceCatWhite + com_dat$CatPopUnderPov + com_dat$pctUrban + 
                       com_dat$perCapInc + com_dat$whitePerCap + com_dat$PctUnemployed + com_dat$PctFam2Par 
                       + com_dat$PctImmigRec5 + com_dat$PctVacMore6Mos + com_dat$MedRent + com_dat$NumStreet
                       + com_dat$PctPolicWhite + com_dat$PopDens)), 3) #Correlation Matrix


#Run model to analyze normality, constant variance and linearity assumptions and to analysize outliers & influential points
full_model = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                  perCapInc + whitePerCap + PctUnemployed + PctFam2Par 
                  + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                  + PctPolicWhite + PopDens, data = com_dat)


summary(full_model)
anova(full_model)
AIC(full_model)


#Noramality Assumption Test
hist(full_model$residuals)

#Collinearity w/ VIF
vif(full_model)

#Constant variance and linearity assumption tests
par(mfrow = c(1,1))
plot(full_model$residuals~predict(full_model))
rstudent = rstudent(full_model)
plot(rstudent~predict(full_model))
abline(-2,0)
abline(2,0)

#Outliers and Influential Data analysis
n = length(com_dat$ViolentCrimesPerPop)
k = 14
cutoff = 2*(k+1)/n

leverage = hat(model.matrix(full_model))

plot(leverage)
abline(cutoff,0)

cd = cooks.distance(full_model)
plot(cd)
abline(cutoff,0)



#Create Dataframe (dataset) that does Not contain the outliers
#Do the outliers lead to a significant change in the multiple linear regressions?
com_dat['cd'] = cd
outliers = filter(com_dat, cd > cutoff) #Data set composed of the outliers
keep_values = filter(com_dat, cd < cutoff) #Data set omitting the outliers


#Analyize the influence of outliers on the multiple linear regression 
#Full model without outliers:
clean_subModel = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                  perCapInc + whitePerCap + PctUnemployed + PctFam2Par 
                + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                + PctPolicWhite + PopDens, data = keep_values)


summary(clean_subModel)
anova(clean_subModel)
AIC(clean_subModel)


#Noramlity assumption test
hist(clean_subModel$residuals)

#Collinearity VIF
vif(clean_subModel)

#Constant variance and linearity assumption tests
par(mfrow = c(1,1))
plot(clean_subModel$residuals~predict(clean_subModel))
rstudent = rstudent(clean_subModel)
plot(rstudent~predict(clean_subModel))
abline(-2,0)
abline(2,0)


#Get rid of Collinear terms
#Analyize which collinear terms are significant/useful for com_dat$perCapInc + com_dat$whitePerCap & com_dat$PctUnemployed + com_dat$NumStreet
#Procedure:
  #1. Take variable out and analyize model based off AIC and adj R^2
  #2. Do #1 for each variable
  #3. Keep variable that leads to highest adj R^2 and AIC

#1.) com_dat$perCapInc vs com_dat$whitePerCap

#w/com_dat$perCapInc  & w/out  com_dat$whitePerCap
submodel1 = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                      perCapInc + PctUnemployed + PctFam2Par 
                    + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                    + PctPolicWhite + PopDens, data = com_dat)


summary(submodel1)
anova(submodel1)
AIC(submodel1)

#w/ com_dat$whitePerCap  & w/out com_dat$perCapInc
submodel2 = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban 
                    + whitePerCap + PctUnemployed + PctFam2Par 
                    + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                    + PctPolicWhite + PopDens, data = com_dat)


summary(submodel2)
anova(submodel2)
AIC(submodel2)




#2.) com_dat$PctUnemployed + com_dat$NumStreet

#w/com_dat$PctUnemployed   & w/out  com_dat$NumStreet
submodel3 = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                 perCapInc + PctUnemployed + PctFam2Par 
               + PctImmigRec5 + PctVacMore6Mos + MedRent
               + PctPolicWhite + PopDens, data = com_dat)


summary(submodel3)
anova(submodel3)
AIC(submodel3)

#w/ com_dat$NumStreet  & w/out com_dat$PctUnemployed 
submodel4 = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban 
               + whitePerCap +PctFam2Par 
               + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
               + PctPolicWhite + PopDens, data = com_dat)


summary(submodel4)
anova(submodel4)
AIC(submodel4)

#Full Model w/OUT collinear terms:
Gfull_model = lm(ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                  perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                + PctPolicWhite + PopDens, data = com_dat)

summary(Gfull_model)
anova(Gfull_model)
AIC(Gfull_model)

#Analyze collinearity
vif(Gfull_model)


#Model Selection: will analysize the selection based on singular variables (no interaction nor transformation terms)
  #Goal: answer which variables are most important in an elementary analysis
  #Steps:
    #1. w/ Forward Selection, find multiple reg equation
    #2. Analysize correlations between variables
    #3. Run the selected model (from #1) and analysize adjusted R^2 and AIC

fit = lm(ViolentCrimesPerPop~1,data = com_dat)  #creates just an intercept model

step <- stepAIC(fit, scope =  ViolentCrimesPerPop~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                  perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                + PctPolicWhite + PopDens,direction="forward")



step$anova # display results

com_dat$state

subfit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
              state + pctUrban + PctPolicWhite + CatPopUnderPov + MedRent + 
              perCapInc, data = com_dat)

summary(subfit)
anova(subfit)
AIC(subfit)

vif(subfit)




#Interactio Terms
max_model= lm( ViolentCrimesPerPop ~  state + raceCatWhite + CatPopUnderPov + pctUrban + 
                 perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
               + PctPolicWhite + PopDens
               +state* ( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                           perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                         + PctPolicWhite + PopDens)
               + raceCatWhite*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                  perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                + PctPolicWhite + PopDens)
               + CatPopUnderPov*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                    perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                  + PctPolicWhite + PopDens)
               +pctUrban*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                             perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                           + PctPolicWhite + PopDens)
               + perCapInc*(state + raceCatWhite + CatPopUnderPov + pctUrban + 
                               whitePerCap + PctUnemployed + PctFam2Par 
                            + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                            + PctPolicWhite + PopDens)
               + PctFam2Par*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                              + PctPolicWhite + PopDens)
               + PctImmigRec5*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                  perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                + PctPolicWhite + PopDens)
               + PctVacMore6Mos*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                    perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                  + PctPolicWhite + PopDens)
               + MedRent*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                             perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                           + PctPolicWhite + PopDens)
               + NumStreet*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                               perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                             + PctPolicWhite + PopDens)
               + PctPolicWhite*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                   perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                 + PctPolicWhite + PopDens)
               + PopDens*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                             perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                           + PctPolicWhite + PopDens)
                , data = com_dat)

summary(max_model)
anova(max_model)
AIC(max_model)

#Find best model with Interaction Terms
model_select <- stepAIC(fit, scope = ViolentCrimesPerPop ~ state + raceCatWhite + CatPopUnderPov + pctUrban + 
                          perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                        + PctPolicWhite + PopDens
                        +state* ( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                    perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                  + PctPolicWhite + PopDens)
                        + raceCatWhite*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                           perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                         + PctPolicWhite + PopDens)
                        + CatPopUnderPov*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                             perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                           + PctPolicWhite + PopDens)
                        +pctUrban*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                      perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                    + PctPolicWhite + PopDens)
                        + perCapInc*(state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                       whitePerCap +  PctFam2Par 
                                     + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                     + PctPolicWhite + PopDens)
                        + PctFam2Par*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                         perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                       + PctPolicWhite + PopDens)
                        + PctImmigRec5*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                           perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                         + PctPolicWhite + PopDens)
                        + PctVacMore6Mos*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                             perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                           + PctPolicWhite + PopDens)
                        + MedRent*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                      perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                    + PctPolicWhite + PopDens)
                        + NumStreet*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                        perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                      + PctPolicWhite + PopDens)
                        + PctPolicWhite*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                            perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                          + PctPolicWhite + PopDens)
                        + PopDens*( state + raceCatWhite + CatPopUnderPov + pctUrban + 
                                      perCapInc + PctFam2Par + PctImmigRec5 + PctVacMore6Mos + MedRent + NumStreet
                                    + PctPolicWhite + PopDens),direction="forward")

model_select$anova # display results

#Model Selection
dmodel_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                  state + pctUrban + MedRent + CatPopUnderPov + 
                  PopDens + raceCatWhite:pctUrban + PctFam2Par:state + PctFam2Par:pctUrban + 
                  PctFam2Par:raceCatWhite + state:pctUrban + NumStreet:MedRent + 
                  state:CatPopUnderPov + PctFam2Par:NumStreet + MedRent:CatPopUnderPov + 
                  PctFam2Par:MedRent + raceCatWhite:MedRent + state:PopDens + 
                  pctUrban:PopDens + PctFam2Par:PopDens + CatPopUnderPov:PopDens + 
                  MedRent:PopDens, data = com_dat)

summary(dmodel_fit)
anova(dmodel_fit)
AIC(dmodel_fit)
vif(dmodel_fit)


d0model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite + 
                   state:pctUrban + PctFam2Par:NumStreet + raceCatWhite:MedRent + pctUrban:PopDens
                  + CatPopUnderPov:PopDens +MedRent:PopDens , data = com_dat)


anova(d0model_fit)


SSR = 0.123  + 0.077  + 0 + 0.092 + 0.021 + 0.133  +  0.104 +  0.051  

SSE = 39.33
MSE = 0.020

f = (SSR/8)/MSE
f
pval = 1 - pf(f, 8, 1968)
pval



#Narrowing down model
#Procedure:
#1. Perform partial F test
#2. Keep or get rid of variable
#3. Continue to perform, until find best model


d0model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite + 
                   state:pctUrban + PctFam2Par:NumStreet + raceCatWhite:MedRent + pctUrban:PopDens
                 + CatPopUnderPov:PopDens +MedRent:PopDens , data = com_dat)


anova(d0model_fit)


SSR = 0.123  + 0.077  + 0 + 0.092 + 0.021 + 0.133  +  0.104 +  0.051  

SSE = 39.33
MSE = 0.020

f = (SSR/8)/MSE
f
pval = 1 - pf(f, 8, 1968)
pval



d1model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite + 
                   state:pctUrban + PctFam2Par:NumStreet + raceCatWhite:MedRent + pctUrban:PopDens
                 + CatPopUnderPov:PopDens , data = com_dat)

anova(d1model_fit)

d2model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite + 
                   state:pctUrban + PctFam2Par:NumStreet + raceCatWhite:MedRent + pctUrban:PopDens
                  , data = com_dat)

anova(d2model_fit)

d2model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite + 
                   state:pctUrban + PctFam2Par:NumStreet + raceCatWhite:MedRent
                 , data = com_dat)

anova(d2model_fit)

d3model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite + 
                   state:pctUrban + PctFam2Par:NumStreet
                 , data = com_dat)

anova(d3model_fit)


d4model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite + 
                   state:pctUrban 
                 , data = com_dat)

anova(d4model_fit)

d5model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban + PctFam2Par:raceCatWhite 
                   
                 , data = com_dat)

anova(d5model_fit)

d6model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens +PctFam2Par:pctUrban , data = com_dat)

anova(d6model_fit)


d6model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                   PctFam2Par:MedRent +  state:PopDens + 
                   PctFam2Par:PopDens , data = com_dat)

anova(d6model_fit)

d7model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban  + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + 
                   state:CatPopUnderPov + MedRent:CatPopUnderPov + 
                    state:PopDens + PctFam2Par:PopDens, data = com_dat)

anova(d7model_fit)

d7model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + PctFam2Par:pctUrban  +
                   state:CatPopUnderPov +  
                   state:PopDens + PctFam2Par:PopDens + MedRent:CatPopUnderPov , data = com_dat)

anova(d7model_fit)


d8model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                   NumStreet:MedRent + PctFam2Par:pctUrban  +
                   state:CatPopUnderPov +  
                   state:PopDens + PctFam2Par:PopDens, data = com_dat)

anova(d8model_fit)

d9model_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                   state + pctUrban + MedRent + CatPopUnderPov + 
                   PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                    PctFam2Par:pctUrban  +
                   state:CatPopUnderPov +  
                   state:PopDens + PctFam2Par:PopDens + NumStreet:MedRent , data = com_dat)

anova(d9model_fit)



#Final Model:
final_fit = lm(ViolentCrimesPerPop ~PctFam2Par + raceCatWhite + NumStreet + 
                 state + pctUrban + MedRent + CatPopUnderPov + 
                 PopDens + raceCatWhite:pctUrban + PctFam2Par:state + 
                 PctFam2Par:pctUrban  +
                 state:CatPopUnderPov +  
                 state:PopDens + PctFam2Par:PopDens + NumStreet:MedRent, data = com_dat)


summary(final_fit)
anova(final_fit)
AIC(final_fit)

#Noramlity assumption test
hist(final_fit$residuals)

#Collinearity VIF
vif(final_fit)

#Constant variance and linearity assumption tests
par(mfrow = c(1,1))
plot(final_fit$residuals~predict(final_fit))
rstudent = rstudent(final_fit)
plot(rstudent~predict(final_fit))
abline(-2,0)
abline(2,0)


