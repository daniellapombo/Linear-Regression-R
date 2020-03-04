ais = read.table(file.choose(),header=TRUE)

#SSE

SSE = sum(fit$residuals)

### Step 1, Analyze the Scatter Plot
plot(ais$Ht~ais$Wt)

### Step 2 Create the linear model
fit = lm(Ht~Wt,data=ais)
summary(fit)

### Step 4 Analyze suitability of model -  There is a lot to this!!!! More to come
anova(fit)

### sums of residuals - should be close to 0
sumepsilon = sum(fit$residuals)

### SSE
SSE = sum(fit$residuals^2)

Variance = 1/(length(ais$Ht) - 2)*SSE
RMSE = sqrt(Variance)

### Setp 6 Predict

beta1 = fit$coefficients[2]
beta0 = fit$coefficients[1]

meanWT = mean(ais$Wt)
SDWT = sd(ais$Wt)
n = length(ais$Wt)


### Predict the height in cm of a 70 kg athlete
### If you want a specific value enter X= that value
### Sequence makes it graoh the whole confidence interval or prediction interval using our functions

X = seq(min(ais$Wt), max(ais$Ht), length.out = 30)
predict = beta0 + beta1*X
predict


### Create a confidence band

confintline = function(X,Sx,Yhat,n,Xbar,RMSE,CL) {
  alpha = 1-CL
  CILB= Yhat - qt(1 - alpha/2, n - 2)*RMSE*sqrt(1/n + (X - Xbar)^2/((n-1)*Sx^2))
  CIUB= Yhat + qt(1 - alpha/2, n - 2)*RMSE*sqrt(1/n + (X - Xbar)^2/((n-1)*Sx^2))
  return(cbind(CILB,CIUB))
}

CL = 0.95

CI = confintline(X,SDWT, predict, n, meanWT, RMSE, CL)
CI

plot(ais$Ht~ais$Wt)
abline(fit, col = "green")
lines(X,CI[,"CILB"], lty='dashed', col = 'red')
lines(X,CI[,"CIUB"], lty='dashed', col = 'red')

### Create a prediction band

predintline = confintline = function(X,Sx,Yhat,n,Xbar,RMSE,CL) {
  alpha = 1-CL
  PILB= Yhat - qt(1 - alpha/2, n - 2)*RMSE*sqrt(1 + 1/n + (X - Xbar)^2/((n-1)*Sx^2))
  PIUB= Yhat + qt(1 - alpha/2, n - 2)*RMSE*sqrt(1 + 1/n + (X - Xbar)^2/((n-1)*Sx^2))
  return(cbind(PILB,PIUB))
}

PI = predintline(X,SDWT, predict, n, meanWT, RMSE, CL)
PI

lines(X,PI[,"PILB"], lty='dashed', col = 'blue')
lines(X,PI[,"PIUB"], lty='dashed', col = 'blue')

