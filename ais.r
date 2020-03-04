ais = read.table("C:/Users/mperry2/Downloads/ais.txt", header  = TRUE)

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
sqrt(Variance)
### Setp 6 Predict

beta1 = fit$coefficients[2]
beta0 = fit$coefficients[1]


### Predict the height in cm of a 70 kg athlete

predict = beta0 + beta1*70
predict


