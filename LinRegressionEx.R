ais = read.table("C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\ais.txt", header = T)

#Analyze Scatter Plot
plot(ais$Ht~ais$Wt)

xbar = mean(ais$Wt)
sx = sd(ais$Wt)

ybar = mean(ais$Ht)
sy = sd(ais$Ht)

xbar
sx
ybar
sy

fit.ais = lm(ais$Ht~ais$Wt) #fit.ais = lm(Ht~Wt, data = ais)
summary(fit.ais)
plot(ais$Ht~ais$Wt)
abline(145, 0.3, col = 'red')
abline(130.15831, 0.54588, col = 'green')
abline(fit.ais, col = 'blue')

slope = fit.ais$coefficients[2]
slope

intercept = fit.ais$coefficients[1]
intercept

#Predict hieght of 75 kg
intercept + slope*75

# Predict hieght of list of weights from 60 to 90 incrementing by 5
x = seq(60,90, 5)
intercept + slope*x

#Calc SSE
SSE = sum(fit.ais$residuals^2)
SSE

n = length(ais$Ht)

#Calc P value
pvalue = pt(-17.68, df = n-2) + pt(17.68, df = n-2, lower.tail = F)

tcrit = qt(0.025+.95, df = n-2)
tcrit

#As Tcrit < |test stat| and pvalue < .05 Reject the null - there is sufficent evidence to suggest beta not is not zero