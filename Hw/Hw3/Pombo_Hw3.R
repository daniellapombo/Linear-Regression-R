#1
one_file = read.csv('C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\Hw3\\ch05q02.txt', header = T)
colnames(one_file)
#print(one_file)

#a 
fit_one = lm(one_file$"X.SBP."~one_file$"X.QUET.")
summary(fit_one)
#b
slope = fit_one$coefficients[2]
print(slope)

SSE = sum(fit_one$residuals^2)

n = length(one_file$"X.SBP.")
print(n)
MSE = (1/(n - 2))*SSE
RMSE = sqrt(MSE)
RMSE
x_sd = sd(one_file$"X.QUET.")
SSX = (n-1)*x_sd^2
slope_sd = RMSE/sqrt(SSX)

test_stat = (slope - 0)/ slope_sd

print(test_stat)

t = qt((1-.05/2), n-2)

print(t)

p_val = 2*pt(test_stat, n-2, lower.tail = F)

print(p_val)


#c Plot Regression Line and Prediction  Interval

#Create Prediction Interval
y_intrcpt = fit_one$coefficients[1]

mean_ = mean(one_file$"X.QUET.")
sd_ = sd(one_file$"X.QUET.")


# Sequence graphs the entire confidence/predictoin interval using functions

X = seq(min(one_file$"X.QUET."), max(one_file$"X.QUET."), length.out = 30)
pred_intval = y_intrcpt + slope*X
pred_intval


#Create a prediction band

predintline = function(X,Sx,Yhat,n,Xbar,RMSE,CL) {
  alpha = 1-CL
  PILB= Yhat - qt(1 - alpha/2, n - 2)*RMSE*sqrt(1 + (1/n) + (X - Xbar)^2/((n-1)*Sx^2))
  PIUB= Yhat + qt(1 - alpha/2, n - 2)*RMSE*sqrt(1 + (1/n) + (X - Xbar)^2/((n-1)*Sx^2))
  return(cbind(PILB,PIUB))
}


plot(one_file$"X.SBP."~one_file$"X.QUET.")
abline(fit_one, col = "green")

PI = predintline(X,sd_, pred_intval, n, mean_, RMSE, .95)
PI

lines(X,PI[,"PILB"], lty='dashed', col = 'blue')
lines(X,PI[,"PIUB"], lty='dashed', col = 'blue')


#d
pred_ = y_intrcpt + slope*3.4
print(pred_)
alpha = .05
PILB= pred_ - qt(1 - alpha/2, n - 2)*RMSE*sqrt(1 + 1/n + (3.4 - mean_)^2/((n-1)*sd_^2))
PIUB= pred_ + qt(1 - alpha/2, n - 2)*RMSE*sqrt(1 + 1/n + (3.4 - mean_)^2/((n-1)*sd_^2))
print(c(PILB, PIUB))

#D Residual Plots
#par(mfrow = c(3,1))
plot(fit_one$residuals~predict(fit_one))
hist(fit_one$residuals)
qqnorm(fit_one$residuals)

#2	DISTANCE = y and MPH = x 
two_file = read.csv('C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\Hw3\\ch05q07.txt', header = T)
colnames(two_file)
plot(two_file$"X.DIST."~two_file$"X.MPH.")

#b
two_fit = lm(two_file$"X.DIST."~two_file$"X.MPH.")
abline(two_fit, col = "red")

y_int = two_fit$coefficients[1]
slope = two_fit$coefficients[2]

print(c(y_int, slope))

#c
plot(two_fit$residuals~predict(two_fit))

#3 	DISTANCE = y and MPH = x 
three_fit = lm(two_file$"X.SQRTDIST."~two_file$"X.MPH.")
plot(two_file$"X.SQRTDIST."~two_file$"X.MPH.")
abline(three_fit, col = "red")

y_int = three_fit$coefficients[1]
slope = three_fit$coefficients[2]

print(c(y_int, slope))

summary(three_fit)

#c
plot(three_fit$residuals~predict(three_fit))

#d a 98% confidence interval ??1 and interpret
x_mean = mean(two_file$"X.MPH.")
x_sd = sd(two_file$"X.MPH.")
n = length(two_file$"X.MPH.")
SSX = (n-1)*x_sd^2
print(n)

SSE = sum(three_fit$residuals^2)

MSE = (1/(n - 2))*SSE
RMSE = sqrt(MSE)
RMSE
slope_sd = RMSE/sqrt(SSX)
alpha = .02

CILB_slope = slope - qt(1-alpha/2,n-2)*slope_sd
CIUB_slope = slope + qt(1-alpha/2,n-2)*slope_sd
print(c(CILB_slope, CIUB_slope))

#e
alpha = 0.01
H_null = 1
t_stat = (slope - H_null)/slope_sd
print(t_stat)
p_value = 2*pt(t_stat, n-2, lower.tail = T)
t = qt((1-alpha/2), n-2)
print(t)
print(t_stat)
p_value

#f 95% Confidence Bands Implementation
X = seq(min(two_file$"X.MPH."),max(two_file$"X.MPH."),length.out=30)
predict = y_int + slope*X
predict

confintline = function(X,Sx,Yhat,n, Xbar,RMSE,CL){
  alpha = 1 - CL
  CILB= Yhat - qt(1-alpha/2,n-2)*RMSE/sqrt((n-1)*Sx^2)
  CIUB = Yhat + qt(1-alpha/2,n-2)*RMSE/sqrt((n-1)*Sx^2)
  return(cbind(CILB,CIUB))
}
CL= 0.95

CI= confintline(X, x_sd, predict, n, x_mean, RMSE, CL)
CI

plot(two_file$"X.SQRTDIST."~two_file$"X.MPH.")
abline(three_fit,col = "purple")
lines(X,CI[,"CILB"],lty='dashed',col = 'red')
lines(X,CI[,"CIUB"],lty='dashed',col = 'red')

#g
y_pred = y_int + slope*45
y_pred
x = 45
alpha = 1-.95
CI_pred = c(y_pred - qt(1-alpha/2,n-2)*RMSE*sqrt((1/n)+ (x-x_mean)^2/SSX), y_pred + qt(1-alpha/2,n-2)*RMSE*sqrt((1/n)+ (x-x_mean)^2/SSX))
CI_pred


#4
four_file = read.csv('C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\Hw3\\ch05q08.txt', header = T)
colnames(four_file)

plot(four_file$"X.SAL."~four_file$"X.CGPA.")

#a
fr_fit = lm(four_file$"X.SAL."~four_file$"X.CGPA.")
summary(fr_fit)
print(c(fr_fit$coefficients[1], fr_fit$coefficients[2]))

#b
abline(fr_fit, col = 'blue')

#c Residual Graphs
par(mfrow = c(1,3))
plot(fr_fit$residuals~predict(fr_fit))
hist(fr_fit$residuals)
qqnorm(fr_fit$residuals)


#5
age = c(8, 10, 1*12, 1*12 + 3, 1*12 + 6, 1*12 + 9, 2*12, 2*12+6, 3*12, 3*12 + 6, 4*12, 4*12 + 6, 5*12, 5*12 + 6, 6*12)
voc_size = c(0, 1, 3, 19, 22, 118, 272, 446, 869, 1222, 1540, 1870, 2072, 2289, 2562)
print(c(length(age), length(voc_size)))
print(age)


fit_v = lm(voc_size~age)
plot(fit_v$residuals~predict(fit_v))
hist(fit_v$residuals)
qqnorm(fr_fit$residuals)

#6
fv_file = read.table('C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\Hw3\\LIFT.txt', header = T)
colnames(fv_file)

#a
par(mfrow = c(1,1))
plot(fv_file$"ARMSTRENGTH"~fv_file$"DYNAMICLIFT")
fv_fit = lm(fv_file$"ARMSTRENGTH"~fv_file$"DYNAMICLIFT")
summary(fv_fit)

#b
r = 0.1542^2
r

#c
slope_a = fv_fit$coefficients[2]

n = length(fv_file$"DYNAMICLIFT")
n
x_sd = sd(fv_file$"DYNAMICLIFT")
SSX = (n-1)*x_sd^2

SSE = sum(fv_fit$residuals^2)
SSE
MSE = (1/(n - 2))*SSE
MSE
RMSE = sqrt(MSE)
RMSE
slope_sd = RMSE/sqrt(SSX)
slope_sd
SSX

test_stat = (slope_a - 0)/slope_sd
pval = 2*pt(test_stat, n-2, lower.tail = F)
t = qt(1-.05/2, n-2)
print(c(t, test_stat, pval))

#d
anova(fv_fit)