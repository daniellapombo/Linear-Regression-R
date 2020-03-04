#1

#y = b0 + ax + bx^2 + cx^3 + dx^4

file_one = read.csv("C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\question1.txt", header = T)
colnames(file_one) #Prints column names
#a
#plot(file_one$"y1"~file_one$"x1") #y = bx^4 + c
#b
#plot(file_one$"y2"~file_one$"x2") #y = bx + c
#c
plot(file_one$"y3"~file_one$"x3")


#3
file_three = read.csv("C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\ch05q01.txt", header = T)
colnames(file_three)
#a 
#plot(file_three$"X.DRYWGT."~file_three$"X.AGE.")
plot(log(file_three$"X.DRYWGT.", 10)~file_three$"X.AGE.")

#b Regression line
fit.y = lm(file_three$"X.DRYWGT."~file_three$"X.AGE.")  #Fits linear model
fit.z = lm(log(file_three$"X.DRYWGT.", 10)~file_three$"X.AGE.") #Fits linear model
summary(fit.y)
summary(fit.z)

slope.y = fit.y$coefficients[2]
slope.z = fit.z$coefficients[2]

intercept.y = fit.y$coefficients[1]
intercept.z = fit.z$coefficients[1]


#y.hat = intercept.y + slope.y*x
#z.hat = intercept.z + slope.y*x
y.hat = c(intercept.y, slope.y)
z.hat = c(intercept.z, slope.y)
y.hat
z.hat

#c Calc SSE
SSE.y = sum(fit.y$residuals^2)
SSE.z = sum(fit.z$residuals^2)

SSE.y
SSE.z

#d
plot(file_three$"X.DRYWGT."~file_three$"X.AGE.")
abline(fit.y, col = 'red')
plot(log(file_three$"X.DRYWGT.", 10)~file_three$"X.AGE.")
abline(fit.z, col = 'green')

#4
file_four = read.csv("C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\ch05q12.txt", header = T)
colnames(file_four) #Prints column names
#a
age_months = c(12*file_four$"X.YEARS."+file_four$"X.MONTHS.")
age_months
plot(file_four$"X.VOC_SIZE."~age_months)
#b
fit.voc = lm(file_four$"X.VOC_SIZE."~age_months) #Fits linear model 
summary(fit.voc)
slope.voc = fit.voc$coefficients[2]
intercept.voc = fit.voc$coefficients[1]
print(c(intercept.voc, slope.voc))

SSE.voc = sum(fit.voc$residuals^2)
print(SSE.voc)

#e
x = 30*12
y.vocab_hat = -621.126 + 43.893*x
(y.vocab_hat)

#f
plot(file_four$"X.VOC_SIZE."~age_months)
abline(fit.voc, col = 'red')

#g
#?append
vocab.size = c(0, file_four$"X.VOC_SIZE.")
age_m = c(0, age_months)
print(vocab.size)
print(age_m)
fit.new = lm(vocab.size~age_m)

slope = fit.new$coefficients[2]
int = fit.new$coefficients[1]

plot(vocab.size~age_m)
abline(fit.new, col = 'red')
