brand = c("FRX Road", "Tx 120", "Rider A10", "FRX X60", "TX 480", "TX 1000" )
weight = c(37, 35, 30, 29, 29, 28)
price = c(640, 660, 820, 790, 840, 970)

mean(price)
summary(price)
hist(weight)
hist(price)
plot(price~weight)

df = data.frame(brand, weight, price)
print(df)

df$price
plot(df$price~df$weight, pch = 3, col = 'red', main = "Price vs Weight of Mountain Bike")

#Import & read tabular txt file
leadFile = read.table("C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\LEAD.txt", header = TRUE)

print(leadFile)
head(leadFile)
summary(leadFile)

#Scatter plot
plot(iqf~iqv, data = leadFile) #Produces scatter plot

#Correlation coefficient
cor(leadFile$iqf, leadFile$iqv) #Produces and processes correlation coefficient

#Linear Regressoin
fit.leadFile = lm(iqf~iqv, data = leadFile) #Produces linear regression
summary(fit.leadFile) #Gives basic info about regression

#Import CSV
cost = read.table(file = "C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\PUPILCOSTATLANTA.csv", sep = ",", header = T)

head(cost)

plot(COSTPERPUPIL~PUPILTEACHERRATIO, data = cost)
cor(cost$COSTPERPUPIL, cost$PUPILTEACHERRATIO)
fit.cost = lm(COSTPERPUPIL~PUPILTEACHERRATIO, data = cost)
plot(COSTPERPUPIL~PUPILTEACHERRATIO, data = cost)
abline(fit.cost)

