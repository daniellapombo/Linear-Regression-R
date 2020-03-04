#Normal Distribution (Z)
#Probability: pnorm()
#Critical z-value qnorm(area, mean,sd)

#*T-distribution
#Probability: pt(t,df,side(tail))
#Critical t value qt(area, df)

#F-distribution
#Usually F compares variance 
#Probability pf(F, df1, df2, side(tail))
#Critical f value qf(area, df1, df2)

qf()

tc = qt(0.95,19)
tc

pv = 1-pt(1.44,19, lower.tail = F)
pv

probz = 2*pnorm(2.1, mean = 0, sd =1, lower.tail = F)
probz


#Probability that z is greater than 292
x = pnorm(292,269,16,lower.tail = F)
#Probability that z is less than 292
y = 1-x


