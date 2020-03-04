#Hypothesis test code!

#Find critical value
cr <-- qt(0.05, 19) #Generates invT()
print(cr)

#Test statistic
t_stat = (558.29-500)/(181.43/sqrt(20)) #Test statistic
print(t_stat)

#HTest
pval = pt(t_stat, df=19, lower.tail=F) #Generate p-value aka Tcdf(t_stat, +inf, df)
print(pval) #Upper tail value

inside = pnorm(2.1, mean=0, sd=1, lower.tail= T)- pnorm(-2.1, mean=0, sd=1, lower.tail= T)
print(inside)
res = 1-(inside)
print(res)
