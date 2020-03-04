#1
#a Calc P(Z>= -1) = ?
one.a.ans = pnorm(q = -1, mean = 0, sd = 1, lower.tail = F)
print(one.a.ans)
#[1] 0.8413447
#b P(Z<= ?) = .20
#qnorm is the inverse of pnorm (cdf)
#Used to answer 'what is the Z-score of th epth quantile of the normal distribution?
one.b.ans = qnorm(.20)
print(one.b.ans) #is lower tail
#[1] -0.8416212

#2
#a P(F 6,24 >= ?) = 0.05
two.a.ans = qf(.05, df1 = 6, df2 = 24,lower.tail = F)
print(two.a.ans)
#[1] 2.508189
#b P(F 5,40 >= 2.9) = ?
two.b.ans = df(x = 2.9, df1 = 6, df2 = 24)
print(two.b.ans)
#[1] 0.0402719

#3
scores = c(0,2,5,6,3,3,3,1,4,3)
s.mean = mean(scores)
s.median = median(scores)
s.var = var(scores)
three.ans = c(s.mean,s.median,s.var)
print(three.ans)
#[1] 3.000000 3.000000 3.111111

#4
n = 32
samp_mean= 30
samp_sd = 11
CL = .99
alpha = .01/2
#CI = [samp_mean +- t_stat*(samp_sd/sqrt(32))]
t_stat = abs(qt(p = (.01/2), df = (32-1) ))
print(t_stat)
four.ans.CI = c((samp_mean - t_stat*(samp_sd)/sqrt(n)), (samp_mean + t_stat*(samp_sd)/sqrt(n)))
print(four.ans.CI)
#[1] 24.66409 35.33591

#5
sam_dat = read.delim("C:\\Users\\danie\\Documents\\School\\Math\\Stat 308\\Hw\\salmonella.txt", header = F)
head(sam_dat)
print(sam_dat[1,1])
sam_dat = unlist(sam_dat)
#H0 = .3, Ha < .3
null_H = .3
alt_H <= .3
sam_mean = mean(sam_dat)
sam_sd = sd(sam_dat)
sam_n = length(sam_dat)
degf = sam_n - 1
t.alpha = qt(p= .05, df = degf)
t_stat = (sam_mean - null_H)/(sam_sd/sqrt(sam_n))
p_value = pt(t_stat, df = degf, lower.tail = T)
print(p_value)
#[1] 0.4482166
#?t.test()
t_test = t.test(sam_dat, mu = null_H, alternative = "less", conf.level = .95)
print(t_test)
#	One Sample t-test

#data:  sam_dat
#t = -0.13098, df = 41, p-value = 0.4482
#alternative hypothesis: true mean is less than 0.3
#95 percent confidence interval:
#  -Inf 0.3253898
#sample estimates:
#  mean of x 
#0.2978571 

# Fail to reject the null as pvalue = .4482 > alpha = .05; 
#Therefore there is not enough evidence, currently, to state that the saminella levels are less than .03.  

