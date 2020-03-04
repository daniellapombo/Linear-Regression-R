men_haha = c(89, 90, 52, 97, 86, 88)
women_haha = c(84,97, 58, 90, 75)

men_mean = mean(men_haha) 
men_sd = sd(men_haha) #finds standard deviation

women_mean = mean(women_haha)
women_sd = sd(women_haha) # Computes the sample standard deviation

N.one = length(men_haha)
N.two = length(women_haha)

se = sqrt(men_sd^2/N.one + women_sd^2/N.two)

df= N.one + N.two -2

te = qt(0.975, df)

lower_bound = (men_mean - women_mean) - te*se
upper_bound = (men_mean - women_mean) + te*se

CI = c(lower_bound, upper_bound) #-18.35, 24.086
print(CI)

Ttest = t.test(men_haha, women_haha, alternative = "two.sided", conf.level = .95)
names(Ttest) 
T_stat = Ttest$statistic #0.306
print(T_stat)
print(Ttest$p.value)
#We are 95% confidence that the population mean  difference in the 'humor' of men and women is between CI:[-18.35,24.086]
