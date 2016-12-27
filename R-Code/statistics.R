##### visualization/descriptive statistics and linear regression 


# this scripts provides the code for the visualization/descriptive statistic and the linear regression 
# reponse variable is the switching row 


# 1) calculate descriptive statistic for various variables 
# 2) visualization of my response variable 
# 3) apply several transformation to my response variable 
# 4) conduct parametric and non-parametric test for my response variable 
    # parametirc test of the response variable for some subgroups 
# 5) linear regression 
      # linear regression on some subgroups 
# 7) Robustnes 
  # transform switching row with log 
  # tobit model that controls for censored data 
 


# set the working directory for all the figures of the section 
#setwd(paste(wkdir, "/figures/", sep = ""))


# baisc statistics 
# switching row (various desctriptive statistics)
mean(dtnew$T012, na.rm = T) # 13.422
sd(dtnew$T012, na.rm = T) # 7.276
var(dtnew$T012, na.rm = T) # 52.946 
median(dtnew$T012, na.rm = T) # 15 


# return (mean and standard deviation)
mean(dtnew$return, na.rm = T) # 33.555 
sd(dtnew$return, na.rm = T) # 18.19097 

#age (various descritpve statistics)
median(dtnew$age, na.rm = T) # 44
mean(dtnew$age, na.rm = T) # 46.09 
sd(dtnew$age, na.rm = T) # 18.0117 
range(dtnew$age, na.rm = T) # 14-90 

# distribution for all the religious communities  
(tablereli <- table(dtnew$relicom))
prop.table(tablereli)*100

# Gender distributio  
(tablegender <- table(dtnew$male))
prop.table(tablegender)*100

# how many people in the sample are from west/east Germany 
(tablewest <- table(dtnew$west))
prop.table(tablewest)*100

####################### visualize the data


# make a histogram of the switching row with a normal distribution 
tikz(file = "histswitchingrow.tex", width = 5, height = 5)
ggplot(dtnew, aes(T012))+
  geom_histogram(aes(y=..density..), bins = 20)+
  stat_function(fun = dnorm, colour = "red", args = list(mean = mean(dtnew$T012), sd = sd(dtnew$T012)), lwd = 0.9)+
  xlab("Switching row")
dev.off()

# data does not look normally distributed as most observations are on the very reight end 
# data also looks right censored/this is true as for all participants that do not switch the row a 
# switiching row of 21 is assumed 

# check the normality with a qqplot 

# try the same with ggplot 
ggplot(dtnew, aes(sample = T012))+
  stat_qq(geom="lines")
# qqline is still missing 

# normality of switching row with base package 
tikz(file = "QQplotswitching.tex", width = 5, height = 5)
qqnorm(dtnew$T012)
qqline(dtnew$T012, col = 2, lwd = 0.9)
dev.off()

# how would it look like if the data is normally distributed 
switch.t <- rnorm(500, mean(dtnew$T012), sd(dtnew$T012) )

qqnorm(switch.t)
qqline(switch.t)

# compare the two qqplots (normality) in one window 


tikz(paste(p.figures,"normalityswitchingrow.tex"), width = 5, height = 5)
par(mfrow = c(2,1))

qqnorm(dtnew$T012)
qqline(dtnew$T012)

qqnorm(switch.t)
qqline(switch.t)
dev.off()

# T012 does not look normally distributed so parametric test are probably not appropriate 
# sample size is large enough so central limit theorem applies 


# check the normality of T012 with a base histogram and add a normal distribution 
hist(dtnew$T012, density=20, breaks=20, prob=TRUE, xlim = c(0,21))
curve(dnorm(x, mean = mean(dtnew$T012), sd = sd(dtnew$T012)), 
      col="darkblue", lwd=2, add=TRUE)


############# transform the data 

# log transformation 
logT12 <- log(dtnew$T012)

hist(logT12)
#does not look normally distributed 

qqnorm(logT12)
qqline(logT12)
# does not look normally distributed 
####### look transformation is not a good transformation for this data 

# arcsin transformation
asinT012 <- asin(sqrt(dtnew$T012))
######## get a lot of NaN 

# histogram 
hist(asinT012)

# sqrt transformation 
sqrtT012 <- sqrt(dtnew$T012)

# histogram 
par(mfrow = c(1,1))
hist(sqrtT012)

qqnorm(sqrtT012)
qqline(sqrtT012)
####### does not look normally distributed 

# square transformaton 
squT012 <- dtnew$T012^2

# histogram 
hist(squT012)

qqnorm(squT012)
qqline(squT012)
######## does not look normally distributed 

# reciprocal 
reciT012 <- 1/dtnew$T012

# hist 
hist(reciT012)
######### violates assumptions of transformation 

# antilog 
expT012 <- exp(dtnew$T012)


#hist 
hist(expT012)
########### not appropriate 

########### no transformation is appropriate so we go on with the untransformed T012 


############ testing for normality Shapiro test 
# check if the switching row is normally distributed 
shapiro.test(dtnew$T012) # p< 0.0001 -> T012 is likely not normally distrbuted 

# t-Test check whether the means of religious and non-religious people are different
# I use a Welch´s t-test as the variances are not the same 

# switching row for religious or not 
tapply(dtnew$T012, dtnew$religion, mean, na.rm = T) # 0=12.45 1 = 13.81 

# test if the variances of religious and non-religious are equal so we can decide which t-test we can use  
tapply(dtnew$T012, dtnew$religion, var) # 0 = 58.24937 1 = 50.42187 

# variances are not equal so we should use a Welch´s t-test 


# standard deviation of religious and no-religious participants 
tapply(dtnew$T012, dtnew$religion, sd) # 0 = 7.632127, 1= 7.100836 
# sd are very close  

# Welchs t-test 
t.test(dtnew$T012~dtnew$religion, conf.level =0.90)

# difference in religious intensity 
# t.test(dtnew$T012~ dtnew$reliintens, conf.level=0.90)




# Wilcox-Test (nonparametric test) -> we use a nonparametric test as the T012 is not normally distributed 
# sign test 
wilcox.test(dtnew$T012~dtnew$religion, conf.level=0.95)
# what is the interpretation of the Wilcox-Test 


# test wether there is a difference in religious intensity 
#wilcox.test(dtnew$T012~ dtnew$reliintens, conf.level=0.90)


# we stick with the original T012 as the CLT applies 
# variance test 
# F-test when the distributions are normal 
var.test(dtnew$T012~ dtnew$religion, conf.level=0.95)

# levene test when the distributions are not normal 
leveneTest(dtnew$T012,dtnew$religion)


####### investigate the switching row for various subgroups 

# switching row for the religious subgroups
tapply(dtnew$T012, dtnew$relicom, mean, na.rm=T)
tapply(dtnew$T012, dtnew$relicom, sd, na.rm=T)


# switching row for male and female 
tapply(dtnew$T012, dtnew$male, mean, na.rm = T)
tapply(dtnew$T012, dtnew$west, mean, na.rm = T)

# switching row for religious or not 
tapply(dtnew$T012, dtnew$religion, mean, na.rm = T)

# switching row for religious intensity 
tapply(dtnew$T012, dtnew$reliintens, mean, na.rm=T)


# religious communities 
ggplot(data.frame(dtnew), aes(x=dtnew$relicom))+
  geom_bar()



######## linear regression: 

# switching row ~ religion 
reg1 <- lm(dtnew$T012 ~ dtnew$religion, na.action = na.omit) 
summary(reg1)
# participant that are in a religious comunity have a switiching row that is 1.36 higher and therefore discount the future more 


# update reg1 for the first anova/we need the same amount of rows 
reg1b <- lm(reg1, subset = !is.na(dtnew$scaledbig_g) & !is.na(dtnew$scaledbig_e) & !is.na(dtnew$scaledbig_n) & !is.na(dtnew$scaledbig_o) & !is.na(dtnew$scaledbig_v) & !is.na(dtnew$male))
summary(reg1b)

# uptdate for reg1 for first anova 
reg1c <- lm(reg1, subset = !is.na(dtnew$male) & !is.na(dtnew$constraint) & !is.na(dtnew$logincom) & !is.na(dtnew$age))
summary(reg1c)

####### regression on subpopulation 

# age <= 44 and >44 
sub1 <- lm(dtnew$T012~ dtnew$religion, subset = dtnew$age<=44, na.action = na.exclude)
summary(sub1)

sub2 <- lm(dtnew$T012 ~ dtnew$religion, subset = dtnew$age>44, na.action = na.exclude)
summary(sub2)


# subset for gender 
sub3 <- lm(dtnew$T012 ~ dtnew$religion, subset = dtnew$male==1, na.action = na.exclude)
summary(sub3)

sub4 <- lm(dtnew$T012~ dtnew$religion, subset = dtnew$male==0, na.action = na.exclude)
summary(sub4)

############## 

# variable other degree will be exclude from the regression analysis as we do not have a observation 

# control for age, male, logincome constraint 
reg2 = lm(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint, na.action = na.omit)
summary(reg2)

# update reg2 for the second anova 

reg2b <- lm(reg2, subset =!is.na(dtnew$scaledbig_g) & !is.na(dtnew$scaledbig_e) & !is.na(dtnew$scaledbig_n) & !is.na(dtnew$scaledbig_o) & !is.na(dtnew$scaledbig_v) )

# controll for  all control variables 
reg3 = lm(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
          dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male+ dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree , na.action = na.exclude)
summary(reg3)

# update reg3 for regression 
reg3b = lm(reg3, subset = !is.na(dtnew$scaledbig_g) & !is.na(dtnew$scaledbig_e) & !is.na(dtnew$scaledbig_n) & !is.na(dtnew$scaledbig_o) & !is.na(dtnew$scaledbig_v) & !is.na(dtnew$logincom) & !is.na(dtnew$constraint) & !is.na(dtnew$age) )

# reg3 without interactions for the linear assumption 
reg3c <-  lm(dtnew$T012~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v  + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree , na.action = na.omit)
summary(reg3c)



######### ANOVA 

# first anova: compare reg1 with reg2
anova(reg1c, reg2)

# second anova: compare reg2 with reg3
anova(reg2b, reg3)


###### export the three regression outputs 
stargazer(reg1, reg3,
          order = c(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,12,13,14,15,21), 
          dep.var.labels = c("Switching row"),
          covariate.labels = c("Religionszugehoerigkeit", "Alter", "Alter quadriert", "Male", "Einkommen (log)", "Kreditbeschraenkung",
                     "Gewissenhafigkeit", "Extraversion", "Neurotizismus", "Offenheit", "Vertraeglichkeit", "Gewissenhaftikeit*Male",
                     "Extraversion*Male", "Neurotizismus*Male", "Offenheit *Male", "Vertraeglichkeit*Male", "Abitur", "Realschulabschluss", "Besucht die Schule",
                     "Kein Schulabschluss", "Constant"), 
          align = TRUE,
          no.space = TRUE)





# this analysis is not part of the paper 

# control for age, male, logincome, constraint and education 
reg4 = lm(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree, na.action = na.omit)
summary(reg4)


# check if ther are NA´s 
any(is.na(dtnew$T012))
any(is.na(dtnew$religion))
any(is.na(dtnew$male))
any(is.na(dtnew$abi))
any(is.na(dtnew$real))
any(is.na(dtnew$enrolled))
any(is.na(dtnew$nodegree))

# whith NA´s 
any(is.na(dtnew$scaledwordtest))
any(is.na(dtnew$scaledsymtest))
any(is.na(dtnew$scaledbig_v))
any(is.na(dtnew$scaledbig_o))
any(is.na(dtnew$scaledbig_n))
any(is.na(dtnew$scaledbig_e))
any(is.na(dtnew$scaledbig_g))
any(is.na(dtnew$constraint))
any(is.na(dtnew$logincom))
any(is.na(dtnew$age))



############ robustnes 

# Log of switching row 
rob1 = lm(log(dtnew$T012) ~ dtnew$religion, na.action = na.omit)
summary(rob1)

rob2 = lm(log(dtnew$T012) ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
            dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male+ dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree , na.action = na.exclude)
summary(rob2)


stargazer(rob1, rob2,
          order = c(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,12,13,14,15,21), 
          dep.var.labels = c("Switching row (log)"),
          covariate.labels = c("Religionszugehoerigkeit", "Alter", "Alter quadriert", "Male", "Einkommen (log)", "Kreditbeschraenkung",
                               "Gewissenhafigkeit", "Extraversion", "Neurotizismus", "Offenheit", "Vertraeglichkeit", "Gewissenhaftikeit*Male",
                               "Extraversion*Male", "Neurotizismus*Male", "Offenheit *Male", "Vertraeglichkeit*Male", "Abitur", "Realschulabschluss", "Besucht die Schule",
                               "Kein Schulabschluss", "Constant"), 
          align = TRUE,
          no.space = TRUE)

# religious intensity 

rob3 = lm(dtnew$T012 ~ dtnew$reliintens, na.action = na.omit)
summary(rob3)


rob4 = lm(dtnew$T012 ~ dtnew$reliintens + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
            dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male+ dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree , na.action = na.exclude)
summary(rob4)




# for different religious subgroups 
rob5 <- lm(dtnew$T012 ~ dtnew$relicom)
summary(rob5)

rob6 <- lm(dtnew$T012 ~ dtnew$relicom + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
             dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male+ dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree , na.action = na.exclude)
summary(rob6)

####### Tobit model that controls for consored data/ not used in the analysis 
# tobit model with 
t1 <- vglm(dtnew$T012 ~ dtnew$religion, tobit(Lower = 1, Upper = 21), na.action = na.omit)
summary(t1)


t2 <- vglm(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
             dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male + dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled, tobit(Lower = 1, Upper = 21), na.action = na.omit)

# why can´t I include dtnew$nodegree I think I just have 3 observations 

summary(t2)

# stargazer does not function for tobit models/mache es selbst in LaTex 


##########################################


