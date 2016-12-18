##### statistics 

# baisc statistics 
# switching row 
mean(dtnew$T012, na.rm = T) # 13.422
sd(dtnew$T012, na.rm = T) # 7.276
var(dtnew$T012, na.rm = T) # 52.946 
median(dtnew$T012, na.rm = T) # 15 


# return 
mean(dtnew$return, na.rm = T) # 33.555 
sd(dtnew$return, na.rm = T) # 18.19097 

#age 
median(dtnew$age, na.rm = T) # 44
mean(dtnew$age, na.rm = T) # 46.09 
sd(dtnew$age, na.rm = T) # 18.0117 
range(dtnew$age, na.rm = T) # 14-90 

# religious communities with table 
(tablereli <- table(dtnew$relicom))
prop.table(tablereli)*100

# gender 
(tablegender <- table(dtnew$male))
prop.table(tablegender)*100



# west 
(tablewest <- table(dtnew$west))
prop.table(tablewest)*100

########## visualize the data
tikz(file = "switchingrow.tex", width = 5, height = 5)
ggplot(dtnew, aes(T012 !=21))+
  geom_histogram(aes(y=..density..), bins = 20)+
  stat_function(fun = dnorm, colour = "red", args = list(mean = mean(dtnew$T012), sd = sd(dtnew$T012)), lwd = 0.9)+
  ggtitle("switching row with normal distribution")+
  xlab("switching row")
dev.off()


# does not look normally distributed, right censored 

# check the same with a qqplot 
# try the same with ggplot 
ggplot(dtnew, aes(sample = T012))+
  stat_qq(geom="lines")


# base package 
qqnorm(dtnew$T012)
qqline(dtnew$T012)

# how would it look like if the data is normally distributed 
switch.t <- rnorm(500, mean(dtnew$T012), sd(dtnew$T012) )

qqnorm(switch.t)
qqline(switch.t)

# compare the two plots in one window 
tikz(file = "normalityswitchingrow.tex", width = 5, height = 5)
par(mfrow = c(2,1))

qqnorm(dtnew$T012)
qqline(dtnew$T012)

qqnorm(switch.t)
qqline(switch.t)

dev.off()


# T012 does not look normally distributed so parametric test are probably not appropriate 
# sample size is large enough so central limit theorem 

# shows that there is a substantial heterogenity 

# check the normality of T012 with a base plot
hist(dtnew$T012, density=20, breaks=20, prob=TRUE, xlim = c(0,21))
curve(dnorm(x, mean = mean(dtnew$T012), sd = sd(dtnew$T012)), 
      col="darkblue", lwd=2, add=TRUE)


############# transform the data 

# log transformation 
logT12 <- log(dtnew$T012)

hist(logT12)
# does not look normally distributed 

qqnorm(logT12)
qqline(logT12)
# does not look normally distributed 
# look transformation is not a good transformation for this data 

# arcsin transformation
asinT012 <- asin(sqrt(dtnew$T012))
# get a lot of NaN 

# histogram 
hist(asinT012)

# sqrt transformation 

sqrtT012 <- sqrt(dtnew$T012)

# histogram 
par(mfrow = c(1,1))
hist(sqrtT012)

qqnorm(sqrtT012)
qqline(sqrtT012)
# does not look normally distributed 

# square transformaton 

squT012 <- dtnew$T012^2

# histogram 
hist(squT012)

qqnorm(squT012)
qqline(squT012)
# does not look normally distributed 

# reciprocal 
reciT012 <- 1/dtnew$T012

# hist 
hist(reciT012)
# not violates assumptions of transformation 

# antilog 
expT012 <- exp(dtnew$T012)


#hist 
hist(expT012)
# not appropriate 

# no transformation is appropriate so we go on with the untransformed T012 

# testing for normality 
# check if the switching row is normally distributed 
shapiro.test(dtnew$T012) # p< 0.0001 -> T012 is likely not normally distrbuted 

# are the test normally distributed 
shapiro.test(dtnew$wordtest)
shapiro.test(dtnew$symtest)




# Testing for Runs 
runs.test(as.factor(dtnew$religion))
runs.test(as.factor(dtnew$male))




# t-Test check out what does alternative mena? is it Ho?
# why have I choosen this t-test 

# test if the variances are equal so we know which t-test we can use 
tapply(dtnew$T012, dtnew$religion, var) # 0 = 58.25, 1= 50,42 
# variances are not equal so we should use a Welchs t-test 

# Welchs t-test 
t.test(dtnew$T012~dtnew$religion, conf.level =0.90)

# difference in religious intensity 
t.test(dtnew$T012~ dtnew$reliintens, conf.level=0.90)
# Ho: both group switch at the same row 
# we can reject the Ho at an alpha = 5% sig. level. that both groups swith at the same group 


# Wilcox-Test (nonparametric test) -> we use a nonparametric test as the T012 is not normally distributed 
# sign test 
wilcox.test(dtnew$T012~dtnew$religion, conf.level=0.90)
# what is the interpretation of the Wilcox-Test 

wilcox.test(dtnew$T012~ dtnew$reliintens, conf.level=0.90)


################ randomization and resampling 



# we stick with the original T012 as the CLT applies 
# variance test 
# F-test when the distributions are normal 
var.test(dtnew$T012~ dtnew$religion, conf.level=0.90)

# levene test when the distributions are not normal 
leveneTest(dtnew$T012,dtnew$religion)
# check how to interpret this result 




# Correlation
# what do we measure with a correlation? 

# spearman test as we have not normal distributed data 
# for normal distributed data we can use pearson: I should do this as well as switching row is not normal distributed 


cor.test(dtnew$religion, dtnew$T012, method = "spearman")
cor.test(dtnew$religion, dtnew$T012, method = "pearson")


cor.test(dtnew$reliintens, dtnew$T012, method = "spearman")

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


# graphs for different groups

# Introducation in Statistic with R page 74

# religious communities 
ggplot(data.frame(dtnew), aes(x=dtnew$relicom))+
  geom_bar()


# regression on subpopulation 

sub1 <- lm(dtnew$T012~ dtnew$religion, subset = dtnew$age<=44, na.action = na.exclude)
summary(sub1)

sub2 <- lm(dtnew$T012 ~ dtnew$religion, subset = dtnew$age>44, na.action = na.exclude)
summary(sub2)

sub3 <- lm(dtnew$T012 ~ dtnew$religion, subset = dtnew$male==1, na.action = na.exclude)
summary(sub3)

sub4 <- lm(dtnew$T012~ dtnew$religion, subset = dtnew$male==0, na.action = na.exclude)
summary(sub4)



# export subgroups 
stargazer(sub1, sub2, sub3, sub4,
          out = "table3.tex",
          dep.var.labels = c("Ungeduld (Experimentergebnis)"), 
          column.labels = c("Alter>=44", "Alter>44", "Männlich", "Weiblich"),
          covariate.labels = c("Religionszugehoerigkeit", "Constant"),
          align = TRUE, 
          no.space = TRUE)


######## linear regression: 

# switching row ~ religion 
reg1 <- lm(dtnew$T012 ~ dtnew$religion) 
summary(reg1)
# participant that are in a religious comunity have a switiching row that is 1.36 higher and therefore discount the future more 

plot(reg1)

outlier.test(reg1)
influence.measures(reg1)

# check for autocorrelation 
dwtest(reg1) 

# refit r1 so that we have the same number of rows 
reg1a <- update(reg1, subset = !is.na(dtnew$scaledbig_g) & !is.na(dtnew$scaledbig_e) & !is.na(dtnew$scaledbig_n) & !is.na(dtnew$scaledbig_o) & !is.na(dtnew$scaledbig_v) & !is.na(dtnew$male) & !is.na(dtnew$scaledwordtest) & !is.na(dtnew$scaledsymtest) & !is.na(dtnew$constraint) & !is.na(dtnew$logincom) & !is.na(dtnew$age))
summary(reg1a)

reg1b <- update(reg1, subset = !is.na(dtnew$scaledbig_g) & !is.na(dtnew$scaledbig_e) & !is.na(dtnew$scaledbig_n) & !is.na(dtnew$scaledbig_o) & !is.na(dtnew$scaledbig_v) & !is.na(dtnew$male))
nobs(reg1a)


any(is.na(dtnew$scaledbig_g))
any(is.na(dtnew$scaledbig_e))
any(is.na(dtnew$scaledbig_n))
any(is.na(dtnew$scaledbig_o))
any(is.na(dtnew$scaledbig_v))
any(is.na(dtnew$male))


# controll for intelligence 
reg6 = lm(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
          dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male + dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree  + dtnew$scaledsymtest + dtnew$scaledwordtest, na.action = na.exclude)
summary(reg6)


# no NA´s 
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

# compare the two models 
anova1 <- anova(reg1a, reg6)

stargazer(anova1)
# update regression r6 
reg6a <- update(reg6, subset = !is.na(dtnew$scaledwordtest) & !is.na(dtnew$scaledsymtest) & !is.na(dtnew$scaledbig_g) & !is.na(dtnew$scaledbig_e) & !is.na(dtnew$scaledbig_n) & !is.na(dtnew$scaledbig_o) & !is.na(dtnew$scaledbig_v) & !is.na(dtnew$male) & !is.na(dtnew$constraint) & !is.na(dtnew$logincom) & !is.na(dtnew$age)) 
summary(reg6a)
 

stargazer(reg1, reg6, 
          out = "table4",
          order = c(1,2,3,4,5,6,7,8,9,10,11,18,19,20,21,22,12,13,14,15,16,17,24),
          dep.var.labels = c("Switching row"),
          covariate.labels = c("Religionszugehoerigkeit", "Alter", "Alter quadriert", "Male", "Einkommen (log)", "Kreditbeschraenkung",
                               "Gewissenhafigkeit", "Extraversion", "Neurotizismus", "Offenheit für Erfahrungen", "Vertraeglichkeit", "Gewissenhaftikeit*Male",
                               "Extraversion*Male", "Neurotizismus*Male", "Offenheit für Erfahrungen*Male", "Vertraeglichkeit*Male", "Abitur", "Realschulabschluss", "Besucht die Schule",
                               "kein Schulabschluss", "Kognitiver Test I (nonverbaler Test)", "Kognitiver Test II (verbaler Test)", "Constant"), 
          align = TRUE,
          no.space = TRUE)


# Robustness 


# subgroups 
rsub = dtnew %>% group_by(relicom) %>% do(model = lm(dtnew$T012~ dtnew$religion))
rsub
rsub$model

rsub %>% tidy(model)
rsub %>% glance(model)

# how can I see all variables 

# one-way anova 
oneway.test(dtnew$T012~ dtnew$religion)

# difference between means of groups 
rsub2<- oneway.test(dtnew$T012~ dtnew$relicom, subset = 230:500)
# how to exclude one observation 'Andere religioese Gemeinschaft' 

TukeyHSD(rsub2)

# rebust anova (kruskal-wallis test) -> are the group medians significantly different? 
kruskal.test(dtnew$T012~ dtnew$religion)

# table 
table(dtnew$T012, dtnew$religion)




# controll for log()
reg9 = lm(log(dtnew$T012) ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
          dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male + dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree  + dtnew$scaledsymtest + dtnew$scaledwordtest, na.action = na.exclude)
summary(reg9)

stargazer(reg9,
          order = c(1,2,3,4,5,6,7,8,9,10,11,18,19,20,21,22,12,13,14,15,16,17,24),
          dep.var.labels = c("Switching row"),
          covariate.labels = c("Religionszugehoerigkeit", "Alter", "Alter quadriert", "Male", "Einkommen (log)", "Kreditbeschraenkung",
                               "Gewissenhafigkeit", "Extraversion", "Neurotizismus", "Offenheit für Erfahrungen", "Vertraeglichkeit", "Gewissenhaftikeit*Male",
                               "Extraversion*Male", "Neurotizismus*Male", "Offenheit für Erfahrungen*Male", "Vertraeglichkeit*Male", "Abitur", "Realschulabschluss", "Besucht die Schule",
                               "kein Schulabschluss", "Kognitiver Test I (nonverbaler Test)", "Kognitiver Test II (verbaler Test)", "Constant"), 
          align = TRUE,
          no.space = TRUE)


# regression religious intensity 
reg8 = lm(dtnew$T012~ dtnew$reliintens + + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
            dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male + dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree  + dtnew$scaledsymtest + dtnew$scaledwordtest, na.action = na.exclude)
summary(reg8)

stargazer(reg8, 
          order = c(1,2,3,4,5,6,7,8,9,10,11,18,19,20,21,22,12,13,14,15,16,17,24),
          dep.var.labels = c("Switching row"),
          covariate.labels = c("Intensitaet Religion", "Alter", "Alter quadriert", "Male", "Einkommen (log)", "Kreditbeschraenkung",
                               "Gewissenhafigkeit", "Extraversion", "Neurotizismus", "Offenheit für Erfahrungen", "Vertraeglichkeit", "Gewissenhaftikeit*Male",
                               "Extraversion*Male", "Neurotizismus*Male", "Offenheit für Erfahrungen*Male", "Vertraeglichkeit*Male", "Abitur", "Realschulabschluss", "Besucht die Schule",
                               "Kein Schulabschluss", "Kognitiver Test I (nonverbaler Test)", "Kognitiver Test II (verbaler Test)", "Constant"), 
          align = TRUE,
          no.space = TRUE)


# Tobit model can be mixed with interval regression survival package

# Tobit model 
t1 <- vglm(dtnew$T012 ~ dtnew$religion, tobit(Lower = 1, Upper = 21), na.action = na.exclude)
summary(t1)


t2 <- vglm(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
             dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male + dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$scaledsymtest + dtnew$scaledwordtest, tobit(Lower = 1, Upper = 21), na.action = na.omit)
# why can´t I include dtnew$nodegree I think I just have 3 observations 
summary(t2)



# export Tobit model: can´t export the Tobit model with stargazer
# possible with memisic 
stargazer(t1, t2,
          align = TRUE,
          no.space = TRUE)


# intervall regression 
# I think this just applies if we use the returns more of a limitation so the next paper could be conducted with interval regression 



# we deal with count data glm model with poisson distribution 
glm1 <- glm(dtnew$T012 ~ dtnew$religion, family = quasipoisson, na.action = na.exclude)
summary(glm1)



glm2 = glm(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
             dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male + dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree  + dtnew$scaledsymtest + dtnew$scaledwordtest, family = poisson(link = "log") ,na.action = na.exclude )
summary(glm2)

# adjust the number of rows first 
anova(glm1, glm2)


