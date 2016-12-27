# Diagnostic of the linear regression 

# this scritp investigates whether the assumptions of the linear regression are met 
# the diagnostic is conducted for regression3b


# 1) create data frame for the residuals of the regression1
# 2)  global test
# 3) investigate the residuals through various plots/ normality assumption
# 4) investigate the model assumption linearity  
# 5) independence of errors 
# 6) investigate whether the variance of the regression is constant (homoscedasicity)
# 7) multicollinearity
# 8) influental observations 


# set working directory for my diagnostic plots 
#setwd(paste(wkdir, "/diagnostic/", sep = ""))

# create a data frame for the diagnostic with the residuals of reg1 
residuals1 <- residuals(reg3b)
diagnostic1 <- data.frame(residuals1)




########## Globaler Test of the model 
# shows which assumptions are violated 
gvmodel1 <- gvlma(reg3b)
summary(gvmodel1)

# export the results 


#######  normal distribution of error term 

# make a Q-Q plot for the residuals 
par(mfrow =c(1,1))
qqnorm(diagnostic1$residuals1)
qqline(diagnostic1$residuals1)
# does not look normally distributed 

# how would normal distributed residuals look like 
# create a vector of the regression residuals that is normally distributed 
resid.t1 <- rnorm(nobs(reg3b), mean = mean(diagnostic1$residuals1), sd = sd(diagnostic1$residuals1))

# plot the normally distributed residuals in a Q-Q plot 
qqnorm(resid.t1)
qqline(resid.t1)

# export Q-Q plot of regression residuals into diagnostic folder 
tikz(file = "reg3bresidualsnormal1.tex", width = 5, height = 5)
par(mfrow = c(1,1))
qqnorm(residuals(reg3b))
qqline(residuals(reg3b))
dev.off()

# export Q-Q plot of normally distributed residuals into diagnostic folder 
tikz(file =  "reg3bresidualsnormal2.tex", width = 5, height = 5)
qqnorm(resid.t1)
qqline(resid.t1)
dev.off()


# make an interaction Q-Q plot with a 95 confidence interval/car package 
# if you want to terminate the interaction plot -> ESC 
#par(mfrow =c(1,1))
qqPlot(reg1, id.method = "identify", simulate = TRUE, main = "QQ Plot")
# plot shows that we do not meet the normality assumption  


# histogram of residuals with normal function 
m <- mean(residuals(reg3b))
std <- sqrt(var(residuals(reg3b)))
par(mfrow = c(1,1))

tikz(paste(p.diagnostic, "reg1histresidualsnormal2.tex"), width = 5, height = 5)
     hist(residuals(reg3b), density=20, breaks=20, prob=TRUE, 
     xlab="x-variable", 
     main="normal curve over histogram")
curve(dnorm(x, mean = m, sd = std), 
      col="red", lwd=2, add=TRUE)
dev.off()


# histogram with normal distribution / ggplot 
# export this into the diagnostic folder 
tikz(file = "residhistreg3b.tex", width = 5, height = 5)
ggplot(diagnostic1, aes(residuals1))+
  geom_histogram(aes(y=..density..), bins = 35)+
  #ggtitle("Histogram of regression residuals with normal distribution")+
  xlab("Regressionsresiduen")+
  stat_function(fun = dnorm, colour = "red", args = list(mean = mean(diagnostic1$residuals1), sd = sd(diagnostic1$residuals1)), lwd = 0.9)
dev.off()

# interactive qqplot with confidence interval 
qqPlot(reg3b, id.method = "identify", simulate = TRUE, main = "QQ Plot", envelope = 0.90)
# don´t meet the normality assumption since many residuals lay outside of the 90% confidence interval 


# Shapiro test: tests if the residuals are normally distributed 
shapiro.test(diagnostic1$residuals1)
# Null hypotheses is that the residuals are normal --> we reject the Null hypothesis


########### Linearity 
plot(reg3b, which=1)
# does not violate the normality assumption 
plot(reg3b, which=3)
# does not violate the normality assumption 

# shows if we meet the linearity assumption 
tikz(file = "crPlots.tex", width = 5, height = 5)
crPlots(reg3c)
dev.off()
# all plots that we do not a violation of the normality assumption

######## independence of errors 

durbinWatsonTest(reg3b)

############ Heteroskedasitiztät (do we have non-constant variance)


# export plot to investigate the distribution of the residuals into the diagnostic folder 
tikz(file = "heteroscedasticity.tex", width = 5, height = 5)
plot(reg3b, which=3,)
dev.off()

#plot(fitted(reg3b),sqrt(abs(residuals(reg3b))), xlab="Fitted",ylab=
       #expression(sqrt(hat(epsilon))))

# conduct a Breush-Pagan test to detect whether we have heteroscedasticity or not 
bptest(reg3b)
# fail to reject the null hypothesis 



#spread level plot  for homoskedasticity / car package 
tikz(file = "Spreadlevel.tex", width = 5, height = 5)
spreadLevelPlot(reg3b, main = "Spread-Level Plot")
dev.off()
# try to ransform the data with ^ 1.787609 and than check it again
# did not do that but within an extensive diagnostic analysis I could do that 



# multicollinearity/ car package 
vif(reg3b)
sqrt(vif(reg3b)) >2



############## this part is not part of the analysis as it deals with the detection of influental observations for the regression analysis

# observations that are not predicted well by the model -> outliers 
out1 <- outlierTest(reg6)
out1


# new regression withough 123 
#r6b = lm(r6a, subset = )
#summary(r6b)

plot(reg6, which=2)


# observations that change the fit of the model in substantive way -> influential observations 
plot(reg6, which=4)


##### leverage observations 
hat.plot <- function(reg3b) {
  p <- length(coefficients(reg3b))
  n <- length(fitted(reg3b))
  plot(hatvalues(reg3b), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(reg3b), names(hatvalues(reg3b)))
}
hat.plot(reg3b)

# 130 & 185 leverage observations 

#leveragePlots(reg3b)


# influental observations 
tikz(file = "CooksD.tex", width = 5, height = 5)
cutoff <- 1
plot(reg3b, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
dev.off()


# added variable plots 
avPlots(reg3b, ask=FALSE, onepage=TRUE, id.method="identify")


#influencePlot(reg6, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook’s distance")

