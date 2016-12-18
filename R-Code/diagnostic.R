# Diagnostics 

# create a data frame for the diagonostic with residuals 
residuals <- residuals(reg6)
diagnostic <- data.frame(residuals)

# normal distribution error term 

# Q-Q plot 
qqnorm(residuals(reg6), ylab = "Residuals", main = "")
qqline(residuals(reg6))
# more a short tailed distribution 

# how would normal distributed residuals look like 

resid.t <- rnorm(residuals(reg6), 500, sd(residuals(reg6)))

qqnorm(resid.t)
qqline(resid.t)

# compare the two 
par(mfrow = c(2,1))
qqnorm(residuals(reg6), ylab = "Residuals", main = "")
qqline(residuals(reg6))

qqnorm(resid.t)
qqline(resid.t)

# we do not fulfill the normality assumption 

par(mfrow =c(1,1))
qqPlot(r6a, id.method = "identify", simulate = TRUE, main = "QQ Plot")
# don´t meet the normality assumption 


# histogram of residuals with normal function 
m <- mean(residuals(reg6))
std <- sqrt(var(residuals(reg6)))
hist(residuals(reg6), density=20, breaks=20, prob=TRUE, 
     xlab="x-variable", 
     main="normal curve over histogram")
curve(dnorm(x, mean = m, sd = std), 
      col="darkblue", lwd=2, add=TRUE)


# histogram with ggplot 
# does not function 
ggplot(diagnostic, aes(residuals))+
  geom_histogram()+
  stat_function(fun = dnorm, colour = "red", args = list(mean = 0, sd = 2.5))
  
# Shapiro test: tests if the residuals are normally distributed 
shapiro.test(residuals(reg6))
# Null hypotheses is that the residuals are normal --> we reject the Null hypothesis


# independence of errors 
durbinWatsonTest(reg6)
dwtest(reg6)
# I have autocorrelation in my dataset/ what should I do than? 

# Linearity 
plot(reg6, which=1)
# -> does not violate the normality assumption 
plot(reg6, which=3)
# -> does violate the normality assumption 

crPlots(reg6)
ceresPlots(reg6)
# how can I take interation effects into account 

# Heteroskedasitiztät 
par(las=1)
plot(fitted(r6a), residuals(reg6), xlab = "Fitted", ylab="Residuals")
abline(h=0, col="red")

plot(reg6, which=1)
plot(reg6, ask=TRUE)

bptest(reg6)
# fail to reject the null hypothesis 

plot(fitted(r6a),residuals(reg6),xlab="Fitted",ylab="Residuals")
abline(h=0)

plot(fitted(reg6),sqrt(abs(residuals(reg6))), xlab="Fitted",ylab=
       expression(sqrt(hat(epsilon))))
plot(reg6, which=3)

spreadLevelPlot(reg6)


# Globaler Test von Modellannahmen
gvmodel <- gvlma(reg6)
summary(gvmodel)


# multicollinearity 
vif(reg6)
sqrt(vif(reg6)) >2


# Finding unusual observations 
# observations that are not predicted well by the model -> outliers 
out1 <- outlierTest(reg6)
out1


# new regression withough 123 
#r6b = lm(r6a, subset = )
#summary(r6b)

plot(reg6, which=2)


# observations that change the fit of the model in substantive way -> influential observations 
plot(reg6, which=4)



# both characteristics -> leverage 

hat.plot <- function(reg6) {
  p <- length(coefficients(reg6))
  n <- length(fitted(reg6))
  plot(hatvalues(reg6), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(reg6), names(hatvalues(reg6)))
}
hat.plot(reg6)

leveragePlots(reg6)
# influental observations 
cutoff <- 4/325
plot(reg6, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

avPlots(reg6, ask=FALSE, onepage=TRUE, id.method="identify")



influencePlot(reg6, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook’s distance")



# transformation of the model 
summary(powerTransform(dtnew$T012))

#experimental disign and data analysis for biologists 

#model the censored model by my own and than compare the distributions 
boxTidwell(dtnew$T012 ~ dtnew$religion + dtnew$age + I(dtnew$age^2) + dtnew$male + dtnew$logincom + dtnew$constraint + dtnew$scaledbig_g + dtnew$scaledbig_e + dtnew$scaledbig_n + dtnew$scaledbig_o + dtnew$scaledbig_v + dtnew$scaledbig_g*dtnew$male + dtnew$scaledbig_e*dtnew$male +
             dtnew$scaledbig_n*dtnew$male + dtnew$scaledbig_o*dtnew$male + dtnew$scaledbig_v*dtnew$male + dtnew$abi + dtnew$real + dtnew$enrolled + dtnew$nodegree  + dtnew$scaledsymtest + dtnew$scaledwordtest, na.action = na.exclude)

