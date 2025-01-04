rm(list = ls())
options("install.lock"=FALSE)

library(quantreg)
library(fastDummies)
library(MASS)

# storing data
mydata <-read.csv("C:/Users/USER/Downloads/Non-Parametric/Project/insurance.csv")

attach(mydata)
head(mydata)
m<-data.frame(mydata)
summary(mydata)
summary(m)
dim(m)

# separating independent and dependent data
Y <- mydata[ ,7]
X <- mydata[ ,-7]

# descriptive statistics
summary(Y)
summary(X)

# one-hot encoding
data <- dummy_cols(mydata, remove_first_dummy = TRUE)
head(data)

# plot response variable
hist(charges,prob=TRUE,main='Histogram of Insurance Charges')
d <- seq(min(charges),max(charges))
f <- dnorm(d,mean=mean(Y),sd=sd(Y))
lines(d, f, col='red')
lines(density(Y), col='blue',lwd=2)
legend(x='topright',legend=c("Normal Density", "Estimated Density Over Charges"),col=c("red", "blue"), lty=1:2, cex=0.8)
boxplot(Y)
q<-unname(quantile(Y, seq(0,1, by=0.1)))
plot(x=seq(0,1, by=0.1), y= q, type='o')

# shapiro-wilk test for assurance
shapiro.test(Y)

# OLS regression over original response variable
olsreg_origal <- lm(charges ~ ., data=m)
summary(olsreg_origal)

# check assumptions of linear regression
par(mfrow=c(2,2))
plot(olsreg_origal)

par(mfrow=c(1,1))
plot(olsreg_origal,1) # linearity of the data
plot(olsreg_origal,2) # normality of residuals
plot(olsreg_origal,3) # homogeneity of variance
plot(olsreg_origal,4) # cook's distance
plot(olsreg_origal,5) # residuals vs leverage

res<-rstandard(olsreg_origal)
shapiro.test(res)

c<-cooks.distance(olsreg_origal)
plot(c, pch=16, col='darkblue', col.main='darkred', xlab='Index',ylab='Cooks Distance')
max(c)
qf(0.95,1338,1330,lower.tail=FALSE) #at 0.05 level of significance value of F(alpha; n, n-p-1)

#box-cox 
boxcox(lm(Y~1)) # best lambda should be 0, i.e. best transformation should be log(Y)


# plot log transformed response variable
hist(log(Y),prob=TRUE,main='Histogram of log Transformed Insurance Charges')
d_log <- seq(min(log(Y)),max(log(Y)))
f_log <- dnorm(d,mean=mean(log(Y)),sd=sd(log(Y)))
lines(d_log, f_log, col='red')
lines(density(log(Y)), col='blue',lwd=2)

# shapiro-wilk test for assurance
shapiro.test(log(Y))

# OLS regression over log(original) response variable
olsreg_log <- lm(log(charges) ~ ., data=m)
summary(olsreg_log)

# check assumptions of linear regression
par(mfrow=c(2,2))
plot(olsreg_log)

par(mfrow=c(1,1))
plot(olsreg_log,1) # linearity of the data
plot(olsreg_log,2) # normality of residuals
plot(olsreg_log,3) # homogeneity of variance
plot(olsreg_log,4) # cook's distance
plot(olsreg_log,5) # residuals vs leverage

res<-rstandard(olsreg_log)
shapiro.test(res)


# plot sqrt transformed response variable
hist(sqrt(Y),prob=TRUE,main='Histogram of log Transformed Insurance Charges')
d_sqrt <- seq(min(sqrt(Y)),max(sqrt(Y)))
f_sqrt <- dnorm(d,mean=mean(sqrt(Y)),sd=sd(sqrt(Y)))
lines(d_sqrt, f_sqrt, col='red')
lines(density(sqrt(Y)), col='blue',lwd=2)

# shapiro-wilk test for assurance
shapiro.test(sqrt(Y))

# OLS regression over sqrt(original) response variable
olsreg_sqrt <- lm(sqrt(charges) ~ ., data=m)
summary(olsreg_sqrt)

# check assumptions of linear regression
par(mfrow=c(2,2))
plot(olsreg_sqrt)

par(mfrow=c(1,1))
plot(olsreg_sqrt,1) # linearity of the data
plot(olsreg_sqrt,2) # normality of residuals
plot(olsreg_sqrt,3) # homogeneity of variance
plot(olsreg_sqrt,4) # cook's distance
plot(olsreg_sqrt,5) # residuals vs leverage

res<-rstandard(olsreg_sqrt)
shapiro.test(res)


# plot quantile regression
quantreg.all <- rq(charges ~ ., tau=seq(0.05, 0.95, by = 0.05), data=mydata)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)

res_qr <-resid(quantreg.all)
coeff_qr<-coef(quantreg.all)
res_qr
plot(res_qr)
rstandard(quantreg.all)


## tests of coefficients significantly different from quantile to quantile (jointly and for each variable separately)
quantreg_1<-rq(charges ~ ., tau=0.01, data=mydata)
quantreg_5<-rq(charges ~ ., tau=0.05, data=mydata)
quantreg_25<-rq(charges ~ ., tau=0.25, data=mydata)
quantreg_50<-rq(charges ~ ., tau=0.5, data=mydata)
quantreg_75<-rq(charges ~ ., tau=0.75, data=mydata)
quantreg_95<-rq(charges ~ ., tau=0.95, data=mydata)
quantreg_99<-rq(charges ~ ., tau=0.99, data=mydata)

#jointly for all variable (used dafault 'wald' test method)
anova(quantreg_1, quantreg_5)
anova(quantreg_5, quantreg_25)
anova(quantreg_25, quantreg_50)
anova(quantreg_50, quantreg_75)
anova(quantreg_75, quantreg_95)
anova(quantreg_95, quantreg_99)


# for all variable (used dafault 'wald' test method)
anova(quantreg_1, quantreg_5)
anova(quantreg_5, quantreg_25)
anova(quantreg_25, quantreg_50)
anova(quantreg_50, quantreg_75)
anova(quantreg_75, quantreg_95)
anova(quantreg_95, quantreg_99)

#separately for each variable (used dafault 'wald' test method)
anova(quantreg_1, quantreg_5, joint=FALSE)
anova(quantreg_5, quantreg_25, joint=FALSE)
anova(quantreg_25, quantreg_50, joint=FALSE)
anova(quantreg_50, quantreg_75, joint=FALSE)
anova(quantreg_75, quantreg_95, joint=FALSE)
anova(quantreg_95, quantreg_99, joint=FALSE)
