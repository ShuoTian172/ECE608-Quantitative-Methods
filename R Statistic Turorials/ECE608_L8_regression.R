## Regression Lecture
# June 26, 2019

# Lots of packages
library(tidyverse)  #Data management
library(QuantPsyc)  #Regression standardized betas
library(ggpubr)     #Add regression information to graphs
# Packages for Assumption Testing
library(Hmisc)      #rcorr function
library(lawstat)    #runs.test for regression
library(lmtest)     #Durbin-Watson test
library(car)        #Variance Inflation Factor
library(MASS)       #stepAIC function for stepwise regression (but no details)
library(olsrr)      #Stepwise regression functions

## Covariance and Correlation
#Bring in data
df <- mtcars
str(df)

#Calculate covariance
cov(df$wt, df$mpg, method = "pearson")

#Calculate correlations
cor.test(x = df$wt,
         y = df$mpg,
         alternative = "two.sided",
         method = "pearson")

# p value is large meaning accept r = 0 relationship

#Plot scatterplot and linear fit to represent correlation
ggplot(data = df, aes(x = wt, y = mpg))+
  geom_point()+
  geom_smooth(method=lm, se = FALSE)


## Simple Regression

#Linear model
mod1 <- lm(mpg ~ wt, data = df)  #Generate model
mod1

# R-squared (Anova model) and F-statistic are two way to explain the model, 
# give you two different info.


#Model summaries
summary(mod1)                    #View model summary
QuantPsyc::lm.beta(mod1)         #View standardized betas

ggplot(data = df, aes(x = wt, y = mpg))+   #Identify data
  geom_point()+                            #Create a scatterplot
  geom_smooth(method=lm, se = FALSE)+      #Add a line fo best fit
  scale_x_continuous(limits = c(0, 6))+    #Set x scale from 0 to 6
  stat_cor(label.y = 15)+                  #List R and p value at height y=15
  stat_regline_equation(label.y = 12)      #List equation of line at height y=12
  

## Multiple Regression
mod2 <- lm(mpg ~ disp + hp + drat + wt + qsec, data = df)
summary(mod2)
QuantPsyc::lm.beta(mod2)

## Model selection for Multiple Regression
#Standard Multiple Regression (Forced Entry)
mod2 <- lm(mpg ~ disp + hp + wt + qsec, data = df2)
summary(mod2)

#Statistical Multiple Regression (Stepwise)

#https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
k <- ols_step_all_possible(mod2) #Theoretically all models
plot(k)


# penter is used to see the varialbe added change the model whith p value lower than 0.3
forward <- ols_step_forward_p(mod2, penter = 0.3, details = TRUE) #penter default is 0.3
plot(forward)

backward <- ols_step_backward_p(mod2, prem = 0.5, details = TRUE) #prem default is 0.3
plot(backward)

both <- ols_step_both_p(mod2, pent = 0.3, prem = 0.5, details = TRUE) #pent 0.3, prem 0.3
plot(both)

#Hierarchical Multiple Regression (Blockwise)
hmod0 <- lm(mpg ~ 1, data = df) # 1 is placeholder 
hmod1 <- lm(mpg ~ disp, data = df)
hmod2 <- lm(mpg ~ disp + hp, data = df)
hmod3 <- lm(mpg ~ disp + hp + wt, data = df)
hmod4 <- lm(mpg ~ disp + hp + wt + qsec, data = df)

stats::anova(hmod0,hmod1,hmod2,hmod3,hmod4)

summary(hmod3)




## Assumption Checking for Regression (PART 2)
#Assumption 1: Are the variables continuous?
str(df)

#Assumption 2: Are y values independent from each other?
#Logic

#Assumption 3: Non-zero variance of predictors
options(scipen = 9999)
apply(df, 2, var)

#Assumption 4: The regression model is linear in predictors
Hmisc::rcorr(as.matrix(df), type = "pearson")

#Assumption 5: No perfect multicollinearity
car::vif(mod2) #Only works for multiple variables

#Assumption 6: Homoscedasticity
#Assumption 7: Normality of residuals
#Assumption 8: Cook's distance
par(mfrow = c(2,2))  #Set plotting window to a 2x2 orientation
plot(mod2)           #Plot all regression plots
par(mfrow = c(1,1))  #Set plotting window back to single
# Plot1: Homoescedasticity. Are the residuals equal at every level?
# Plot2: Normality of residuals
# Plot3: Standardized homoscedasticity
# Plot4: Cook's distance

#Assumption 9: Independence of residuals
stats::acf(mod2$residuals)         #Plot for lag function: Is there a pattern in the lag? Is it predictable?
lawstat::runs.test(mod2$residuals) #Runs test: Do the residuals differ from a straight line?
lmtest::dwtest(mod2)               #Durbin-Watson Test: Is there first order autocorrelation? 1.5-2.5 = normal.

#Assumption 10: The mean of residuals is zero
mean(mod2$residuals)

#Assumption 11: X variables and residuals are uncorrelated
Hmisc::rcorr(df$mpg, mod2$residuals)
stats::cor.test(df$mpg, mod2$residuals)

#Assumption 12: The number of observations must be greater than the number of Xs
#Logic

