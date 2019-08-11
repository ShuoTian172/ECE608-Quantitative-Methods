## Regression Lecture
# UPDATED July 3, 2019

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

#Plot scatterplot and linear fit to represent correlation
ggplot(data = df, aes(x = wt, y = mpg))+
  geom_point()+
  geom_smooth(method=lm, se = FALSE)


## Simple Regression

#Linear model
mod1 <- lm(mpg ~ wt, data = df)  #Generate model
mod1

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
mod2 <- lm(mpg ~ disp + hp + wt + qsec, data = df)
summary(mod2)

#Statistical Multiple Regression (Stepwise)

#https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
k <- ols_step_all_possible(mod2) #Theoretically all models
plot(k)

forward <- ols_step_forward_p(mod2, penter = 0.3, details = TRUE) #penter default is 0.3
plot(forward)

backward <- ols_step_backward_p(mod2, prem = 0.5, details = TRUE) #prem default is 0.3
plot(backward)

both <- ols_step_both_p(mod2, pent = 0.3, prem = 0.5, details = TRUE) #pent 0.3, prem 0.3
plot(both)

#Hierarchical Multiple Regression (Blockwise)
hmod0 <- lm(mpg ~ 1, data = df)
hmod1 <- lm(mpg ~ disp, data = df)
hmod2 <- lm(mpg ~ disp + hp, data = df)
hmod3 <- lm(mpg ~ disp + hp + wt, data = df)
hmod4 <- lm(mpg ~ disp + hp + wt + qsec, data = df)

stats::anova(hmod0,hmod1,hmod2,hmod3,hmod4)

summary(hmod3)




## Assumption Checking for Regression (PART 2)
df <- mtcars
mod2 <- lm(mpg ~ disp + hp + wt + qsec, data = df)
summary(mod2)

#Assumption 1: Are the dependent variables continuous? Yes
str(df)

#Assumption 2: Are dependent y values independent from each other? Yes
#Logic

#Assumption 3: Non-zero variance of predictors Yes
options(scipen = 9999)
apply(df, 2, var)

#Assumption 4: The regression model is linear in predictors Yes
Hmisc::rcorr(as.matrix(df), type = "pearson")

#Assumption 5: No perfect multicollinearity, above 10 is bad
car::vif(mod2) #Only works for multiple variables

#Assumption 6: Highly influential points (Cook's distance)
#Assumption 7: Homoscedasticity
#Assumption 8: Normality of residuals
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
df.res <- data.frame(df, mod2$residuals) %>% 
  dplyr::select(-c(cyl,drat,vs,am,gear,carb))
Hmisc::rcorr(as.matrix(df.res), type = "pearson")

#Assumption 12: The number of observations must be greater than the number of Xs
#Logic


## A sample of Logistic Regression
#Set up the data.frame to split mpg into a binomial response
df.log <- df %>% 
  mutate(fuel = factor(case_when(
    mpg < 18 ~ "Poor",
    mpg > 18 ~ "Good"
  )))
  
#Create the logigistic model
mod.log <- glm(fuel ~ disp + hp + wt, family = binomial, data = df.log)
summary(mod.log)

#Look at the individual predictions based on the model
glm.probs <- predict(mod.log,type = "response")
glm.probs[1:5]

#Create a frequency table to see Prediction counts
glm.pred <- ifelse(glm.probs > 0.5, "Poor", "Good")
table(glm.pred,df.log$fuel)

## A sample of Poison Regression
#Set up new dataframe from the warpbreaks dataset
library(datasets)
df.poi <- warpbreaks

#Create the Poisson model
mod.poi <- glm(breaks ~ wool + tension, data = df.poi, family = poisson)
summary(mod.poi)

#Predict data from model
new.df.poi <- data.frame(wool = "B",
                         tension = "M")

predict(mod.poi, newdata = new.df.poi, type = "response")
