## Power Lecture
# June 18, 2019

library(tidyverse) #For data wrangling
library(ez)        #For ANOVA
library(pwr)       #For power calculation
library(pwr2)      #For 2-way ANOVA power calculation
library(effsize)   #For effect size calculation

###Calculating power from study results
##T-test example: Two groups with example sample sizes
#Create the data: Two levels of experience, measuring distance from center of dart board
df <- data.frame(player = factor(seq(1,60,by=1)),
                 exp = factor(c(rep(1,30),rep(2,30))),
                 dist = c(rnorm(n = 30, mean = 30, sd = 10),
                          rnorm(n = 30, mean = 40, sd = 11)))

#Run the T-test
t.test(formula = dist ~ exp,
       data = df,
       alternative = "two.sided",
       var.equal = TRUE,
       paired = FALSE)

#Calcualte the effect size
df.d <- cohen.d(df$dist ~ df$exp)
d <- abs(df.d[["estimate"]])

#Investigate the power
pwr.t.test(d = d,
           n = 30,
           sig.level = 0.05,
           power = NULL,
           type = "two.sample")

##ANOVA Example
df2 <- data.frame(player = factor(seq(1,90,by=1)),
                  exp = factor(c(rep(1,30),rep(2,30),rep(3,30))),
                  dist = c(rnorm(n = 30, mean = 30, sd = 10),
                           rnorm(n = 30, mean = 32, sd = 11),
                           rnorm(n = 30, mean = 40, sd = 9)))

df2.ez <- ezANOVA(data = df2,
                  dv = dist,
                  wid = player,
                  between = exp,
                  type = 3,
                  return_aov = TRUE)
df2.ez

f <- sqrt((df2.ez[["ANOVA"]][["ges"]])/(1 - df2.ez[["ANOVA"]][["ges"]]))

pwr.anova.test(k = 3,
               n = 30,
               f = f,
               power = NULL,
               sig.level = 0.05)

ggplot(data = dt, aes(x = exp, y = dist))+
  geom_boxplot()



### Sample size calculations
## Example 1: John wants to design a study to investigate the differene between men and women for blood pressure.
## He anticipates an effect size of d = 0.2, with an alpha = 0.05, beta = 0.2,

pwr.t.test(d = 0.2,
           n = NULL,
           power = 0.8,
           type = "two.sample",
           sig.level = 0.05)

## Example 2: Mary wants to design a study to find main effects for education (high school, college, university) on income.
## She anticipates an effect size of ges = 0.102, with an alpha = 0.01, and power of 90%

f <- sqrt(0.102/(1-0.102))
pwr.anova.test(k = 3,
               n = NULL,
               f = f,
               power = 0.9,
               sig.level = 0.01)

## Example 3: Joy wants to design a study to find main effects for exercise type (3 types) over 2 timepoints
## She anticipates a low effect of exercise, and a medium effect for time.

ss.2way(a = 3,
        b = 2,
        alpha = 0.05,
        beta = 0.20,
        f.A = 0.14,
        f.B = 0.29,
        B=100)


#Create power plots for varying sample sizes
#Easiest function
pwr.plot(n = seq(1,30,by=1),
         k = 3,
         f = 0.4,
         alpha = 0.05)

#Manual plotting for multiple effect size levels
n <- seq(5,50,by=1) #Sample size vector
lenn <- length(n)
d <- c(0.3,0.5,0.8) #Effect size vector
lend <- length(d)

pwrtable <- array(numeric(lenn*lend), dim = c(lenn,lend)) #Loop power calculations
for (i in 1:lend){
  for (j in 1:lenn){
    result <- pwr.t.test(d = d[i],
                         n = n[j],
                         sig.level = 0.05,
                         type = "two.sample",
                         alternative = "two.sided")
    pwrtable[j,i] <- result$power
  }
}

#Wrangle and plot power plots using ggplot
pwrtable <- data.frame(pwrtable, n)
names(pwrtable) <- c("plow","pmid","phigh","samplesize")
pwrtable <- gather(data = pwrtable, key = pwrlevel, value = power,1:3)
pwrtable$pwrlevel <- factor(pwrtable$pwrlevel, levels = c("plow","pmid","phigh"))
ggplot(data = pwrtable, aes(x = samplesize, y = power))+
  geom_line(aes(group = pwrlevel, color = pwrlevel))+
  geom_hline(yintercept = 0.80)
