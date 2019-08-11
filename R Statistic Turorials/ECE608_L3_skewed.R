#Lecture 3.5 lingering t-test tutorial

## Transforming Data

#Bring in packages
library(tidyverse)
library(car)
library(e1071)

#Create a skewed dataset with unequal sample sizes
g1 <- rbeta(n = 100, shape1 = 1, shape2 = 10)
g2 <- rbeta(n = 90, shape1 = 10, shape2 = 10)
dfg1 <- data.frame(group = rep("g1",100),
                   value=g1,
                   ID=seq(1,100,by=1))
dfg2 <- data.frame(group = rep("g2",90),
                   value=g2,
                   ID=seq(101,190,by=1))
df1 <- rbind(dfg1,dfg2)

ggplot(dat=df1, aes(x=value, fill=group))+
  geom_histogram(binwidth=0.1, alpha=0.5, position="identity")

#G1 was created skewed; we can double-check during normality testing
qqnorm(g1)
print(skewness(g1))
shapiro.test(x = g1)
ks.test(x = g1,
        y = "pnorm",
        mean = mean(g1),
        sd = sd(g1))

qqnorm(g2)
print(skewness(g2))
shapiro.test(x = g2)
ks.test(x = g2,
        y = "pnorm",
        mean = mean(g2),
        sd = sd(g2))

#We can fix the outcome with a sqrt transform and run normality again
df1 <- df1 %>% 
  mutate(value_sqrt = sqrt(value))
qq.g1 <- df1 %>% 
  filter(group == "g1")
qq.g2 <- df1 %>%
  filter(group == "g2")

qqnorm(qq.g1$value_sqrt)
print(skewness(qq.g1$value_sqrt))
shapiro.test(x = qq.g1$value_sqrt)
ks.test(x = qq.g1$value_sqrt,
        y = "pnorm",
        mean = mean(qq.g1$value_sqrt),
        sd = sd(qq.g1$value_sqrt))

qqnorm(qq.g2$value_sqrt)
print(skewness(qq.g2$value_sqrt))
shapiro.test(x = qq.g2$value_sqrt)
ks.test(x = qq.g2$value_sqrt,
        y = "pnorm",
        mean = mean(qq.g2$value_sqrt),
        sd = sd(qq.g2$value_sqrt))


#Let's check Levene's Test
leveneTest(value~group, data = df1, center = mean)
ggplot(dat = df1, aes(x = group, y = value))+
  geom_violin()+
  geom_point()

leveneTest(value_sqrt~group, data = df1, center = mean)
ggplot(dat = df1, aes(x = group, y = value_sqrt))+
  geom_violin()+
  geom_point()

## Dependent t-test assumptions
t1 <- rnorm(n=100, mean=100, sd=20)
t2 <- rnorm(n=100, mean=150, sd=20)

#Create dataframe with repeated measurements (time1 and time2 for the same individual)
df2 <- data.frame(t1,t2) %>%
  mutate(ID = factor(seq(1,100,by=1))) %>%   #Adds a unique identifier
  mutate(diff = t1 - t2)                     #Takes the difference between measurements

ggplot(dat = df2, aes(x = diff))+
  geom_histogram(binwidth = 10, position = "identity")

qqnorm(df2$diff)
ks.test(x = df2$diff,
        y = "pnorm",
        mean = mean(df2$diff),
        sd = sd(df2$diff))

#T-test for 'wide-format' data
t.test(x = df2$t1,
       y = df2$t2,
       alternative = "two.sided",
       var.equal = TRUE,
       paired = TRUE)

#Adjust data into long format and re-run T-test
df2.long <- df2 %>%
  select(-diff) %>% 
  gather(key = time, value = value, 1:2)

t.test(formula = value ~ time,
       data = df2.long,
       alternative = "two.sided",
       var.equal = TRUE,
       paired = TRUE)
