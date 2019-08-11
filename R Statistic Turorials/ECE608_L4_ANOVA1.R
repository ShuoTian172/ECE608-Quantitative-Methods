## ECE608 Lecture 4: One-way ANOVAs
# May 29, 2019

library(tidyverse)
library(car)
library(ez)
library(e1071)

#Dart Throwing proof
dt <- data.frame(player = factor(seq(1,20,by=1)),
                 exp = factor(c(rep(1,7),rep(2,7),rep(3,6))),
                 dist = c(160,168,150,166,158,147,156,
                          170,176,160,156,160,162,152,
                          150,166,170,154,160,162))

dt.ez <- ezANOVA(data = dt,
                 dv = dist,
                 wid = player,
                 between = exp,
                 type = 3,
                 return_aov = TRUE)
dt.ez

ggplot(data = dt, aes(x = exp, y = dist))+
  geom_boxplot()

##In-depth ANOVA assumption testing
# Creating a dataset for between-groups ANOVA
g1 <- rnorm(n = 100, mean = 100, sd = 30)
g2 <- rnorm(n = 100, mean = 105, sd = 35)
g3 <- rnorm(n = 100, mean = 150, sd = 35)
df1 <- data.frame(g1,g2,g3) %>% 
  gather(key = "group", value = "measure") %>% 
  mutate(id = factor(seq(1,300,by = 1))) %>% 
  mutate(group = factor(group))

ggplot(dat=df1, aes(x=measure,fill=group)) +
  geom_histogram(binwidth=10,alpha=0.5,position="identity")

#Run the ANOVA first
df1.ez <- ezANOVA(data = df1,
                 dv = measure,
                 wid = id,
                 between = group,
                 type = 3,
                 return_aov = TRUE)
df1.ez


#Check normality of residuals
qqnorm(df1.ez[["aov"]][["residuals"]])
skewness(df1.ez[["aov"]][["residuals"]])
kurtosis(df1.ez[["aov"]][["residuals"]])
ks.test(x = df1.ez[["aov"]][["residuals"]],
        y = "pnorm",
        mean = mean(df1.ez[["aov"]][["residuals"]]),
        sd = sd(df1.ez[["aov"]][["residuals"]]))

#Check HoV of residuals: ezANOVA already did it for us!

#If HoV bad, can use Welch'es one-way test
oneway.test(measure ~ group, data = df1)



## Post-hoc analysis
#We could use manual T-tests:
t.test(data = df1,
       x = df1$measure[df1$group == "g1"],
       y = df1$measure[df1$group == "g2"],
       alternative = "two.sided",
       var.equal = TRUE,
       paired = FALSE)

#NOTE: This is the same as filtering the data first:
df.test <- df1 %>% 
  filter(group == "g1" | group == "g2")

t.test(formula = measure ~ group,
       data = df.test,
       alternative = "two.sided",
       var.equal = TRUE,
       paired = FALSE)

#Run the other comparisons:
t.test(data = df1,
       x = df1$measure[df1$group == "g1"],
       y = df1$measure[df1$group == "g3"],
       alternative = "two.sided",
       var.equal = TRUE,
       paired = FALSE)
t.test(data = df1,
       x = df1$measure[df1$group == "g2"],
       y = df1$measure[df1$group == "g3"],
       alternative = "two.sided",
       var.equal = TRUE,
       paired = FALSE)

#However, a much easier way is to use another function:
pairwise.t.test(x = df1$measure,
                g = df1$group,
                paired = FALSE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni") #or "holm"

#Return Means and SD for each group
tapply(df1$measure, list(df1$group), mean)
tapply(df1$measure, list(df1$group), sd)
tapply(df1$measure, list(df1$group), length)

#Can also use Tukey's HSD
TukeyHSD(df1.ez[["aov"]])

##Plot the final results
sig.df <- data.frame(group = "g3", measure = 275)
ggplot(data = df1, aes(x = group, y = measure))+
  geom_boxplot(aes(fill = group))+
  geom_text(data = sig.df, label = "*", size = 10)+
  scale_y_continuous(limits = c(-1,300))+
  xlab("Group")+
  ylab("Measure (AU)")+
  labs(caption = "*P<0.05 different from g1 and g2")

##Repeated-Measures ANOVA
df1.rm <- df1 %>%
  mutate(id = factor(rep(seq(1,100,by=1),3)))

df1.ez.rm <- ezANOVA(data = df1.rm,
                  dv = measure,
                  wid = id,
                  within = group,
                  type = 3,
                  return_aov = TRUE)
df1.ez.rm

#Repeated-Measures post-hoc:
pairwise.t.test(x = df1.rm$measure,
                g = df1.rm$group,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni") #or "holm"
