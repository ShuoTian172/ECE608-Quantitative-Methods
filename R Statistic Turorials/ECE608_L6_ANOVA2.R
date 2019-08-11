## ECE608 Lecture 6: Factorial ANOVAs
# June 12, 2019
# Three types of Factorial ANOVAs:
# 1. All between factors
# 2. All within factors (repeated measures)
# 3. Mixed between-within factors

library(tidyverse)
library(ez)
library(e1071)
library(car)
library(emmeans)

#Read and clean data
df <- read.csv("C:/Users/kinja/Dropbox/Documents/R Working file/ECE608/PopFlow.csv")
names(df)[1] <- "D10" #Need this since something is wrong with the csv file (UNICODE error)
df1 <- df %>% 
  gather(key = "condition", value = "flow") %>% 
  mutate(ankle = case_when(
    grepl(pattern = "D", x = condition) == 1 ~ "dorsiflexion",
    grepl(pattern = "N", x = condition) == 1 ~ "neutral",
    grepl(pattern = "P", x = condition) == 1 ~ "plantarflexion"
    )) %>% 
  mutate(force = case_when(
    grepl(pattern = "10", x = condition) == 1 ~ "N10",
    grepl(pattern = "20", x = condition) == 1 ~ "N20",
    grepl(pattern = "30", x = condition) == 1 ~ "N30"
  )) %>% 
  select(-condition)

## 1. Factorial ANOVA: All between factors
#Create IDs for an all between example
df1b <- df1 %>% 
  mutate(ID = factor(seq(1,180,by=1)))

df1b.ez <- ezANOVA(data = df1b,
                   dv = flow,
                   wid = ID,
                   between  = c(ankle,force),
                   type = 3,
                   return_aov = TRUE)
df1b.ez

#Check Assumptions (new qqPlot function)
qqPlot(df1b.ez[["aov"]][["residuals"]])
skewness(df1b.ez[["aov"]][["residuals"]])
kurtosis(df1b.ez[["aov"]][["residuals"]])
shapiro.test(x = df1b.ez[["aov"]][["residuals"]])

TukeyHSD(x = df1b.ez[["aov"]])

## 2. Factorial ANOVA: All within factors
#Create IDs for an all within example
df1w <- df1 %>% 
  mutate(ID = factor(rep(seq(1,20,by=1),9)))

#ANOVA
df1w.ez <- ezANOVA(data = df1w,
                   dv = flow,
                   wid = ID,
                   within = c(ankle,force),
                   type = 3,
                   return_aov = TRUE)
df1w.ez

#Check Assumptions (new qqPlot function)
qqPlot(df1w.ez[["aov"]][["ID:ankle:force"]][["residuals"]])
skewness(df1w.ez[["aov"]][["ID:ankle:force"]][["residuals"]])
kurtosis(df1w.ez[["aov"]][["ID:ankle:force"]][["residuals"]])
shapiro.test(x = df1w.ez[["aov"]][["ID:ankle:force"]][["residuals"]])

#Posthocs
ggplot(data = df1w, aes(x = force, y = flow, fill = ankle))+
  geom_boxplot()

df1.10 <- df1 %>% 
  filter(force == "N10")
pairwise.t.test(x = df1.10$flow,
                g = df1.10$ankle,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.20 <- df1 %>% 
  filter(force == "N20")
pairwise.t.test(x = df1.20$flow,
                g = df1.20$ankle,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.30 <- df1 %>% 
  filter(force == "N30")
pairwise.t.test(x = df1.30$flow,
                g = df1.30$ankle,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.aD <- df1 %>% 
  filter(ankle == "dorsiflexion")
pairwise.t.test(x = df1.aD$flow,
                g = df1.aD$force,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.aN <- df1 %>% 
  filter(ankle == "neutral")
pairwise.t.test(x = df1.aN$flow,
                g = df1.aN$force,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.aP <- df1 %>% 
  filter(ankle == "plantarflexion")
pairwise.t.test(x = df1.aP$flow,
                g = df1.aP$force,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

## 3. Factorial ANOVA: One within and one between factor
df1m <- df1 %>% 
  mutate(ID = factor(rep(seq(1,60,by=1),3)))

df1m.ez <- ezANOVA(data = df1m,
                   dv = flow,
                   wid = ID,
                   within = ankle,
                   between = force,
                   type = 3,
                   return_aov = TRUE)
df1m.ez

#Check Assumptions (new qqPlot function)
qqPlot(df1m.ez[["aov"]][["ID:ankle"]][["residuals"]])
skewness(df1m.ez[["aov"]][["ID:ankle"]][["residuals"]])
kurtosis(df1m.ez[["aov"]][["ID:ankle"]][["residuals"]])
shapiro.test(x = df1m.ez[["aov"]][["ID:ankle"]][["residuals"]])

#Posthocs
ggplot(data = df1w, aes(x = force, y = flow, fill = ankle))+
  geom_boxplot()

df1.10 <- df1 %>% 
  filter(force == "N10")
pairwise.t.test(x = df1.10$flow,
                g = df1.10$ankle,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.20 <- df1 %>% 
  filter(force == "N20")
pairwise.t.test(x = df1.20$flow,
                g = df1.20$ankle,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.30 <- df1 %>% 
  filter(force == "N30")
pairwise.t.test(x = df1.30$flow,
                g = df1.30$ankle,
                paired = TRUE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.aD <- df1 %>% 
  filter(ankle == "dorsiflexion")
pairwise.t.test(x = df1.aD$flow,
                g = df1.aD$force,
                paired = FALSE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.aN <- df1 %>% 
  filter(ankle == "neutral")
pairwise.t.test(x = df1.aN$flow,
                g = df1.aN$force,
                paired = FALSE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")

df1.aP <- df1 %>% 
  filter(ankle == "plantarflexion")
pairwise.t.test(x = df1.aP$flow,
                g = df1.aP$force,
                paired = FALSE,
                alternative = "two.sided",
                p.adjust.method = "bonferroni")