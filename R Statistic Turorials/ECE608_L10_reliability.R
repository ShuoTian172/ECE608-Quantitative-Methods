##Lecture 10: Reliability and Validity
#July 10, 2019

library(tidyverse)
library(psych)

##ICC Example
#Create a dataframe with random numbers
df <- data.frame(r1 = seq(1,100,by=1),
                  r2 = seq(4,103,by=1),
                  r3 = seq(1,200,by=2))

#Calculate ICC for the entire data.frame
psych::ICC(df)

##Bland-Altman Example
#Sample Bland-Altman plot from random data
library(BlandAltmanLeh)
df2 <- data.frame(r1 = rnorm(n=100, mean = 50, sd = 4),
                  r2 = rnorm(n=100, mean = 53, sd = 5))
#Create BA plot
bland.altman.plot(group1 = df2$r1, group2 = df2$r2, graph.sys = "ggplot2")

#Identify line plot
ggplot(data = df2, aes(x = r1, y = r2))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  xlim(35,70)+
  ylim(35,70)

