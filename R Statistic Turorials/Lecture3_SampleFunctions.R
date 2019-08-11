#Lecture notes for ECE608 May 22, 2019

#Read in packages
library(tidyverse)   #Package to organize data
library(car)         #Package to use Levene's test
library(e1071)       #Package for skewness and kurtosis

#Create fake datasets
g1 <- rnorm(n=100, mean=100, sd=20)   #rnorm generates a random normal distribution
g2 <- rnorm(n=100, mean=150, sd=20)
df <- data.frame(g1,g2) %>%           #I use the pipeline to create a data.frame then create long-format data
  gather(key = "group",value = "value",1:2)

#Checking Assumptions
ggplot(dat=df, aes(x=value, fill=group))+
  geom_histogram(binwidth=10, alpha=0.5, position="identity")  #Create a histogram. Binwidth has to be manual

qqnorm(g1) #Creates Q-Q plot for each variable
qqnorm(g2)

shapiro.test(x = g1) #Runs shapiro-wilk normality test for each variable
shapiro.test(x = g2)

ks.test(x = g1, y = "pnorm", mean = mean(g1), sd = sd(g1)) #Runs K-S normality test
ks.test(x = g2, y = "pnorm", mean = mean(g2), sd = sd(g2))

leveneTest(value~group, data=df, center=mean) #Runs Levene's test for homogeneity of variance

#Running the actual Student's T-test
t.test(formula = value ~ group,  #Identify the columns and groups
       data = df,                #Identify the dataset
       alternative = "less",     #Identify if it is one-sided, or two-sided
       var.equal = TRUE,         #Identify results of Levene's test
       paired = FALSE)           #Identifies whether it is an independent or dependent t-test

ggplot(dat=df, aes(x=group, y=value))+
  geom_boxplot()     #Create a boxplot

# Activity: Use the iris dataset to look at the different in
# petal.length between setosa and versicolor species

head(iris)              #Look at first 6 rows of a data.frame
levels(iris$Species)    #Look at available levels of a variable

df <- iris %>%          #Filter out only the two species
  filter(Species == "setosa" | Species == "versicolor")

t.test(formula = Petal.Length ~ Species,
       data = df,
       alternative = "two.sided",
       var.equal = TRUE,
       paired = FALSE)

ggplot(dat=df, aes(x = Species, y = Petal.Length))+
  geom_boxplot()

# Sample Write-up
# The plant species setosa had smaller petal lengths
# than species versicolor (t(98)=-39.49, P<0.001)