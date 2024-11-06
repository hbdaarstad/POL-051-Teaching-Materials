# 2024 Fall POL 51 - WK 7 Causality
# TA Haley Daarstad
# Professor Juan Tellez 

# Load packages ----------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(broom)

# Creating Data ----------------------------------------------------------------
set.seed(1995)

# rnorm() generates random data points in a normal distribution based on a 
# a given mean and variance 
# " n = " is the amount of random generated points
# "mean= " is the average
# "sd= " is the variance/standard deviation 
x1 <- rnorm(n = 100, mean = 0, sd = 1)

hist(x1, main="Distribution of X1",
     xlab="100 draws")
abline(v=mean(x1), col="red")

# rbinom() generates random binary data points 
# " n = " is the amount of random generated points
# "size= " is how many times you want this to occur
# "prob= " is the probability of getting a 1 or 0
x2 <- rbinom(n = 100, size = 1, prob = 0.50)

hist(x2, main="Distribution of X2",
     xlab="100 draws")

# More Regression --------------------------------------------------------------

# Connecting simulation to regression 
# we want a 1000 data points
n <- 1000

# creating a data frame with randomly generated data
df<- tibble(income = rnorm(n,30000,10000),
            age = rnorm(n,35,10),
            female = rbinom(n,1,.5),
            heart_health = 60 + .001*income - 1*age + 8*female + rnorm(n,0,10)) 
# rbinom() generates a vector of binomial distributed random variables
# Does how we calculate heart health look familiar?

# what type of model is this?
model1 <- lm(heart_health ~ income + age + female, df)
tidy(model1)
# do you think there is anything wrong with this model?


# Lets rescale income
df <- df %>%
  mutate(income_scale = income/1000)

model2 <- lm(heart_health ~ income_scale + age + female,df)
model2 %>%
  tidy()
# Do you think this is a better model? Why or Why not?

ggplot(df, aes(x = income_scale, y = heart_health, color = age)) +  geom_point()

# Confounders ------------------------------------------------------------------

# What if we have a confounding variable?

weekly_exercise <- rnorm(n,5,2)

df_confound <- tibble(income_scale = (rnorm(n,30000,10000) + 10000*weekly_exercise)/1000,
                      age = rnorm(n,35,10),
                      female = rbinom(n,1,.5),
                      heart_health = 60 + 10*weekly_exercise - 1*age + 8*female + rnorm(n,0,10))

model3 <- lm(heart_health ~ income_scale + age + female, df_confound)
tidy(model3)

df_confound %>%
  ggplot(aes(x = income_scale, y = heart_health)) + 
  geom_point()

# What is different from the model above?

# Okay lets correct for the confounder!

model.fix <- lm(heart_health ~ weekly_exercise + income_scale + age + female,df_confound)
tidy(model.fix)

df_confound %>%
  ggplot(aes(x=weekly_exercise,y = heart_health)) + 
  geom_point()

# The Fundamental Problem of Causal Inference ----------------------------------

# simulate a randomlized controled trial
df_exper <- tibble(treatment = rbinom(n,1,.5),
                   headache_duration = 2 - 1*treatment + rnorm(n,0,.25))

# 1 = treated
# 0 = untreated 

exper_model <- lm(headache_duration ~ treatment, df_exper)
tidy(exper_model)

# What can we say about being treated and headache duration?
# What is the counter factual? Can we compare those treated to the 
# REAL counter factual?

## Lets look at the estimated ATE or the average treatment effect
# the ATE is the estimated difference of means

# lets make two objects treated and untreated 
treated <- df_exper %>%
  filter(treatment == 1) 
treated <- treated$headache_duration
untreated <- df_exper %>%
  filter(treatment != 1) 
untreated <- untreated$headache_duration
# What is the ATE?
mean(treated) - mean(untreated)

