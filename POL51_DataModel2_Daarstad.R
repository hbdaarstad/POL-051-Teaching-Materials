# 2024 Fall POL 51 - WK 5 Modeling
# TA Haley Daarstad
# credit for this code goes to Jack Rametta!! 

# Load packages ----------------------------------------------------------------
library(juanr)
library(MASS) #run install.packages("MASS") if this fails 
library(broom) #cleaning up model results   
library(ggthemes)
library(tidyverse)
library(forcats) #package for reordering factor variables by some value
library(ggdist) 

# generating data  -------------------------------------------------------------
 
set.seed(1995)

# ignore this chunk, making up data for us to play around with ####
corr_generator <- function(n, corr){
  mean_vector <- c(0, 0)
  cov_matrix <- matrix(c(1, corr, corr, 1), 2, 2)
  samples <- mvrnorm(n = n, mu = mean_vector, Sigma = cov_matrix)
  df <- as.data.frame(samples)
  colnames(df) <- c("X", "Y")
  return(df)
}
 
n <- 1000
df <- data.frame(corr_generator(n, 0.8),
                 corr_generator(n, 0.5),
                 corr_generator(n, 0),
                 corr_generator(n, -.25),
                 corr_generator(n, -.75))


# correlations -----------------------------------------------------------------

#
# For each of these, what's the rough correlation? 
df |> ggplot(aes(x = X,   y = Y))     + geom_point()
df |> ggplot(aes(x = X.1, y = Y.1))   + geom_point()
df |> ggplot(aes(x = X.2, y = Y.2))   + geom_point()
df |> ggplot(aes(x = X.3, y = Y.3))   + geom_point()
df |> ggplot(aes(x = X.4, y = Y.4))   + geom_point()
#
# What if you want the number instead of the plot? Easy 
df |> summarize(corr_xy = cor(X,Y))
df |> summarize(corr_xy = cor(X.1,Y.1))
df |> summarize(corr_xy = cor(X.2,Y.2))
df |> summarize(corr_xy = cor(X.3,Y.3))
df |> summarize(corr_xy = cor(X.4,Y.4))

library(ggcorrplot) #install.packages("ggcorrplot")

# simple example on simulated data 
ggcorrplot(cor(df),lab = TRUE)

# complex example with big five personality scores 
ggcorrplot(cor(subset(big_five, select = -c(country,age,sex))), 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           outline.color = "white",
           ggtheme = ggthemes::theme_few(),
           colors = c("#6D9EC1", "grey90", "#E46726"))

# run ?ggcorrplot for more info and to see what these different arguments mean 
# 
# corrr package ## 
#
# the corrr package is a nice package for quickly getting correlation tables

library(corrr) #install.packages("corrr")

# example with the simulated data 
correlate(df)

# example with big five personality scores 
correlate(big_five |> select(-c(country,age,sex)))

# Bivarate Moddels -------------------------------------------------------------


# Drawing lines through scatter plots, a time honored tradition 
df |> 
  ggplot(aes(x = X,   y = Y))    + geom_point() + geom_smooth(method = "lm")
df |> 
  ggplot(aes(x = X.1, y = Y.1))  + geom_point() + geom_smooth(method = "lm")
df |> 
  ggplot(aes(x = X.2, y = Y.2))  + geom_point() + geom_smooth(method = "lm")
df |> 
  ggplot(aes(x = X.3, y = Y.3))  + geom_point() + geom_smooth(method = "lm")
df |> 
  ggplot(aes(x = X.4, y = Y.4))  + geom_point() + geom_smooth(method = "lm")
# 
# Interpreting a simple bivariate model 
df |> 
  ggplot(aes(x = X,   y = Y))    + geom_point() + geom_smooth(method = "lm")

# Linear Regression ------------------------------------------------------------

model <- lm(Y ~ X, data = df)
model |> tidy()
# 

#### OLS: Continuous DV + Continuous IV ####
?big_five
big.five.model <- lm(conscientiousness_score*100 ~ age, big_five)
big.five.model |> tidy() 

# going to take a sample here so we don't break your machines 
big_five_small <- sample_n(big_five,10000)

big_five_small |> 
  ggplot(aes(x = age, y = conscientiousness_score)) + 
  geom_point(shape = 1,color = "darkgreen",alpha = .5) + 
  geom_smooth(method = "lm",color = "black") + 
  theme_few()

#### OLS: Continuous DV + Binary IV ####

# recode sex so it's a simple numeric binary 
big_five <- big_five |> 
  mutate(female = case_when(sex == "female" ~ 1,
                            sex == "male" ~ 0))
#
five.sex.model <- lm(agreeable_score*100 ~ female,big_five)
tidy(five.sex.model)

big_five_small |> 
  ggplot(aes(y = sex, x = agreeable_score,fill = sex)) + 
  ggdist::stat_histinterval(point_interval = "mean_qi",alpha = .6) +
  geom_vline(xintercept = .671,color = "blue",linetype = "dotted",size = 1) +
  geom_vline(xintercept = .671+.0434,color = "red",linetype = "dotted",size = 1) + 
  scale_fill_manual(values = c("red","blue")) + 
  ylab("") + 
  theme_few()


#### OLS: Continuous DV + Categorical IV ####

# Make a new age variable with only a few cateogries 
big_five <- big_five |> mutate(age_cats = case_when(
  age <= 18 ~ "Minors",
  age > 18 & age <= 24 ~ "College Age",
  age > 24 & age <= 35 ~ "Prime Age",
  age > 35 & age <= 50 ~ "Middle Age",
  age > 50 & age <= 65 ~ "Pretty Old Age",
  age > 65 & age <= 80 ~ "Social Security Beneficaries",
  age > 80 ~ "Old Enough for High Office"))

age.model <- lm(openness_score*100 ~ age_cats,big_five) #why did I multiply by 100 here?
age.model |> tidy()

big_five |> 
  ggplot(aes(y = age_cats, x = agreeable_score,fill = age_cats)) + 
  ggdist::stat_histinterval(point_interval = "mean_qi",alpha = .6) +
  ylab("") + 
  theme_gdocs() + 
  scale_fill_brewer(palette = "Spectral")

# Easier to see when arranged  

big_five |> 
  mutate(group = forcats::fct_reorder(age_cats,agreeable_score)) |> # you may find this function helpful.
  ggplot(aes(y = group, x = agreeable_score,group = group,fill = group)) + 
  ggdist::stat_histinterval(point_interval = "mean_qi",alpha = .6) +
  ylab("") + 
  theme_gdocs() + 
  scale_fill_brewer(palette = "Spectral")

#### Multiple Regression #### 
multi.model <- lm(openness_score*100 ~ female + neuroticism_score, big_five)
multi.model |> tidy()

# Model Predictions ------------------------------------------------------------

# Simplest possible example 
five.sex.model
scenario = crossing(female = 1)
augment(five.sex.model, newdata = scenario)

# More complex 
multi.model
scenario.multi <- crossing(female = 0, neuroticism_score = .5)
augment(multi.model, newdata = scenario.multi)

# More complex with grid 
scenario.multi.grid <- crossing(female = c(0,1), neuroticism_score = seq(0,1,.1))
augment(multi.model, newdata = scenario.multi.grid)

