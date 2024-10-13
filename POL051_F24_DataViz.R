## Week 2 ## 

# Basics of Data Visualization
# POL 051 F24 
# TA Haley Daarstad

# Libraries --------------------------------------------------------------------

# Remember to load our libraries
library(gapminder)
library(tidyverse)
library(ggthemes)
library(moderndive)
library(nycflights13)
library(wesanderson) # wes anderson themed color pallete

## R Basics --------------------------------------------------------------------

# R is a big calculator

# addition
4 + 4 
# 8

# multiplication
22*2
# 44

# division
1/2
# 0.5

### We store most of our information in what we call "objects"

# making objects
df <- gapminder
df = gapminder
df = gapminder #notice these are the same! I prefer <-, but = works just the same

## Data Viz --------------------------------------------------------------------


## Scatter Plots

head(df)

# What would be the variables of interest?

# let's make a plot!
ggplot(data = df,aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# What are aes and geom?

## More on scatter plots!!

# Can't really see what's going on here!
ggplot(df, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(shape = 1)
# That's a little better
ggplot(df, aes(x = gdpPercap, y = lifeExp,
               shape =continent)) +
  geom_point()
# Add a theme
ggplot(df, aes(x = gdpPercap, y = lifeExp,
               color = continent)) +
  geom_point() +
  theme_few()  + 
  scale_color_brewer(palette="Dark2")


## Facet wrap

ggplot(df, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() + 
  # Makes seperate graphs based on a varaible in this case continent
  facet_wrap(~ continent, nrow = 2)

## Line Graphs

head(early_january_weather)
# Lets check what these variables mean!
?early_january_weather

ggplot(data = early_january_weather, 
       mapping = aes(x = time_hour, y = temp)) +
  geom_line()


## Histograms

# How do we add labels??

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram() +
  labs(
    # Lets add a label for the bottom (x axis)
    x = "Tempature",
    # lets add one for the y axis
    y = "Frequency",
    # Let's add a title
    title = "Hourly temperature in Newark for January 1-15, 2013"
  )

## Box plots

ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()


## Bar Plots

## DO NOT TOUCH JUST RUN ##
fruits <- tibble(
  fruit = c("apple", "apple", "orange", "apple", "orange")
)
fruits_counted <- tibble(
  fruit = c("apple", "orange"),
  number = c(3, 2)
)

ggplot(data = fruits, mapping = aes(x = fruit)) +
  geom_bar()

ggplot(data = fruits, mapping = aes(x = fruit, fill = fruit)) +
  geom_bar() +
  theme_clean() +
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1"))


#### YOUR TURN ####

# MAKE A PLOT WITH THE GAPMINDER CODE BELLOW
# Pick 5 countries in the Americas...
# colored by country. Add a theme, labels, and change the color 


gapminder_subset <- gapminder |>
  filter(continent == "Americas") 

print(gapminder_subset$country)

gapminder_subset <- gapminder_subset |>
  filter(country == "Argentina" | country == "Brazil" |  country == "Chile" | country == "Colombia" | country == "Cuba")




  