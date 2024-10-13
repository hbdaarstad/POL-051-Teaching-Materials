# Basics of Data Wrangling
# POL 051 F24 
# TA Haley Daarstad

# Libraries --------------------------------------------------------------------

library(juanr) 
library(tidyverse)
library(ggthemes)

# Review Data ------------------------------------------------------------------


?rebel_leader
rebel_leader


# Filter() ---------------------------------------------------------------------

# Filter for only rebel leaders that are dead 
rebel_leader |> 
  filter(dead == 1)

# Filter for only rebel leader that are dead AND in conflict with US 
rebel_leader |> 
  filter(dead == 1 & state == "United States")

# Saving this as an object 
dead_rebels <- rebel_leader |> 
  filter(dead == 1 & state == "United States")

# Printing objects 
dead_rebels 

# Lets use the %in% operator # 

# let's pick out rebel leaders in conflict with the US, UK, or France 
country_group = c("United States","United Kingdom","France")
# 
rebel_leader |> 
  filter(state %in% country_group) 

# Save this to an object 
rebel_group = rebel_leader |> 
  filter(state %in% country_group) 

# summarize() ------------------------------------------------------------------

rebel_leader |>
  #lets group by state
  group_by(state) |>
  # find the percent of dead leader's
  summarize(
    mean = mean(dead)
  ) |> print()

# How can we see the highest to lowest?? using arrange()
rebel_leader |>
  #lets group by state
  group_by(state) |>
  # find the percent of dead leader's
  mutate(
    # ignore this line of code
    gender_binary = ifelse(gender == "Female", 1, 0)
  ) |>
  summarize(
    mean = mean(gender_binary)
  ) |> 
  arrange(desc(mean)) |>
  print() 


# How about if I want to see the top 5?? using top_n()
rebel_leader |>
  #lets group by state
  group_by(state) |>
  # find the percent of dead leader's
  mutate(
    # Making a binary variable for whether a leader was assassinated
  assassination = case_when(
     death_cause == "Assassinated by government" ~ 1,
     death_cause ==  "Assassinated by rival group" ~ 1,
     death_cause ==  "Assassinated by external state" ~ 1,
     death_cause ==  "Executed" ~ 0,
     death_cause ==  "Disease/natural causes" ~ 0,
     death_cause ==  "Fratricide" ~ 0,
     death_cause == "Suicide" ~ 0,
     death_cause ==  "Homocide" ~ 0,
     death_cause ==  "Accident" ~ 0,
     death_cause ==  "KIA" ~ 0
    )
  ) |>
  summarize(
    mean = mean(assassination)
  ) |> 
  top_n(4) |>
  arrange(desc(mean))

# lets also group the rows by the variable gender and filter to only look at 
# Uganda
rebel_leader  |> 
  filter(state == "Uganda") |>
  group_by(state, gender) |>
  summarize(
    mean = mean(dead),
    median = median(dead),
    sd = mean(dead)
  )

# mutate() ---------------------------------------------------------------------

# Making a new column with mutate 
# changes the variable from a dummy 1 or 0 to a TRUE or FALSE

rebel_group = rebel_leader |> 
  mutate(dead.logical = as.logical(dead))

living_rebels = rebel_group |> 
  filter(dead == FALSE) #Notice, FALSE not in parantheses. Why? 

rebel_group |> 
  filter(dead == F) #Notice, this is the same 


# how to use the case_when() function

# ignore this line, fixing a bug with distinct 
living_rebels <- as.tibble(living_rebels)

# Distinct example. Let's say we want all of the study areas of rebels 
living_rebels |> 
  distinct(study_area)

# Mutate example. Let's say we want a new variable indicating where or not a rebel was born pre-1960. 
rebel_leader <- rebel_leader |> 
  mutate(birth_1960 = case_when(birth_year < 1960 ~ FALSE,
                                birth_year >= 1960 ~ TRUE)) 
rebel_leader$birth_1960

# OR 
rebel_leader <- rebel_leader |> 
  mutate(birth_1960 = case_when(birth_year < 1960 ~ 0,
                                birth_year >= 1960 ~ 1))  
rebel_leader$birth_1960

# OR 
rebel_leader <- rebel_leader |> 
  mutate(birth_1960 = case_when(birth_year < 1960 ~ "Born Before 1960",
                                birth_year >= 1960 ~ "Borth During or After 1960"))
rebel_leader$birth_1960

# lets make some plots with our new data sets ----------------------------------

# Make a plot with your new object 
ggplot(living_rebels, aes(x = children)) + geom_histogram()


# Notice these are different 
ggplot(rebel_leader, aes(x = children)) + geom_histogram()


# Other important functions ----------------------------------------------------

# Lets make simplify the data set to only 4 variables
rebel_select = rebel_leader |> 
  select(leadercode, state, religion, dead) |> print()

# Lets remove one variable
rebel_select = rebel_leader |> 
  select(-leadercode) |> print()

# Lets rename one variable

rebel_rename <- rebel_leader |> 
  rename(country = state) 
rebel_rename$country

