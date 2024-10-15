
# Basics of Data Relationships
# POL 051 F24 
# TA Haley Daarstad

# Special thanks to Jack Rametta for some of the code

# Load packages ----------------------------------------------------------------

library(juanr) 
library(tidyverse)
library(ggthemes)


# Review Data ------------------------------------------------------------------

therm
?therm

View(therm)

# summarize() ------------------------------------------------------------------

# Let's look at the feeling thermometer for Christians  
therm |> select(ft_unions)

# Find the mean
# na.rm = TRUE removes NAs
therm |> summarize(mean(ft_unions, na.rm = TRUE)) 

# What would happen if we did not put it?
therm |> summarize(mean(ft_unions))

# Find the median 
therm |> summarize(median(ft_unions, na.rm = TRUE)) 

# Find the standard deviation 
therm |> summarize(sd(ft_unions, na.rm = TRUE)) 

# Find the minimum 
therm |> summarize(min(ft_unions, na.rm = TRUE)) 

# Find the maximum 
therm |> summarize(max(ft_unions, na.rm = TRUE)) 

# Can we do this in a singluar function of summarize? YES!!
therm |>
  summarize( # We put our new variable name and then what we want to calculate
            mean   = mean(ft_altright, na.rm = TRUE),
            median = median(ft_altright, na.rm = TRUE),
            sd     = sd(ft_altright, na.rm = TRUE),
            min    = min(ft_altright, na.rm = TRUE),
            max    = max(ft_altright, na.rm = TRUE)) 

# but what if we grouped by political party affiliation? 
# To group data by category we use the group_by() function
therm |> group_by(party_id) |>
         summarize(mean   = mean(ft_altright, na.rm = TRUE),
                   median = median(ft_altright, na.rm = TRUE),
                   sd     = sd(ft_altright, na.rm = TRUE),
                   min    = min(ft_altright, na.rm = TRUE),
                   max    = max(ft_altright, na.rm = TRUE)) 


# We can calculate counts of categorical variables too by using tally()
# Tally and tally + mutate 
therm |> 
  group_by(party_id) |> 
  tally() 

# Lets find the percent
therm |> 
  group_by(party_id) |> 
  tally() |> 
  mutate(pct = n/sum(n))
 
# Review of arrange 
therm |> 
  group_by(party_id) |> 
  summarize(fem_avg = mean(ft_fem,na.rm=TRUE)) |> 
  arrange(desc(fem_avg))
