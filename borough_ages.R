setwd("/Users/shaypepper/Documents/school/econometrics")
load("data/pums/pums_NY.RData")

library(dplyr)

attach(dat_pums_NY)

# --------------------------------------
data.with.age.factors <- dat_pums_NY %>%
  mutate(
    child = Age < 18,
    twenties = Age <= 29 & Age >= 18,
    thirties = Age <= 39 & Age >= 30,
    forties = Age <= 49 & Age >= 40,
    fifties = Age <= 64 & Age >= 50,
    old = Age >= 65
  ) %>%
  mutate(borough = factor(
    1 * in_Bronx + 
      2 * in_Brooklyn + 
      3 * in_Manhattan + 
      4 * in_StatenI + 
      5 * in_Queens,
    levels = 1:5,
    labels = c("Bronx", 
               "Brooklyn", 
               "Manhattan", 
               "Staten Island", 
               "Queens")
  )) %>%
  filter(in_NYC == 1) %>%
  select(child, twenties, thirties, forties, fifties, old, borough)

xtabs(~borough, data = data.with.age.factors)
t <- table(data.with.age.factors$borough, data.with.age.factors$child)
ftable(t)

