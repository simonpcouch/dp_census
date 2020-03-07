# Source code for tidying of Manson (2019) simulation data

# load libraries
library(tidyverse)
library(vroom)

# read in the data
census <- vroom("data/wide_dp1_150.csv")

# from the codebook:
#   Columns with a "_dp" suffix come from the 2010 Demonstration Data Product, 
#   and columns with a "_sf" suffix come from 2010 Summary File 1.
# H7Z, specifically, contains counts by ethnicity/race category combinations
census_tidy <- census %>%
  # select non-aggregated columns from H7Z
  select(contains("H7Z"), -contains(c("001", "002", "010")), gisjoin) %>%
  # make the data long!
  pivot_longer(c(contains("_sf"), contains("_dp")), 
               names_to = "variable", 
               values_to = "value") %>%
  # break up the variable names so that they give both the variable type
  # and whether the reported value is public or privatized
  separate(variable, c("variable", "type"), sep = "_") %>%
  # recode the variable column to give the ethnicity/race description
  # from the codebook
  mutate(ethnicity_race = case_when(
    variable == "H7Z003" ~ "Not Hispanic or Latino: White alone",
    variable == "H7Z004" ~ "Not Hispanic or Latino: Black or African American alone",
    variable == "H7Z005" ~ "Not Hispanic or Latino: American Indian and Alaska Native alone",
    variable == "H7Z006" ~ "Not Hispanic or Latino: Asian alone",
    variable == "H7Z007" ~ "Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone",
    variable == "H7Z008" ~ "Not Hispanic or Latino: Some Other Race alone",
    variable == "H7Z009" ~ "Not Hispanic or Latino: Two or More Races",
    variable == "H7Z011" ~ "Hispanic or Latino: White alone",
    variable == "H7Z012" ~ "Hispanic or Latino: Black or African American alone",
    variable == "H7Z013" ~ "Hispanic or Latino: American Indian and Alaska Native alone",
    variable == "H7Z014" ~ "Hispanic or Latino: Asian alone",
    variable == "H7Z015" ~ "Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone",
    variable == "H7Z016" ~ "Hispanic or Latino: Some Other Race alone",
    variable == "H7Z017" ~ "Hispanic or Latino: Two or More Races",
  )) %>%
  select(-variable) %>%
  # then, break up ethnicity/race into two columns
  separate(ethnicity_race, 
           c("ethnicity", "race"), 
           sep = fixed(": "), 
           remove = FALSE) %>%
  # pivot to a slightly wider format, where the public and private count
  # estimates are given on the same row
  pivot_wider(id_cols = c(gisjoin, ethnicity, race, ethnicity_race, type),
              names_from = type,
              values_from = value) %>%
  mutate(pct_change = (dp - sf) / sf * 100)

# save a small sample of the data for easier reproducibility from gh
census_tidy_small <- census_tidy[1:100000,]
save(census_tidy_small, file = "data/census_tidy_small.Rda")
