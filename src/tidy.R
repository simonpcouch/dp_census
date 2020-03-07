# Source code for tidying of Manson (2019) simulation data

# load libraries
library(tidyverse)

# read in the data
census <- read_csv("data/wide_dp1_150.csv")

# from the codebook:
#   Columns with a "_dp" suffix come from the 2010 Demonstration Data Product, 
#   and columns with a "_sf" suffix come from 2010 Summary File 1.
census_long <- census %>%
  select(-name_dp, -name_sf) %>%
  pivot_longer(c(contains("_sf"), contains("_dp")), 
               names_to = "variable", 
               values_to = "value") %>%
  separate(variable, c("variable", "type")) %>%
  filter(!variable %in% c('H7X001', 'H7Y001', 'H7Z001', 'H70001', 'H71001', 
                          'H76001', 'H77001', 'H78001', 'H79001', 'H8A001', 
                          'H8C001', 'H8D001', 'H8G001', 'H8H001', 'H7V001')) %>%
  separate(variable, into = c("question", "dem_group"), sep = 3)



