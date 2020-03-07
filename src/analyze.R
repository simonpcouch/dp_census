# Source code for analyses of Manson (2019) simulations

# this file makes use of the `census_tidy` dataframe resulting from
# the script at src/tidy.R. a sample of the data is available at
# data/census_tidy_small.Rda

# load libraries
library(tidyverse)
library(xtable)

# goal measures:
#   Proportion of nonzero populations estimated to be 0 by race/ethnicity 
#      category
#   Proportion of nonzero populations estimated to be l/geq _x the true 
#      population by race/ethnicity category
#   Proportion of nonzero estimates for true populations of 0 by race/ethnicity
#      category

# first, calculate these measures for non-h/l white vs all other
error_estimate_by_race_ethnicity_bin <- census_tidy %>%
  filter(sf != 0) %>%
  mutate(is_non_hl_white = case_when(
    ethnicity_race == "Not Hispanic or Latino: White alone" ~ ethnicity_race,
    TRUE ~ "Racial Minority"
  )) %>%
  group_by(is_non_hl_white) %>%
  summarize(`Zero` = mean(pct_change == -100, na.rm = TRUE),
            `Less Than Half of True Population` = mean(pct_change == -100, na.rm = TRUE),
            `Twice the True Population` = mean(pct_change > 200, na.rm = TRUE),
            `Ten Times the True Population` = mean(pct_change > 1000, na.rm = TRUE))

error_estimate_by_race_ethnicity_bin

# save to file
write_csv(error_estimate_by_race_ethnicity_bin,
          path = "data/summaries/error_estimate_by_race_ethnicity_bin.csv")

# calculate this by race/ethnicity category as well
error_estimate_by_race_ethnicity <- census_tidy %>%
  filter(sf != 0) %>%
  group_by(ethnicity_race) %>%
  rename('Ethnicity/Race Category' = ethnicity_race) %>%
  summarize(`Zero` = mean(pct_change == -100, na.rm = TRUE),
            `Less Than Half of True Population` = mean(pct_change == -100, na.rm = TRUE),
            `Twice the True Population` = mean(pct_change > 200, na.rm = TRUE),
            `Ten Times the True Population` = mean(pct_change > 1000, na.rm = TRUE))

error_estimate_by_race_ethnicity

# save to file
write_csv(error_estimate_by_race_ethnicity,
          path = "data/summaries/error_estimate_by_race_ethnicity.csv")

# write this to a tex file as well
print(xtable(error_estimate_by_race_ethnicity, digits = 3),
      file = "paper/figures/error_estimate_by_race_ethnicity.tex",
      include.rownames = FALSE,
      hline.after = c(0, 1))

# also, how often are nonzero counts given for true counts of zero?
nonzero_estimate_by_race_ethnicity_bin <- census_tidy %>%
  mutate(is_non_hl_white = case_when(
    ethnicity_race == "Not Hispanic or Latino: White alone" ~ ethnicity_race,
    TRUE ~ "Racial Minority"
  )) %>%
  group_by(is_non_hl_white) %>%
  summarize(`Estimated Nonzero Count` = mean(pct_change == Inf, na.rm = TRUE))  

nonzero_estimate_by_race_ethnicity_bin

# save to file
write_csv(nonzero_estimate_by_race_ethnicity_bin,
          path = "data/summaries/nonzero_estimate_by_race_ethnicity_bin.csv")


