# Source code for visualization of Manson (2019) simulations

library(tidyverse)
library(patchwork)
library(rmutil)

# percent error by race
error_by_race_plot <- census_tidy %>%
  ggplot() +
  aes(y = pct_change, x = race_abbrev) +
  geom_boxplot(outlier.alpha = 0) +
  labs(x = "Racial Category", 
       y = "Percent Error") +
  ylim(-100, 200) +
  theme_minimal() +
  theme(text = element_text(family = "Times"))

ggsave("paper/figures/error_by_race.png", error_by_race_plot, 
       width = 4.5, height = 3, units = "in")


# percent error by true population
set.seed(343)
error_by_true_pop_plot <- census_tidy %>%
  sample_n(20000) %>%
  ggplot() +
  aes(x = sf, y = pct_change) +
  geom_point(alpha = .2) +
  labs(x = "True Population Count", 
       y = "Percent Error") +
  theme_minimal() +
  xlim(0, 1000) + 
  scale_y_continuous(breaks = seq(-100, 500, 100), limits = c(-100, 500)) +
  theme(text = element_text(family = "Times"),
        plot.margin = margin(0, 5.5, 5.5, 5.5, "pt"))

# population count per census block, by race
pop_by_race_plot <- census_tidy %>%
  mutate(is_non_hl_white = case_when(
    ethnicity_race == "Not Hispanic or Latino: White alone" ~ "White",
    TRUE ~ "People of Color"
  )) %>%
  ggplot() +
  aes(x = sf, fill = is_non_hl_white) +
  geom_density(alpha = .75) +
  labs(fill = "Racial Category",
       y = "Sqrt(Density)",
       x = "") +
  theme_minimal() +
  scale_y_sqrt() +
  scale_x_continuous(limits = c(0, 1000),
                     labels = NULL) +
  theme(text = element_text(family = "Times"),
        legend.position = c(.65, .5),
        plot.margin = margin(5.5, 5.5, 0, 5.5, "pt")) +
  scale_fill_grey()
  

error_by_true_pop_by_race <- pop_by_race_plot / error_by_true_pop_plot

ggsave("paper/figures/error_by_true_pop.png", error_by_true_pop_by_race,
       width = 4.5, height = 5, units = "in")


# finally, a visualization aiming to give intuition for what
# epsilon-distributed error means for populations of very different sizes


dif_priv_plot <- data.frame(x = c(rlaplace(50000, 50, 40), rlaplace(50000, 1000, 40)),
           type = rep(c("A", "B"), each = 50000)) %>%
  ggplot(aes(x = x, fill = type)) +
  geom_density(alpha = .75) +
  geom_vline(xintercept = 50) +
  geom_vline(xintercept = 1000) +
  scale_y_continuous(breaks = NULL, limits = c(0, .014)) +
  scale_x_continuous(breaks = seq(-500, 1500, 500)) +
  theme_minimal() +
  theme(text = element_text(family = "Times"),
        legend.position = c(.5, .5)) +
  labs(x = "Estimated Population Count",
       y = "Density",
       fill = "Population") +
  scale_fill_grey()

ggsave("paper/figures/dif_priv.png", dif_priv_plot,
       width = 4.5, height = 3, units = "in")

