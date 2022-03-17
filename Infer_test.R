library(infer)
library(tidyverse)

vignette("observed_stat_examples")

data(gss)

# take a look at its structure
glimpse(gss)
p_hat <- gss %>% 
  specify(response = sex, success = "female") %>%
  calculate(stat = "prop")

p_hat <- gss %>% 
  observe(response = sex, success = "female", stat = "prop")

boot_dist <- gss %>%
  specify(response = sex, success = "female") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")

percentile_ci <- get_ci(boot_dist)

visualize(boot_dist) +
  shade_p_value(obs_stat = p_hat, direction = "greater")

standard_error_ci <- boot_dist %>%
  get_ci(type = "se", point_estimate = p_hat)

visualize(boot_dist) +
  shade_confidence_interval(endpoints = standard_error_ci)


