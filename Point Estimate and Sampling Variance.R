library(tidyverse)

population_size <- 259000000
p <- 0.425
sample_size <- 100
population <- c(rep("approve",population_size * p), rep("not",population_size * (1-p)))
sample <- sample(population, sample_size)
sample_approve <- sum(sample == "approve")
p_hat <- sample_approve / sample_size

### For loop version

simulations <- 10000
p_hat_results <- numeric(simulations)
for (i in 1:simulations) {
  sample <- sample(population, sample_size)
  sample_approve <- sum(sample == "approve")
  p_hat <- sample_approve / sample_size
  p_hat_results[i] <- p_hat
}
results <- data.frame(p_hat = p_hat_results)

results %>% ggplot(aes(x = p_hat)) +
  geom_histogram(color = "black", fill = "forestgreen", binwidth = 0.01) +
  geom_vline(aes(xintercept = p), linetype = "dashed", color = "firebrick") +
  labs(
    title = "Distribution of Sampled Approval Rates",
    subtitle = paste("Using", simulations, "simulated sample proportions with p = ", p),
    x = "Sample proportion",
    y = "Number of simulations"
  ) +
  scale_x_continuous(limits = c(0.00,1.00), breaks = seq(0.00,1.00,0.10)) 

mean(results$p_hat)
sd(results$p_hat)


results %>% ggplot(aes(x = p_hat)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "forestgreen", binwidth = 0.01) +
  geom_vline(aes(xintercept = p), linetype = "dashed", color = "firebrick") +
  labs(
    title = "Distribution of Sampled Approval Rates",
    subtitle = paste("Using", simulations, "simulated sample proportions with p = ", p),
    x = "Sample proportion",
    y = "Density"
  ) +
  scale_x_continuous(limits = c(0.00,1.00), breaks = seq(0.00,1.00,0.10)) +
  stat_function(fun = dnorm, args = list(mean = mean(results$p_hat), sd = sd(results$p_hat)))
