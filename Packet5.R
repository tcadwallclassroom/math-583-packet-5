simulations <- 10000    ### Number of simulated tests
num_cups <- 10          ### Number of cups of coffee per test
prob_correct <- .5      ### Probability that Sophia guesses correctly per cup

results <- numeric(simulations)
outcomes <- c("Correct", "Incorrect")
prob_vector <- c(prob_correct, 1 - prob_correct)

for(i in 1:simulations){
  num_correct <- 0 ### Keep track of the number of correct guesses per simulation
  for(j in 1:num_cups){
    guess <- sample(outcomes, 1, prob = prob_vector)
    if(guess == "Correct"){
      num_correct <- num_correct + 1
    } 
  }
  results[i] <- num_correct
}

results <- data.frame(results)
colnames(results) <- "correct"
glimpse(results)

results_table <- results %>% table()
results_table

sims_as_extreme <- results %>% filter(correct >= 8) %>% nrow()
sims_as_extreme
p_val <- sims_as_extreme/simulations
p_val



