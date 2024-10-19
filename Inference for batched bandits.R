# UCB function to calculate upper confidence bounds
ucb <- function(mean_reward, n_pulls, total_pulls) {
  if (n_pulls == 0) {
    return(Inf)  # If the arm hasn't been pulled, its UCB is infinite
  } else {
    return(mean_reward + sqrt(2 * log(total_pulls) / n_pulls))
  }
}

# UCB1 Algorithm for 2-Armed Bandit
ucb_bandit <- function(n_rounds) {
  # Number of pulls and rewards for both arms
  n_pulls <- c(0, 0)
  total_reward <- c(0, 0)
  mean_reward <- c(0, 0)
  
  # Parameters of the Gaussian distribution for the two arms
  true_means <- c(1, 2)  # Example means for the arms
  
  # Initialize variables to store cumulative regret and arm choices
  cumulative_regret <- numeric(n_rounds)
  arm_choices <- numeric(n_rounds)
  
  # Main loop
  for (t in 1:n_rounds) {
    # Compute UCB for each arm
    ucb_values <- sapply(1:2, function(i) ucb(mean_reward[i], n_pulls[i], t))
    
    # Choose the arm with the highest UCB
    chosen_arm <- which.max(ucb_values)
    
    # Generate the reward from a Gaussian distribution with variance 1
    reward <- rnorm(1, mean = true_means[chosen_arm], sd = 1)
    
    # Update the statistics for the chosen arm
    n_pulls[chosen_arm] <- n_pulls[chosen_arm] + 1
    total_reward[chosen_arm] <- total_reward[chosen_arm] + reward
    mean_reward[chosen_arm] <- total_reward[chosen_arm] / n_pulls[chosen_arm]
    
    # Record the arm choice and cumulative regret (assuming the second arm is optimal)
    arm_choices[t] <- chosen_arm
    regret <- max(true_means) - true_means[chosen_arm]
    cumulative_regret[t] <- if (t == 1) regret else cumulative_regret[t - 1] + regret
  }
  
  # Return results
  return(list(
    arm_choices = arm_choices,
    cumulative_regret = cumulative_regret,
    n_pulls = n_pulls,
    mean_reward = mean_reward  # mean_reward represents the sample average
  ))
}

# Call the ucb_bandit function and store the result
n_rounds <- 1000
result <- ucb_bandit(n_rounds)

# Compute the reference line \sqrt{2t}
reference_line <- sqrt(2 * (1:n_rounds))

# Plot the cumulative regret
plot(result$cumulative_regret, type = "l", col = "blue", 
     xlab = "Round (t)", ylab = "Cumulative Regret", 
     main = "Cumulative Regret of UCB Algorithm with sqrt(2t)")

# Add the \sqrt{2t} reference line
lines(reference_line, col = "red", lty = 2)  # Add dashed red line for \sqrt{2t}

# Add legend
legend("topleft", legend = c("Cumulative Regret", expression(sqrt(2 * t))),
       col = c("blue", "red"), lty = c(1, 2), bty = "n")