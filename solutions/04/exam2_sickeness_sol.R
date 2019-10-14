sick <- as.vector(datasets::ldeaths)
sick_df <- data.frame(matrix(sick,ncol=12,byrow=TRUE,dimnames=list(1974:1979,month.abb)))

# Part 1 ----
first <- unlist(sick_df[1:3,], use.names = FALSE)
second <- unlist(sick_df[4:6,], use.names = FALSE)
(observed_diff <- mean(first) - mean(second))

# Part 2 ----
n_reps <- 10000
all_sickness <- c(first, second)
perm_results <- replicate(n_reps, {
  first_ind <- sample(length(all_sickness), length(first))
  new_first <- all_sickness[first_ind]
  new_second <- all_sickness[-first_ind]
  mean(new_first) - mean(new_second)
})

# Part 3 ----
plot(density(perm_results))
abline(v = observed_diff, col = "red")

# Part 4 ----
(p_val <- mean(perm_results >= observed_diff))
ci <- p_val + c(-1, 1) * qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_results))
c(
  lower = ci[1],
  p_val = p_val,
  upper = ci[2]
)
