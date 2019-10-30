ratings <- c(7.76,6.48,7.61,7.95,8.45,8.70,6.05,8.97,8.97,9.68,7.55,6.90,7.56,
             7.73,7.50,8.62,7.24,8.67,8.17,9.58,6.62,6.54,7.11,7.32,7.23,7.47,
             7.56,7.88,7.71,8.40,5.48,5.56,6.61,6.38,6.36,6.82,5.94,6.27,6.57,
             7.94,5.13,5.64,5.95,6.43,6.23,6.23,6.54,6.74,7.28,8.12)

# Part 1 ----
observed_stat <- cor(ratings[-1], ratings[-length(ratings)])

# Part 2 ----
n_permutations <- 10000
perm_results <- replicate(n_permutations, {
  new_ratings <- sample(ratings)
  cor(new_ratings[-1], new_ratings[-length(new_ratings)])
})

# Part 3 ----
# base R
plot(density(perm_results))
abline(v = observed_stat, col = "red")

# ggplot
library(ggplot2)
ggplot() +
  geom_density(aes(x = perm_results)) +
  geom_vline(xintercept = observed_stat, col = "red") +
  theme_bw()

# Part 4 ----
p_val <- mean(perm_results >= observed_stat)

# Part 5 ----
ci <- p_val + c(-1, 0, 1) * qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_results))
names(ci) <- c("lower", "p_val", "upper")
ci
