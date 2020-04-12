## Part 1 ----
(observed_cor <- cor(cars$speed, cars$dist))

## Part 2 ----
n_reps <- 10000
perm_cars <- cars
perm_stats <- replicate(n_reps, {
  perm_cars$speed <- sample(perm_cars$speed)
  cor(perm_cars$speed, perm_cars$dist)
})

## Part 3 ----
library(ggplot2)
ggplot(mapping = aes(x = perm_stats)) +
  geom_density() +
  geom_vline(xintercept = observed_cor, col = "red") +
  theme_bw()

## Part 4 ----
(p_val <- mean(perm_stats >= observed_cor))

## Part 5 ----
p_val + c(lower = -1, p_value = 0, upper = 1) * qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_stats))

## Part 6 ---
# We can reject the null hypothesis and conclude there is a significant positive
# correlation between speed and stopping distance.