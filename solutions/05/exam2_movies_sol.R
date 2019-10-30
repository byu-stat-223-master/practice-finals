# Part 1 ----
# Null hypothesis: Comedy and Action movies have the same runtime
# Alternative hypothesis: Comedy movies are shorter than Action movies

# Part 2 ----
library(tidyverse)
movies <- read_csv(here::here("exams", "exam2", "movies.csv"))

# Part 3 ----
comedy <- filter(movies, genre == "Comedy")
action <- filter(movies, genre == "Action")

# Part 4 ----
(observed_diff <- mean(comedy$runtime) - mean(action$runtime))
n_samples <- 10000
bs_results <- replicate(n_samples, {
  mean(sample(comedy$runtime, replace = TRUE)) - mean(sample(action$runtime, replace = TRUE))
})
(ci <- quantile(bs_results, c(0.025, 0.975)))

# Part 5 ----
(observed_diff <- mean(comedy$runtime) - mean(action$runtime))
n_permutations <- 10000
all_runtimes <- c(comedy$runtime, action$runtime)
perm_results <- replicate(n_permutations, {
  comedy_ind <- sample(length(all_runtimes), length(comedy$runtime))
  mean(all_runtimes[comedy_ind]) - mean(all_runtimes[-comedy_ind])
})
(p_val <- mean(perm_results <= observed_diff))
ci <- p_val + c(-1, 0, 1) * qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_results))
names(ci) <- c("lower", "p_val", "upper")
ci

# Part 6 ----
# Both tests indicate significance. We reject the null hypothesis and conclude
# that Comedy films are shorter than Action films.