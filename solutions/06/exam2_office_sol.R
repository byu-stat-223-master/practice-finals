## Part 1 ----

# Null: Episode word counts for Dwight and Jim are not correlated
# Alternative: Episode word counts for Dwight and Jim are positively correlated

## Part 2 ----
# Setup
library(schrute)
library(tidyverse)
library(tidytext)

office_tokens <- theoffice %>% 
  unnest_tokens(output = word, input = text)

jim_words <- office_tokens %>% 
  filter(character == "Jim") %>% 
  count(season, episode, name = "jim")

dwight_words <- office_tokens %>% 
  filter(character == "Dwight") %>% 
  count(season, episode, name = "dwight")

jd_words <- full_join(jim_words, dwight_words, by = c("season", "episode")) %>% 
  replace_na(list(jim = 0, dwight = 0))

(observed_cor <- cor(jd_words$jim, jd_words$dwight))

## Part 3 ----
# Bootstrap
n_reps <- 10000
bs_stats <- replicate(n_reps, {
  bs_data <- jd_words[sample(nrow(jd_words), replace = TRUE),]
  cor(bs_data$jim, bs_data$dwight)
})
(bs_ci <- quantile(bs_stats, c(0.025, 0.975)))

## Part 4 ----
# Permutation test
perm_data <- jd_words
perm_stats <- replicate(n_reps, {
  perm_data$dwight <- sample(perm_data$dwight)
  cor(perm_data$jim, perm_data$dwight)
})

p_val <- mean(perm_stats >= observed_cor)
p_val + c(lower = -1, p_value = 0, upper = 1) * qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_stats))

## Part 5 ----
# Both bootstrapping and permutation testing indicate that we can reject the
# null hypothesis and conclude and Dwight and Jim have positively correlated
# word counts.

## Part 6 ----
tibble(permutation = perm_stats,
       bootstrap = bs_stats) %>% 
  pivot_longer(cols = everything(), names_to = "method") %>% 
  ggplot(aes(x = value, col = method)) +
  geom_density() +
  geom_vline(xintercept = observed_cor, col = "red") +
  theme_bw()
