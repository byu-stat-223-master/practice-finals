## Part 1 ----
# Null hypothesis: priesthood session has same word count as general relief society session
# Alternative hypothesis: priesthood session has higher word count than general relief society session

# Part 2 ----
library(tidyverse)
conf <- read_csv(here::here("exams", "exam2", "general-conference.csv"))


# Part 3 ----
ph_words <- filter(conf, str_detect(session, "Priesthood"))$words
rs_words <- filter(conf, str_detect(session, "Relief"))$words

eyring <- filter(conf, str_detect(speaker, "Eyring"))$words
holland <- filter(conf, str_detect(speaker, "Holland"))$words

monson <- conf %>% 
  filter(year >= 2008, year < 2018, speaker == "Thomas S. Monson", words > 1000)

monson_1 <- monson %>% 
  mutate(date = lubridate::ymd(glue::glue("{year}-{month}-1"))) %>% 
  filter(date <= "2012-04-01") %>% 
  pull(words)

monson_2 <- monson %>% 
  mutate(date = lubridate::ymd(glue::glue("{year}-{month}-1"))) %>% 
  filter(date > "2012-04-01") %>% 
  pull(words)

# Part 4 ----
## BS Part 1 ----
(observed_diff <- median(ph_words) - median(rs_words))
(observed_diff <- median(eyring) - median(holland))
(observed_diff <- median(monson_1) - median(monson_2))

## BS Part 2 ----
n_reps <- 10000
bs_diffs <- replicate(n_reps, {
  median(sample(monson_1, replace = TRUE)) - median(sample(monson_2, replace = TRUE))
})

## BS Part 3 ----
(ci <- quantile(bs_diffs, c(0.025, 0.975)))

# Part 5 ----
## PM Part 1 ----
(observed_diff <- median(monson_1) - median(monson_2))

## PM Part 2 ----
all_words <- c(monson_1, monson_2)
perm_results <- replicate(n_reps, {
  ind <- sample(length(all_words), length(monson_1))
  new_1 <- all_words[ind]
  new_2 <- all_words[-ind]
  median(new_1) - median(new_2)
})

# Eyring
all_words <- c(eyring, holland)
perm_results <- replicate(n_reps, {
  eyring_ind <- sample(length(all_words), length(eyring))
  new_eyring <- all_words[eyring_ind]
  new_holland <- all_words[-eyring_ind]
  median(new_eyring) - median(new_holland)
})

## PM Part 3 ----
p_val <- mean(perm_results >= observed_diff)
ci <- p_val + c(-1, 1) * qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_results))
c(
  lower = ci[1],
  p_val = p_val,
  upper = ci[2]
)

# Part 6 ----
# Both tests fail to indicate significance. Therefore, we fail to reject the null hypothesis in this case.
