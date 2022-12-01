library(tidyverse)


# Question 1 --------------------------------------------------------------

q1 <- tibble(cals = readLines("data/day01.txt")) %>%
  mutate(is_blank = cals == "") %>%
  mutate(reindeer_id = cumsum(is_blank) + 1) %>%
  filter(!is_blank) %>%
  group_by(reindeer_id) %>%
  summarise(cals = sum(as.integer(cals))) %>%
  arrange(desc(cals))


# Question 2 --------------------------------------------------------------

q1 %>% head(3) %>% pull(cals) %>% sum  
