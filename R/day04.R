library(tidyverse)

# q1 ----------------------------------------------------------------------

df <- tibble(raw=readLines("data/day04.txt")) %>%
  separate(raw,into=c("e1_start","e1_end","e2_start","e2_end")) %>%
  mutate(pair_id = 1:n()) %>%
  mutate(seq1=map2(e1_start,e1_end,~seq(.x,.y,by=1))) %>%
  mutate(seq2=map2(e2_start,e2_end,~seq(.x,.y,by=1)))

df %>%
  mutate(is_1_in_2 = map2_lgl(seq1,seq2,~all(.x %in% .y))) %>%
  mutate(is_2_in_1 = map2_lgl(seq1,seq2,~all(.y %in% .x))) %>%
  filter(is_1_in_2 | is_2_in_1) %>% nrow


# q2 ----------------------------------------------------------------------

df %>%
  mutate(is_1_in_2 = map2_lgl(seq1,seq2,~any(.x %in% .y))) %>%
  mutate(is_2_in_1 = map2_lgl(seq1,seq2,~any(.y %in% .x))) %>%
  filter(is_1_in_2 | is_2_in_1) %>% nrow

