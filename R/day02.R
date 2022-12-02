library(tidyverse)

# Part 1 ------------------------------------------------------------------
decode_df_part1 <- tibble(code_opp = c("A","B","C"),
       code_me=c("X","Y","Z"),
       meaning=c("Rock","Paper","Scissors"),
       value=c(1,2,3))
outcome_df <- expand_grid(opp_meaning=decode_df_part1$meaning,
                          me_meaning=decode_df_part1$meaning) %>%
  mutate(outcome = c(3,6,0,0,3,6,6,0,3))

base_esg <- tibble(raw=readLines("data/day02.txt")) %>%
  separate(raw,into=c("opponent","me"))

base_esg %>%
  left_join(select(decode_df_part1,code_opp,opp_meaning=meaning,value_opp=value),
            by=c("opponent"="code_opp")) %>%
  left_join(select(decode_df_part1,code_me,me_meaning=meaning,value_me=value),
            by=c("me"="code_me")) %>%
  select(-c(opponent,me)) %>%
  left_join(outcome_df) %>%
  mutate(outcome = outcome + value_me) %>%
  pull(outcome) %>% sum
  

# Part 2 ------------------------------------------------------------------

decode_df_part2 <- tibble(code_opp = c("A","B","C"),
                    meaning=c("Rock","Paper","Scissors"),
                    code_me=c("X","Y","Z"),
                    outcome=c(0,3,6))
shape_df <- expand_grid(opp_meaning=decode_df_part2$meaning,
                          code_me=decode_df_part2$code_me) %>%
  left_join(decode_df_part2 %>% select(code_me,outcome),
            by=c("code_me"="code_me")) %>%
  left_join(outcome_df,by=c("outcome"="outcome",
                             "opp_meaning"="opp_meaning"))

base_esg %>% 
  left_join(decode_df_part2 %>% select(code_opp,meaning),by=c("opponent"="code_opp")) %>%
  left_join(shape_df,by=c("me"="code_me","meaning"="opp_meaning")) %>%
  left_join(decode_df_part1 %>% select(meaning,value),by=c("me_meaning"="meaning")) %>%
  mutate(final_outcome = outcome + value) %>%
  pull(final_outcome) %>% sum
