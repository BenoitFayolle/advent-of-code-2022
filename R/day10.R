library(tidyverse)

raw<-readLines("data/day10.txt")
ops_df <- tibble(raw=c("start 1",raw)) %>%
  separate(raw,into=c("operation","value"),fill="right",sep=" ") %>%
  mutate(across(value,as.integer)) %>%
  mutate(cycle_length=case_when(operation=="addx" ~ 2L,
                                operation=="start" ~ 0L,
                                T ~ 1L)) %>%
  mutate(cycle_id = cumsum(cycle_length)) %>%
  mutate(register_value = cumsum(ifelse(is.na(value),0L,value)))

register_spine <- tibble(cycle_id=1:max(ops_df$cycle_id)) %>%
  mutate(register_value = map_int(cycle_id,function(c_id){
    ops_df_id <- tail(which(ops_df$cycle_id <= c_id),1)
      ops_df$register_value[ops_df_id]
    }
    )) %>%
  mutate(register_value_during_cycle = lag(register_value,1))

register_spine %>% 
  filter(cycle_id %in% c(20,60,100,140,180,220)) %>%
  {sum(.$cycle_id*.$register_value_during_cycle)}
  
