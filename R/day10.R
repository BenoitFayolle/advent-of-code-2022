library(tidyverse)


# Question 1 --------------------------------------------------------------

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
  mutate(register_value_during_cycle = lag(register_value,1,default = 1))

register_spine %>% 
  filter(cycle_id %in% c(20,60,100,140,180,220)) %>%
  {sum(.$cycle_id*.$register_value_during_cycle)}
  

# Question 2 --------------------------------------------------------------
sprite_pos <- register_spine %>%
  rename(sprite_center_pixel_position = register_value_during_cycle) %>%
  mutate(sprite_pixels_position = map(sprite_center_pixel_position,~(.-1):(.+1))) %>%
  mutate(x=rep(0:39,6),y=rep(6:1,each=40)) %>%
  mutate(crt_pos = x) %>%
  mutate(is_pixel_lit = map2_lgl(sprite_pixels_position,crt_pos,function(.x,.y){
    .y %in% .x
  }))

sprite_pos %>%
  filter(is_pixel_lit) %>%
  ggplot(aes(x=x,y=y)) +
  geom_point()
  
