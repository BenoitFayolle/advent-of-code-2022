library(tidyverse)


# question 1 --------------------------------------------------------------

find_common_item <- function(c1,c2){
  c1 <- strsplit(c1,"")[[1]]
  c2 <- strsplit(c2,"")[[1]]
  intersect(c1,c2)
}

df <- tibble(raw=readLines("data/day03.txt")) %>%
  mutate(char_count = map_int(raw,nchar)) %>%
  mutate(compart1=map2(raw,char_count,~str_sub(.x,1,.y/2))) %>%
  mutate(compart2=map2(raw,char_count,~str_sub(.x,.y/2+1,.y))) %>%
  mutate(rucksack_id = 1:n()) %>%
  unnest(cols = c(compart1,compart2)) %>%
  mutate(common_item = map2_chr(compart1,compart2,find_common_item)) %>%
  left_join(tibble(priority=c(letters,toupper(letters)),priority_id = 1:52),
            by=c("common_item"="priority"))
  
df %>% pull(priority_id) %>% sum

# question 2 --------------------------------------------------------------
intersect_multi <- function(vec){
  for (i in seq_along(vec)){
    if(i==1)
      out <- strsplit(vec[i],"")[[1]]
    out <- intersect(out,strsplit(vec[i],"")[[1]])
  }
  return(out)
}
tibble(raw=readLines("data/day03.txt")) %>%
  mutate(elf_group =rep(1:(nrow(.)/3),each=3)) %>%
  group_by(elf_group) %>%
  summarise(common_item = intersect_multi(raw)) %>%
  left_join(tibble(priority=c(letters,toupper(letters)),priority_id = 1:52),
            by=c("common_item"="priority")) %>%
  pull(priority_id) %>% sum

