library(tidyverse)

# question 1 --------------------------------------------------------------

data_filename <- "data/day05.txt"
stacks_raw <- readLines(data_filename,n = 8)
moves_raw <- read_lines(data_filename,skip = 10)
# stacks_raw <- readLines(data_filename,n = 3)
# moves_raw <- read_lines(data_filename,skip = 5)

stack_position_ids <- seq.int(2,by=4,length.out=9)
# stack_position_ids <- seq.int(2,by=4,length.out=3)
stacks <- tibble(crate=map(strsplit(stacks_raw,""),
                       ~.[stack_position_ids])) %>%
  mutate(level_id = n():1) %>% arrange(level_id) %>%
  unnest(crate) %>%
  group_by(level_id) %>% mutate(stack_id = 1:n()) %>% ungroup %>%
  mutate(crate = gsub(" ",NA,crate)) %>%
  pivot_wider(names_from=stack_id,values_from=crate,names_prefix = "stack_") %>%
  select(-level_id) %>%
  as.list() %>%
  map(.,~.[which(!is.na(.))])

stacks

moves <- tibble(moves = moves_raw) %>%
  separate(moves,into=c(NA,"n_crates",NA,"from",NA,"to")) %>%
  mutate(across(everything(),as.integer))

rearrange <- function(stacks,move,is_question2=F){
  if (is_question2)
    crates_to_move <- tail(stacks[[move$from]],move$n_crates)
  else 
    crates_to_move <- rev(tail(stacks[[move$from]],move$n_crates))
  end
  stacks[[move$from]] <- head(stacks[[move$from]],-move$n_crates)
  stacks[[move$to]] <- c(stacks[[move$to]],crates_to_move)
  stacks
  print(stacks)
}

stacks_in <- stacks
for (move_id in 1:nrow(moves)){
  stacks_in <- rearrange(stacks_in,moves[move_id,])
}

map_chr(stacks_in,~tail(.,1)) %>% paste0(.,collapse="")


# question2 ---------------------------------------------------------------

stacks_in <- stacks
for (move_id in 1:nrow(moves)){
  stacks_in <- rearrange(stacks_in,moves[move_id,],T)
}

map_chr(stacks_in,~tail(.,1)) %>% paste0(.,collapse="")

