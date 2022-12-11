library(tidyverse)
raw <- readLines("data/day07.txt")


# question 1 --------------------------------------------------------------

df_raw <- tibble(raw) %>%
  mutate(is_command = grepl("^\\$",raw)) %>%
  mutate(incr = ifelse(is_command,1L,0L)) %>%
  mutate(id = cumsum(incr)) %>%
  select(-c(incr,is_command)) %>%
  nest(res=raw) %>%
  mutate(command = map_chr(res,~.$raw[1])) %>%
  mutate(res=map(res,~tail(.,-1))) %>%
  mutate(cded_dir = map_chr(command,~str_split(.,pattern = "\\$ cd ")[[1]][2])) %>%
  mutate(res_leaded = lead(res,1)) %>%
  mutate(is_ls = lead(grepl("^\\$ ls",command),1)) %>%
  filter(!grepl("^\\$ ls",command))


current_dir <- rep(NA,nrow(df_raw))
for (i in 1:nrow(df_raw)){
  cd <- df_raw$cded_dir[i]
  if(cd == "/"){
    current_dir[i] <- "/"
  } else if (cd == ".."){
    current_dir[i] <- dirname(current_dir[i-1])
  } else {
    print(cd)
    current_dir[i] <- gsub("//","/",file.path(current_dir[i-1],cd))
  }
}

transform_ls_res <- function(df){
  df %>% 
    separate(raw,into=c("size","dir")) %>%
    filter(size!="dir") %>%
    mutate(across(size,as.integer))
}

files_df <- df_raw %>% 
  mutate(directory = current_dir) %>%
  filter(is_ls) %>%
  select(ls_res=res_leaded,directory) %>%
  unnest(ls_res,keep_empty = T) %>%
  separate(raw,into=c("size","filename"),sep = " ") %>%
  mutate(size= ifelse(size=="dir",0,size)) %>%
  mutate(across(size,as.integer))

dir_df <- files_df %>%
  group_by(dirpath=directory) %>%
  summarise(size=sum(size)) 

full_size <- rep(NA,nrow(dir_df))
for (i in 1:nrow(dir_df)){
  dirpath <- dir_df$dirpath[i]
  idx <- grep(paste0("^",dirpath),dir_df$dirpath)
  full_size[i] <- sum(dir_df$size[idx])
}

dir_df_full <- dir_df %>%
  mutate(full_size=full_size)

dir_df_full %>% filter(full_size<=100000) %>% pull(full_size) %>% sum()


# question 2 --------------------------------------------------------------

space_needed <- 30000000 - (70000000 - max(dir_df_full$full_size))

dir_df_full %>% filter(full_size>=space_needed) %>% arrange(full_size)
