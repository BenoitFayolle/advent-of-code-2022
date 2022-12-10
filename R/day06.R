library(tidyverse)
raw <- readLines("data/day06.txt")
spl <- strsplit(raw,"")[[1]]

f<-T
i <- 4
spl_out <- spl
while(f){
  if (length(unique(head(spl_out,4)))==4){
    f <- F
    i <- i -1
  }
  i <- i + 1
  spl_out <- tail(spl_out,-1)
}
print(i)


# question 2 --------------------------------------------------------------

f<-T
i <- 14
spl_out <- spl
while(f){
  if (length(unique(head(spl_out,14)))==14){
    f <- F
    i <- i -1
  }
  i <- i + 1
  spl_out <- tail(spl_out,-1)
}
print(i)

