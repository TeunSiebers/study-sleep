# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         digital trace.Rproj
# title:           1. filter apk usage data.R
# date created:    Mon Feb  6 08:49:12 2023
# last updated:    Mon Feb  6 08:49:12 2023
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# remove all variables (start from scratch)
rm(list = ls())

# load packages
library(tidyverse)
library(vroom)


# read zipped file --------------------------------------------------------

# read header only
header <- vroom("data/3.csv", n_max = 1)

# define header names and types
col_n <- names(header)
col_t <- map(header, class) %>% sapply("[[", 1)

# define selection of headers
col_s <- c("user_id", "app_name", "start_time", "fg_time_ms", "last_used")

# rewrite first batch
if(F) vroom("data/3.csv", 
      n_max = 50000000, 
      col_names = col_n, 
      col_select = col_s) %>% 
  filter(fg_time_ms > 0) %>% 
  vroom_write("data/processed chunks/first_50M.csv", delim = ",")

# rewrite second batch
if(F) vroom("data/3.csv", 
      skip = 50000000, 
      n_max = 50000000,
      col_names = col_n,
      col_select = col_s) %>% 
  filter(fg_time_ms > 0) %>% 
  vroom_write("data/processed chunks/second_50M.csv", delim = ",")

# rewrite third batch
if(F) vroom("data/3.csv", 
      skip = 100000000, 
      n_max = 50000000,
      col_names = col_n,
      col_select = col_s) %>% 
  filter(fg_time_ms > 0) %>% 
  vroom_write("data/processed chunks/third_50M.csv", delim = ",")
