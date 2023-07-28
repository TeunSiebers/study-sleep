# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleepRproj
# title:           1.1 filter raw tracking data.R
#
# description:
# This script reads the raw input file from the app usage data (+- 130M rows), 
# filters foreground times that are 0 or less, and exports the remaining part 
# (+- 19M rows) in three chunks for further usage.
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(vroom)


# read zipped file --------------------------------------------------------

# read header only
header <- vroom("data/input/3.csv", n_max = 1)

# define header names and types
col_n <- names(header)
col_t <- map(header, class) %>% sapply("[[", 1)

# define selection of headers
col_s <- c("user_id", "app_name", "start_time", "fg_time_ms", "last_used")

# rewrite first batch
if(F) vroom("data/input/3.csv", 
      n_max = 50000000, 
      col_names = col_n, 
      col_select = col_s) %>% 
  filter(fg_time_ms > 0) %>% 
  vroom_write("data/processed/processed chunks/first_50M.csv", delim = ",")

# rewrite second batch
if(F) vroom("data/input/3.csv", 
      skip = 50000000, 
      n_max = 50000000,
      col_names = col_n,
      col_select = col_s) %>% 
  filter(fg_time_ms > 0) %>% 
  vroom_write("data/processed/processed chunks/second_50M.csv", delim = ",")

# rewrite third batch
if(F) vroom("data/input/3.csv", 
      skip = 100000000, 
      n_max = 50000000,
      col_names = col_n,
      col_select = col_s) %>% 
  filter(fg_time_ms > 0) %>% 
  vroom_write("data/processed/processed chunks/third_50M.csv", delim = ",")
