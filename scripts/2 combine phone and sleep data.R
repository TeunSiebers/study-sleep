# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         sleep-study.Rproj
# title:           2 combine phone and sleep data.R
#
# description:
# ...
#
# bt_yes = bedtime yesterday
# wt_today = wake-up time today
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

library(tidyverse)
library(data.table)
library(glue)
library(MplusAutomation)

analysis <- "main"
# analysis <- "sens_24h"
# analysis <- "sens_99th"
# analysis <- "sens_99th_GP"

# write output?
W <- T

# read data ---------------------------------------------------------------

# read sleep data
# 1,859 obs.; N = 156
timeframes <- read_csv("data/processed/timeframes.csv")

# read smartphone use data (tracking)
# 263,016 obs.; N = 156;
phone <- read_csv(glue("data/processed/log_{analysis}.csv")) %>% # N = 160; 267,642 obs.in 'cleaned_logdata.csv'
  filter(user_id %in% unique(timeframes$user_id)) %>% 
  arrange(user_id, start)



# merge phone and sleep data ----------------------------------------------

# use foverlap(), from data.table package to find overlap in both dataframes
# transform phone and timeframes df into data.table and set key values for x 
# and y, i.e., sort ID and times in the data.tables

# DAYTIME USE ----
x_day <- as.data.table(x = phone, key = c("user_id", "start", "end"))
y_day <- as.data.table(x = timeframes, key = c("user_id", "wt_yes", "hbefore_bt_yes"))

# find overlap in both dfs: 184,132.; N = 155
df_day <- foverlaps(y_day, x_day, type = "any") %>% 
  as_tibble() %>% 
  mutate(start = as_datetime(ifelse(start < wt_yes, wt_yes, start)),
         end = as_datetime(ifelse(end > hbefore_bt_yes, hbefore_bt_yes, end)),
         sec = end - start,
         timeframe = "daytime_use") %>% 
  na.omit()

# PRE-BEDTIME USE ----
x_pre <- as.data.table(x = phone, key = c("user_id", "start", "end"))
y_pre <- as.data.table(x = timeframes, key = c("user_id", "hbefore_bt_yes", "bt_yes"))

# find overlap in both dfs: 12,628 obs.; N = 155
df_pre <- foverlaps(y_pre, x_pre, type = "any") %>% 
  as_tibble() %>% 
  mutate(start = as_datetime(ifelse(start < hbefore_bt_yes, hbefore_bt_yes, start)),
         end = as_datetime(ifelse(end > bt_yes, bt_yes, end)),
         sec = end - start,
         timeframe = "prebed_use") %>% 
  na.omit()

# POST-BEDTIME USE ----
x_post <- as.data.table(x = phone, key = c("user_id", "start", "end"))
y_post <- as.data.table(x = timeframes, key = c("user_id", "bt_yes", "wt_today"))

# find overlap in both dfs: 10,318 obs.; N = 155
df_post <- foverlaps(y_post, x_post, type = "any") %>% 
  as_tibble() %>% 
  mutate(start = as_datetime(ifelse(start < bt_yes, bt_yes, start)),
         end = as_datetime(ifelse(end > wt_today, wt_today, end)),
         sec = end - start,
         timeframe = "postbed_use") %>% 
  na.omit()

# nest trace data in survey df ----
full_df <- bind_rows(df_day, df_pre, df_post) %>% 
  arrange(user_id, start)


# long to wide format -----------------------------------------------------
# 150,562 obs.; N = 155

# sum durations in hours within each timeframe
df <- full_df %>% 
  group_by(user_id, date_res, timeframe, sleep_qual) %>% # keep sleep quality in the dataset
  summarise(total_dur = as.numeric(sum(sec, na.rm = T)) / 60 / 60)

# change to wider format
dur_df <- df %>% 
  group_by(user_id, date_res, sleep_qual) %>% # keep sleep quality in the dataset
  pivot_wider(names_from = timeframe, values_from = total_dur) %>% # change long format into wide format
  mutate_at(vars(daytime_use, prebed_use, postbed_use), ~replace(., is.na(.), 0)) %>%  # replace missings by 0 seconds
  relocate(sleep_qual, .after = postbed_use) %>% 
  ungroup()
  
# save user_ids for extracting demographic information later
if(F) dur_df$user_id %>% unique %>% write.table(file = "data/processed/user_ids.txt")


# save file for Figshare and Mplus ----------------------------------------
# 1,806 obs.; N = 155

# add missing values (999999) and create time variable, indicating the days of the study
mplus <- dur_df %>% 
  complete(user_id, date_res) %>% 
  mutate_all(~replace_na(., -999)) %>% 
  mutate(study_day = as.numeric(date_res - as_date("2020-06-02") + 1), 
         .after = user_id) %>% 
  select(-date_res)

# save as .csv file for Figshare
# if(W) write_csv(mplus, glue("data/output/mplus_{analysis}.csv"))

# save as .dat file
if(W) mplus %>% 
  select(user_id, study_day, daytime_use, prebed_use, postbed_use, sleep_qual) %>% 
  arrange(user_id, study_day) %>% 
  write.table(glue("data/mplus/{analysis}/mplus_{analysis}.dat"), 
              sep = "\t",
              dec = ".",
              row.names = FALSE,
              col.names = FALSE)


# run Mplus analyses -----------------------------------------------------

if(F) runModels(target = list.files("mplus/", include.dirs = T, full.names = T), 
                quiet = F)
