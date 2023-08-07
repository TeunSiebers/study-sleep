# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         sleep-study.Rproj
# title:           2 combine logdata and sleep data.R
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

# write output?
W <- T

# specify files with logdata
logfiles <- c(list.files("data/processed/main/", full.names = T), 
              list.files("data/processed/exploratory/", full.names = T),
              list.files("data/processed/sensitivity/", full.names = T))

for(i in seq_along(logfiles)) {
  # read data ---------------------------------------------------------------
  
  # read sleep data
  # 1,859 obs.; N = 156
  timeframes <- read_csv("data/processed/timeframes.csv")
  
  # read logdata
  logdata <- read_csv(logfiles[i]) %>% # N = 160; 267,642 obs.in 'cleaned_logdata.csv'
    filter(user_id %in% unique(timeframes$user_id)) %>% 
    arrange(user_id, start)
  
  
  # merge logdata and sleep data ----------------------------------------------
  
  # use foverlap(), from data.table package to find overlap in both dataframes
  # transform logdata and timeframes df into data.table and set key values for x 
  # and y, i.e., sort ID and times in the data.tables
  
  # DAYTIME USE ----
  df_day <- foverlaps(
    as.data.table(x = timeframes, key = c("user_id", "wt_yes", "hbefore_bt_yes")), 
    as.data.table(x = logdata, key = c("user_id", "start", "end")), 
    type = "any") %>% 
    mutate(start = as_datetime(ifelse(start < wt_yes, wt_yes, start)),
           end = as_datetime(ifelse(end > hbefore_bt_yes, hbefore_bt_yes, end)),
           sec = end - start,
           timeframe = "daytime_use") %>% 
    na.omit()
  
  # PRE-BEDTIME USE ----
  df_pre <- foverlaps(
    as.data.table(x = timeframes, key = c("user_id", "hbefore_bt_yes", "bt_yes")), 
    as.data.table(x = logdata, key = c("user_id", "start", "end")), 
    type = "any") %>% 
    mutate(start = as_datetime(ifelse(start < hbefore_bt_yes, hbefore_bt_yes, start)),
           end = as_datetime(ifelse(end > bt_yes, bt_yes, end)),
           sec = end - start,
           timeframe = "prebed_use") %>% 
    na.omit()
  
  # POST-BEDTIME USE ----
  df_post <- foverlaps(
    as.data.table(x = timeframes, key = c("user_id", "bt_yes", "wt_today")), 
    as.data.table(x = logdata, key = c("user_id", "start", "end")), 
    type = "any") %>% 
    mutate(start = as_datetime(ifelse(start < bt_yes, bt_yes, start)),
           end = as_datetime(ifelse(end > wt_today, wt_today, end)),
           sec = end - start,
           timeframe = "postbed_use") %>% 
    na.omit()
  
  # nest trace data in survey df ----
  full_df <- bind_rows(df_day, df_pre, df_post) %>% 
    arrange(user_id, start)
  
  
  # long to wide format -----------------------------------------------------
  
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
  
  
  # save file for Figshare and Mplus ----------------------------------------
  # 1,806 obs.; N = 155
  
  # add missing values (999999) and create time variable, indicating the days of the study
  mplus <- dur_df %>% 
    complete(user_id, date_res) %>% 
    mutate_all(~replace_na(., -999)) %>% 
    mutate(study_day = as.numeric(date_res - as_date("2020-06-02") + 1), 
           .after = user_id) %>% 
    select(-date_res)
  
  # change pathname into filename
  file <- logfiles[i] %>% 
    str_extract('(?<=main/|exploratory/|sensitivity/).+$') %>% 
    str_remove('\\.csv')
  
  # save as .csv file for descriptives
  if(str_detect(logfiles[i], "_", negate = T)) write_csv(mplus, glue("data/output/{file}.csv"))
  
  # create directory per .dat file for Mplus analyses
  if (!dir.exists(glue("mplus/{file}"))){
    dir.create(glue("mplus/{file}"))
  }
  
  # save as .dat file
  if(W) mplus %>%
    select(user_id, study_day, daytime_use, prebed_use, postbed_use, sleep_qual) %>%
    arrange(user_id, study_day) %>%
    write.table(glue("mplus/{file}/{file}.dat"),
                sep = "\t",
                dec = ".",
                row.names = FALSE,
                col.names = FALSE)
}


# run Mplus analyses -----------------------------------------------------

input_files <- list.files("mplus/", include.dirs = T, full.names = T,  recursive = T, pattern = ".inp")

if(F) runModels(target = input_files, quiet = F)
