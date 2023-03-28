# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           2. read files.R
# date created:    2022-07-20 12:15:14 CEST
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(lubridate)
library(reticulate)
library(vroom)


# read data files ---------------------------------------------------------

# list the files in the processed chunks folder
files <- list.files("data/input/processed chunks", full.names = T)

# specify column classes
s <- cols(
  user_id = col_character(),
  app_name = col_character(),
  start_time = col_datetime(),
  fg_time_ms = col_double(),
  last_used = col_datetime()
)

# read processed chunks with vroom, given the specified column types
apk_usage <- map_df(files, vroom, col_types = s) %>% 
  na.omit() %>% 
  rename(apk = app_name)


# correct times -----------------------------------------------------------

# correct the timestamps by adding two hours
apk_usage_time <- apk_usage %>% 
  mutate(last_used = last_used + dhours(2))


# get duration per media segment ------------------------------------------
# 19,462,135 obs.

# create duration per media segment based on start_time and fg_time_ms
apk_usage_dur <- apk_usage_time %>%
  group_by(user_id, start_time, apk) %>%
  mutate(duration = c(fg_time_ms[1], diff(fg_time_ms)) %>% milliseconds(),
         start_segment = last_used - duration,
         end_segment = last_used,
         duration = str_remove(duration, "[Ss]$") %>% as.double()) %>% 
  ungroup() %>%
  select(user_id, apk, start_segment, duration, end_segment) %>%
  arrange(user_id, start_segment)


# filter ------------------------------------------------------------------
# 19,462,135 obs.

# filter negative and 0-durations, and include only study days
app_data <- apk_usage_dur %>% 
  filter(duration > 0) %>% # filter out durations lower than or equal to 0
  mutate(day = as_date(start_segment)) %>% # filter only data from start to end of the study
  filter(day >= "2020-06-02", day <= "2020-06-23")


# change IDs --------------------------------------------------------------
# 752,036 obs.

# The IDs in the digital trace data did not yet match the anonymized IDs
# that were used in the surveys. Therefore, the file KeyIDFile.csv was 
# temporarily stored in the data/survey/ folder to apply the changes in IDs
# The key file is stored on ResearchDrive 
# path: ~Alle bestanden/Awesome (Projectfolder)/ESM surveys - Cleaned/KeyIDFile.csv

# load ID key file
key_id <- read_csv2("data/input/KeyIDFile.csv") %>%
  transmute(ID, user_id = as.character(ID_ethica))

# change Ethica IDs into usable IDs for app usage data
df <- app_data %>% 
  left_join(key_id, by = "user_id") %>% 
  mutate(user_id = ID) %>% 
  filter(!is.na(user_id)) %>% 
  select(-ID)


# check time changes ------------------------------------------------------
# 732,839 obs.

# check whether corrected time aligns: compare first start of first session with wake-up time
df %>% 
  mutate(day = as_date(start_segment)) %>% 
  group_by(user_id, day) %>% 
  filter(start_segment == min(start_segment)) %>% 
  transmute(user_id, date = day, apk, start_segment) %>% 
  left_join(read_csv2("input/cleaned esm data - wave2_no attrition.csv") %>% 
              transmute(user_id = ID, date = as_date(TmSchE2), SLE03E2) %>% 
              na.omit(), by = c("user_id", "date"))


# write app dataframe -----------------------------------------------------

if(F) write_csv2(df, "data/processed/cleaned_logdata.csv")
