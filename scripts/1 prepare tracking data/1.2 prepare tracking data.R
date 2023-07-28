# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           1.2 prepare tracking data.R
#
# description:
# This script merges the three processed chunks from the raw datafile. It 
# adds two hours to compensate for the different timezone, and generates 
# durations per app activity and filters out all app activities that last 0 
# sec or less. It then changes the Ethica IDs into participants IDs via
# the keyfile, and finally adds app name and category to each activitiy based
# on the Google Playstore.
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(lubridate)
library(reticulate)
library(vroom) # for loading files with vroom


# read data files ---------------------------------------------------------

# list the files in the processed chunks folder
files <- list.files("data/processed/processed chunks/", full.names = T)

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

# correct the timestamps by adding two hours due to different timezone
apk_usage_time <- apk_usage %>% 
  mutate(last_used = last_used + dhours(2)) %>% 
  distinct(user_id, apk, start_time, fg_time_ms, .keep_all = T) # filter out duplicates (as suggested by Ethica)

# get duration per media segment ------------------------------------------
# 783,206 obs.

# create duration per media segment based on start_time and fg_time_ms
apk_usage_dur <- apk_usage_time %>%
  group_by(user_id, apk, start_time) %>%
  arrange(last_used) %>% 
  mutate(duration = c(fg_time_ms[1], diff(fg_time_ms)) %>% milliseconds(),
         start = last_used - duration,
         end = last_used,
         duration = str_remove(duration, "[Ss]$") %>% as.double()) %>% 
  ungroup() %>%
  select(user_id, apk, start, duration, end) %>%
  arrange(user_id, start)


# filter ------------------------------------------------------------------
# 783,206 obs.

# filter negative and 0-durations, and include only study days
app_data <- apk_usage_dur %>% 
  filter(duration > 0) %>% # filter out durations lower than or equal to 0
  filter(as_date(end) >= "2020-06-01", as_date(start) <= "2020-06-23") %>% 
  mutate(start = ifelse(as_date(start) < as_date("2020-06-01"), 
                        as_datetime("2020-06-01 00:00:00"), start) %>% as_datetime,
         end = ifelse(as_date(end) > as_date("2020-06-23"), 
                      as_datetime("2020-06-23 23:59:59"), end) %>% as_datetime) %>% 
  arrange(user_id, start)


# change IDs --------------------------------------------------------------
# 778,544 obs.; N = 166

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
  select(-ID) %>% 
  arrange(user_id, start)


# bind with Google Playstore ---------------------------------------------
# 758,646 obs.; N = 155

# create function to scrape app name and category based on apk
if(F) { 
  source_python("scripts/google-play-scraper.py") # load scrape function from python script
  
  apk_lookup <- tibble(apk = unique(df$apk)) %>% # create df with Android packages (apks), app names, and app categories
    mutate(scraped_info = map(apk, ~scrape_app(.))) %>%
    unnest_wider(scraped_info)
  
  write_csv2(apk_lookup, "data/input/apk_lookup_data.csv") # save app_lookup data
} # this function does not work anymore, so I have copied the apk_lookup_data.csv from our previous research project

# read scraped apps and categories
apk_lookup <- read_csv2("data/input/apk_lookup_data.csv")

# define game categories
game_cats <- c("Action", "Adventure", "Arcade", "Board", "Card", "Casino",
               "Casual", "Educational", "Music", "Puzzle", "Racing",
               "Role Playing", "Simulation", "Strategy", "Trivia", "Word")

# merge app name and category with original dataset, based on apk and create general gaming category
df_app <- df %>% 
  left_join(apk_lookup) %>% 
  mutate(app_cat = ifelse(app_cat %in% game_cats, "Gaming", app_cat))


# inspect dataset including app names and categories ----------------------
# 758,646 obs.; N = 155

# how much observations came from Ethica itself?
df_app %>% 
  filter(str_detect(apk, "ethica")) %>% nrow() / nrow(df_app) * 100 # 2.85%

# show durations of apps that are not in Google playstore
df_app %>% 
  filter(is.na(app_name)) %>% 
  group_by(apk) %>% 
  summarise(sum_dur = sum(duration, na.rm = T)) %>% 
  arrange(desc(sum_dur))

# how much percent of the total tracking dataset (758,558 obs.)?
(1- (nrow(filter(df_app, !is.na(app_name))) / nrow(df_app))) * 100 # 16%
  

# save app usage dataset with app name and category -----------------------
# 758,646 obs.; N = 155

if(F) write_csv(df_app, "data/processed/df_app.csv")

