# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           1.2 prepare tracking data.R
#
# description:
# This script merges the three processed chunks from the raw datafile. It 
# adds two hours to compensate for the different timezone, and generates 
# durations per app activity and filters out all app activities that last 0 
# sec or less. Finally, it changes the Ethica IDs into participants IDs via
# the keyfile. 
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(lubridate)
library(reticulate)
library(vroom)


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
         start_segment = last_used - duration,
         end_segment = last_used,
         duration = str_remove(duration, "[Ss]$") %>% as.double()) %>% 
  ungroup() %>%
  select(user_id, apk, start_segment, duration, end_segment) %>%
  arrange(user_id, start_segment)


# filter ------------------------------------------------------------------
# 783,206 obs.

# filter negative and 0-durations, and include only study days
app_data <- apk_usage_dur %>% 
  filter(duration > 0) %>% # filter out durations lower than or equal to 0
  mutate(day = as_date(start_segment)) %>% # filter only data from start to end of the study
  filter(day >= "2020-06-01", day <= "2020-06-23")


# change IDs --------------------------------------------------------------
# 778,456 obs.

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


# filter out apps that are not in the Google Playstore --------------------
# 758,558 obs.

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

# show apps that are not in Google playstore
df_app %>% 
  filter(is.na(app_name)) %>% 
  group_by(apk) %>% 
  summarise(sum_dur = sum(duration, na.rm = T)) %>% 
  arrange(desc(sum_dur))

# filter out activities of apps that are not recognized by Google Playstore
df_Gapp <- filter(df_app, !is.na(app_name))

# how much percent of the total tracking dataset (758,558 obs.)?
1- nrow(df_Gapp)/nrow(df_app) # 16%


# filter out outliers -----------------------------------------------------
# 637,178 obs. 

# some app activities are extremely large:
df_Gapp %>% arrange(desc(duration))

# therefore, we will exclude the app activities that lasted longer than the 0.1% 
# longest app activities in the Video and Gaming category (i.e., longer than 4.8 hours)
cutoff <- df_Gapp %>% 
  filter(app_cat %in% c("Video Players & Editors", "Gaming")) %>% 
  .$duration %>% 
  quantile(.999)

df_Gapp_out <- filter(df_Gapp, !duration > cutoff)

# how much percent of the reduced tracking dataset (637,178 obs.)?
1- nrow(df_Gapp_out)/nrow(df_Gapp) # 0.02%


# filter out overlap in app activities ------------------------------------
# 636,846 obs.

# cut off end_segment times if next app activity starts
df_final <- df_Gapp_out %>% 
  group_by(user_id) %>% 
  arrange(user_id, start_segment) %>% 
  mutate(end_segment = ifelse(lead(start_segment) < end_segment, 
                              lead(start_segment), 
                              end_segment) %>% as_datetime) %>% 
  mutate(duration = as.numeric(end_segment-start_segment)) %>% 
  filter(duration > 0) %>% 
  arrange(user_id, start_segment)

# how much observations came from Ethica itself?
filter(df_final, str_detect(apk, "ethica")) %>% nrow() / nrow(df_final) # 3.39%


# save final dataset ------------------------------------------------------
# 615,075 obs.

# save final dataset as .csv
if(F) write_csv(df_final, "data/processed/cleaned_logdata.csv")





