# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           1.3 flatten tracking data.R
#
# description:
# This script is used to remove outliers, flatten overlapping app activities,
# define categories of app activities, and to finalize the tracking datasets
# for later main, exploratory, and sensitivity analyses.
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(valr) # for flattening intervals with bed_merge

# indicate whether or not to write files
W <- F


# define app categories ---------------------------------------------------

# social media
social_media <- c("Instagram", "WhatsApp Messenger", "Snapchat", "TikTok", 
                  "Twitter", "Facebook", "Messenger", "Reddit", 
                  "Discord: Talk, Chat & Hang Out")

# game apps are defined by: app_cat == "Gaming"

# video players and entertainment
video_ent <- c("YouTube", "Netflix", "Twitch: Live Game Streaming", 
               "Disney+", "Videoland", "Ziggo GO", "V LIVE", "HUAWEI Video", 
               "Pokémon TV", "MTV Play", "Amazon Prime Video", "NPO")


# read processed tracking data --------------------------------------------

# read data and set column types
df_app <- read_csv("data/processed/df_app.csv") %>% 
  mutate_at(vars(user_id, apk, app_name, app_cat), as.factor) %>% 
  mutate_at(vars(start, end), as_datetime) %>% 
  mutate(duration = as.numeric(duration)) %>% 
  arrange(user_id, start) %>% 
  filter(user_id %in% read.table("data/processed/user_ids.txt")$x) # 745,947 obs.; N = 155


# remove outliers ---------------------------------------------------------

# exclude app activities that lasted longer than the 0.1% longest app 
# activities in the Video and Gaming category (i.e., longer than 4.8 hours)
outlier_threshold <- df_app %>% 
  filter(app_cat == "Gaming" | app_name %in% video_ent) %>% 
  .$duration %>% 
  quantile(.999) # 17295.22 sec

# remove outliers from dataset
df_app_clean <- df_app %>% 
  filter(duration < outlier_threshold) # 745,706 obs.; N = 155


# visualize flattening ----------------------------------------------------

# There is overlap in the intervals of the app activities
# To avoid that multiple app activities occur at the same time, we flatten all
# app activities into general smartphone use activities.
# 745,706 obs.; N = 155

# create subsample to test flattening
temp_df <- df_app_clean %>% 
  filter(user_id %in% sample((df_app_clean$user_id), 10),
         duration < 12*60*60,
         start > as_datetime("2020-06-08 00:00:00"),
         end < as_datetime("2020-06-12 00:00:00"))

# compare original dataset with flat dataset:
# original 
if(F) temp_df %>% 
  ggplot() +
  geom_linerange(aes(xmin = start, xmax = end, y = user_id), size = 10)

# flat dataset
if(F) temp_df %>% 
  arrange(user_id, start) %>% 
  rename(chrom = user_id) %>% 
  bed_merge() %>%
  mutate_at(vars(start, end), as_datetime) %>% 
  mutate(duration = as.integer(end - start)) %>% 
  ggplot() +
  geom_linerange(aes(xmin = start, xmax = end, y = chrom), size = 10)


# apply flattening to all data --------------------------------------------

# create function to rename user_id to chrom and flatten the data
flatten <- function(...) {
  rename(..., chrom = user_id) %>% 
    bed_merge() %>%
    mutate_at(vars(start, end), as_datetime) %>% 
    mutate(duration = as.double(end - start)) %>% 
    ungroup() %>% 
    rename(user_id = chrom)
}

# flatten for overall smartphone use
phone <- df_app_clean %>% 
  flatten

if(W) write_csv(phone, "data/processed/main/phone.csv")


# exploratory analyses ----------------------------------------------------

# First filter all app activities that belong the specific category. Apply 
# flattening afterwards, so that there is no overlap in app activities within
# the same app category.

# social media apps
social <- df_app_clean %>% 
  filter(app_name %in% social_media) %>% 
  flatten

if(W) write_csv(social, "data/processed/exploratory/social.csv")

# game apps
game <- df_app_clean %>% 
  filter(app_cat == "Gaming") %>%
  flatten

if(W) write_csv(game, "data/processed/exploratory/game.csv")

# video player apps
video <- df_app_clean %>%
  filter(app_name %in% video_ent) %>%
  flatten

if(W) write_csv(video, "data/processed/exploratory/video.csv")


# sensitivity analyses ----------------------------------------------------

# smartphone including all outliers
if(W) df_app %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/phone_outliers.csv")

# social media apps including all outliers
if(W) df_app %>% 
  filter(app_name %in% social_media) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/social_outliers.csv")

# game apps including all outliers
if(W) df_app %>% 
  filter(app_cat == "Gaming") %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/game_outliers.csv")

# video plauer apps including all outliers
if(W) df_app %>% 
  filter(app_name %in% video_ent) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/video_outliers.csv")