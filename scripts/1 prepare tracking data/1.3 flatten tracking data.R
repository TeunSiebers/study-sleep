# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           1.3 flatten tracking data.R
#
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(valr) # for flattening intervals with bed_merge

# indicate whether or not to write files
W <- F


# read processed tracking data --------------------------------------------

# read data and set column types
df_app <- read_csv("data/processed/df_app.csv") %>% 
  mutate_at(vars(user_id, apk, app_name, app_cat), as.factor) %>% 
  mutate_at(vars(start, end), as_datetime) %>% 
  mutate(duration = as.numeric(duration)) %>% 
  arrange(user_id, start) # 758,558 obs.; N = 155


# visualize flattening ----------------------------------------------------

# There is overlap in the intervals of the app activities
# To avoid that multiple app activities occur at the same time, we flatten all
# app activities into general smartphone use activities.
# 758,558 obs.; N = 155

# create subsample to test flattening
temp_df <- df_app %>% 
  filter(user_id %in% sample((df_app$user_id), 10),
         duration < 12*60*60,
         start > as_datetime("2020-06-08 00:00:00"),
         end < as_datetime("2020-06-12 00:00:00"))

# compare original dataset with flat dataset
if(F) temp_df %>% 
  ggplot() +
  geom_linerange(aes(xmin = start, xmax = end, y = user_id), size = 10)

if(F) temp_df %>% 
  arrange(user_id, start) %>% 
  rename(chrom = user_id) %>% 
  bed_merge() %>%
  mutate_at(vars(start, end), as_datetime) %>% 
  mutate(duration = as.integer(end - start)) %>% 
  ggplot() +
  geom_linerange(aes(xmin = start, xmax = end, y = chrom), size = 10)


# apply flattening: all data ----------------------------------------------

# create function to rename user_id,
flatten <- function(...) {
  rename(..., chrom = user_id) %>% 
    bed_merge() %>%
    mutate_at(vars(start, end), as_datetime) %>% 
    mutate(duration = as.double(end - start)) %>% 
    ungroup() %>% 
    rename(user_id = chrom)
}

# flatten for overall smartphone use
phone <- df_app %>% 
  flatten

if(W) write_csv(phone, "data/processed/main/phone.csv")


# exploratory analyses ----------------------------------------------------

# social media
social_media <- c("Instagram", "WhatsApp Messenger", "Snapchat", "TikTok", 
                  "Twitter", "Facebook", "Messenger", "Reddit", 
                  "Discord: Talk, Chat & Hang Out")

social <- df_app %>% 
  filter(app_name %in% social_media) %>% 
  flatten

if(W) write_csv(social, "data/processed/exploratory/social.csv")

# gaming
game <- df_app %>% 
  filter(app_cat == "Gaming") %>%
  flatten

if(W) write_csv(game, "data/processed/exploratory/game.csv")

# video players and entertainment
video_ent <- c("YouTube", "Netflix", "Twitch: Live Game Streaming", 
               "Disney+", "Videoland", "Ziggo GO", "V LIVE", "HUAWEI Video", 
               "Pokémon TV", "MTV Play", "Amazon Prime Video", "NPO")

video <- df_app %>%
  filter(app_name %in% video_ent) %>%
  flatten

if(W) write_csv(video, "data/processed/exploratory/video.csv")


# sensitivity analyses ----------------------------------------------------

# exclude app activities that lasted 24 hours or more
one_day <- 86399

# exclude app activities that lasted longer than the 0.1% longest app 
# activities in the Video and Gaming category (i.e., longer than 4.8 hours)
cutoff <- df_app %>% 
  filter(app_cat == "Gaming" | app_name %in% video_ent) %>% 
  .$duration %>% 
  quantile(.999) # 17295.22 sec

# for smartphone use, also exclude app activities that lasted longer than 4.8 
# hours or those that are not recognized by the Google Playstore

# smartphone (main  analyses)
if(W) df_app %>% 
  filter(duration < one_day) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/phone_24h.csv")

if(W) df_app %>% 
  filter(duration < cutoff) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/phone_99th.csv")

if(W) df_app %>% 
  filter(duration < cutoff, !is.na(app_name)) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/phone_99th_GP.csv")


# social media  (exploratory) 
if(W) df_app %>% 
  filter(app_name %in% social_media, duration < one_day) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/social_24h.csv")

if(W) df_app %>% 
  filter(app_name %in% social_media, duration < cutoff) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/social_99th.csv")


# gaming  (exploratory)
if(W) df_app %>% 
  filter(app_cat == "Gaming", duration < one_day) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/game_24h.csv")

if(W) df_app %>% 
  filter(app_cat == "Gaming", duration < cutoff) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/game_99th.csv")


# video players (exploratory)
if(W) df_app %>% 
  filter(app_name %in% video_ent, duration < one_day) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/video_24h.csv")

if(W) df_app %>% 
  filter(app_cat == "Video Players & Editors", duration < cutoff) %>% 
  flatten %>% 
  write_csv("data/processed/sensitivity/video_99th.csv")

