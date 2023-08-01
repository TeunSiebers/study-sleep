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
# There is much overlap in the intervals of the app activities
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

# only include observations that lasted shorter than 24 hours
df_app %>% filter(duration >= 86399) # 46 obs.

phone_24h <- df_app %>% 
  filter(duration < 86399) %>% 
  flatten

if(W) write_csv(phone_24h, "data/processed/main/phone_24h.csv")

# only include observations that lasted shorter than 4.8 hours
# therefore, we will exclude the app activities that lasted longer than the 0.1% 
# longest app activities in the Video and Gaming category (i.e., longer than 4.8 hours)
cutoff <- df_app %>% 
  filter(app_cat %in% c("Video Players & Editors", "Gaming")) %>% 
  .$duration %>% 
  quantile(.999) # 17288.55 sec

df_app %>% filter(duration >= cutoff) # 258 obs.

phone_99th <- df_app %>% 
  filter(duration < cutoff) %>% 
  flatten

if(W) write_csv(phone_99th, "data/processed/main/phone_99th.csv")

# only include observations that last less than 4.8 hours and are recognized by the Google Playsgtore
df_app %>% filter(duration >= cutoff | is.na(app_name)) # 121,522 obs.

phone_99th_GP <- df_app %>% 
  filter(duration < cutoff, !is.na(app_name)) %>% 
  flatten

if(W) write_csv(phone_99th_GP, "data/processed/main/phone_99th_GP.csv")


# exploratory analyses ----------------------------------------------------

# social media ----
social_media <- c("Instagram", "WhatsApp Messenger", "Snapchat", "TikTok", 
                  "Twitter", "Facebook", "Messenger", "Reddit", 
                  "Discord: Talk, Chat & Hang Out")

social <- df_app %>% 
  filter(app_name %in% social_media) %>% 
  flatten

if(W) write_csv(social, "data/processed/exploratory/social.csv")

social_24h <- df_app %>% 
  filter(app_name %in% social_media, 
         duration < 86399) %>% 
    flatten

if(W) write_csv(social_24h, "data/processed/exploratory/social_24h.csv")

social_99th <- df_app %>% 
  filter(app_name %in% social_media, 
         duration < cutoff) %>% 
    flatten

if(W) write_csv(social_99th, "data/processed/exploratory/social_99th.csv")


# gaming ----
game <- df_app %>% 
  filter(app_cat == "Gaming") %>%
  flatten

if(W) write_csv(game, "data/processed/exploratory/game.csv")

game_24h <- df_app %>% 
  filter(app_cat == "Gaming",
         duration < 86399) %>% 
  flatten

if(W) write_csv(game_24h, "data/processed/exploratory/game_24h.csv")

game_99th <- df_app %>% 
  filter(app_cat == "Gaming",
         duration < cutoff) %>% 
  flatten

if(W) write_csv(game_99th, "data/processed/exploratory/game_99th.csv")


# video players ----
video <- df_app %>% 
  filter(app_cat == "Video Players & Editors") %>%
  flatten

if(W) write_csv(video, "data/processed/exploratory/video.csv")

video_24h <- df_app %>% 
  filter(app_cat == "Video Players & Editors",
         duration < 86399) %>% 
  flatten

if(W) write_csv(video_24h, "data/processed/exploratory/video_24h.csv")

video_99th <- df_app %>% 
  filter(app_cat == "Video Players & Editors",
         duration < cutoff) %>% 
  flatten

if(W) write_csv(video_99th, "data/processed/exploratory/video_99th.csv")


# music ----
music <- df_app %>% 
  filter(app_cat == "Music & Audio") %>%
  flatten

if(W) write_csv(music, "data/processed/exploratory/music.csv")

music_24h <- df_app %>% 
  filter(app_cat == "Music & Audio",
         duration < 86399) %>% 
  flatten

if(W) write_csv(music_24h, "data/processed/exploratory/music_24h.csv")

music_99th <- df_app %>% 
  filter(app_cat == "Music & Audio",
         duration < cutoff) %>% 
  flatten

if(W) write_csv(music_99th, "data/processed/exploratory/music_99th.csv")

