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
(flat_phone <- df_app %>% 
  flatten)

if(F) write_csv(flat_phone, "data/processed/log_main.csv")

# and for exploratory analyses:
# flatten per app category
(flat_appcat <- df_app %>% 
  group_by(app_cat) %>% 
  flatten)

# flatten per app
(flat_app <- df_app %>% 
  group_by(app_name) %>% 
  flatten)


# sensitivity analyses ----------------------------------------------------

# 1. 24 hours ----
# only include observations that lasted shorter than 24 hours
df_app %>% filter(duration >= 86399) # 46 obs.

(sens_24h_flat_phone <- df_app %>% 
  filter(duration < 86399) %>% 
  flatten)

if(F) write_csv(sens_24h_flat_phone, "data/processed/log_sens_24h.csv")

# 2. 4.8 hours ---- 
# only include observations that lasted shorter than 4.8 hours
# therefore, we will exclude the app activities that lasted longer than the 0.1% 
# longest app activities in the Video and Gaming category (i.e., longer than 4.8 hours)
cutoff <- df_app %>% 
  filter(app_cat %in% c("Video Players & Editors", "Gaming")) %>% 
  .$duration %>% 
  quantile(.999) # 17288.55 sec

df_app %>% filter(duration >= cutoff) # 258 obs.

(sens_99_flat_phone <- df_app %>% 
  filter(duration < cutoff) %>% 
  flatten)

if(F) write_csv(sens_99_flat_phone, "data/processed/log_sens_99th.csv")

# 3. 4.8 hours and GP store ----
# only include observations that last less than 4.8 hours and are recognized by the Google Playsgtore
df_app %>% filter(duration >= cutoff | is.na(app_name)) # 121,522 obs.

(sens_playstore_sens_99_flat_phone <- df_app %>% 
  filter(duration < cutoff, !is.na(app_name)) %>% 
  flatten)

if(F) write_csv(sens_playstore_sens_99_flat_phone, "data/processed/log_sens_99th_GP.csv")

