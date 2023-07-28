# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           1.2 create timeframes.R
#
# description:
# ...
#
# bt_yes = bedtime yesterday
# wt_today = wake-up time today
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(lubridate)


# read data ---------------------------------------------------------------

# read sleep data (ESM surveys)
# N = 156; 1,888 obs.
sleep <- read_csv("data/processed/sleepdata.csv") %>% 
  mutate(date_res = as_date(time_res), .before = "time_res",
         weekday = strftime(date_res, '%a')) %>% 
  select(!matches("correct_"))


# add yesterday's wake-up times -------------------------------------------
# 1,888; N = 156

# transfer wake-up times to same day as bedtimes
sleep_df <- sleep %>% 
  complete(user_id, date_res) %>% 
  arrange(user_id, date_res) %>% 
  group_by(user_id) %>%
  mutate(wt_yes = lag(wt_today,  order_by = user_id), .before = bt_yes) %>% 
  mutate(weekend = chron::is.weekend(date_res),
         weekend_yes = chron::is.weekend(date_res - days(1))) %>% 
  filter(!is.na(sleep_qual)) # filter out missing values based on survey responses (and get original)


# missing data imputation -------------------------------------------------
# 1,888 obs.; N = 156

# We now have 3,432 obs. of which many contain missing values for wake-up times
# or bedtimes. Therefore, we will estimate the wake-up as one SD below the 
# person's average wake-up time, separated between weekdays and weekend days. 
# We opted for 1 SD below the person's average because smartphone activities 
# around the person's mean probably indicate the start of a new day, and therefore, 
# all activities just before the average wake-up time should not be categorized as
# post-bedtime use.
wt_estimates_num <- sleep_df %>% 
  transmute(user_id, weekend, wt_today) %>% 
  group_by(user_id, weekend) %>% 
  summarise(n = n(), 
            mean = mean(hour(wt_today) + (minute(wt_today)/60), na.rm = T),
            sd = ifelse(n == 1, 0, sd(hour(wt_today) + 
                                        (minute(wt_today)/60), na.rm = T))) %>% 
  mutate(wt_yes_est = mean - sd)

# create dataset with averages per weekday/weekend day and per person
wt_estimates <- wt_estimates_num %>% 
  transmute(user_id, weekend,
            wt_est = paste0(sprintf("%02g", wt_yes_est %/% 1), 
                            ":", 
                            sprintf("%02g", (wt_yes_est %% 1 * 60) %/% 1), 
                            ":00"))

wt_estimates_yes <- wt_estimates %>% 
  rename(weekend_yes = weekend, wt_yes_est = wt_est)

# merge the dataset with averages with sleep dataframe
# fill missing values of yesterdays wake-up time
full_sleep_df <- sleep_df %>% 
  left_join(wt_estimates_yes) %>% 
  left_join(wt_estimates) %>% 
  # create new datetime estimates for every row
  mutate(wt_yes_est = paste(as.character(date_res - days(1)), " ", wt_yes_est) %>% 
           as_datetime(),
         wt_est = paste(as.character(date_res), " ", wt_est) %>% 
           as_datetime()) %>% 
  # fill the missing values for wake-up times with the estimated wake-up times
  mutate(wt_today = ifelse(is.na(wt_today), wt_est, wt_today) %>% as_datetime,
         wt_yes = ifelse(is.na(wt_yes), wt_yes_est, wt_yes) %>% as_datetime) %>% 
  filter(!wt_yes > bt_yes) %>% # wake-up time yesterday cannot be after bedtime yesterday
  # remove the unnecessary variables
  select(-weekend, -weekend_yes, -wt_yes_est, -wt_est) %>% 
  relocate(weekday, .after = date_res)

# We now have a dataset from which the three timeframes can be creates per 
# person per day, in which all the missed values for wake-up times are filled
# with averages of that person per weekday/ weekend day.
full_sleep_df


# create timeframes per person per day ------------------------------------

# create timeframes per participant, per day
timeframes <- full_sleep_df %>% 
  mutate(hbefore_bt_yes = bt_yes - hours(1), .before = bt_yes) %>% 
  na.omit()

# inspect timeframes data
if(F) ggplot(data = timeframes) + 
  geom_linerange(aes(xmin = wt_yes, xmax = hbefore_bt_yes, y = user_id), color = "red1") + 
  geom_linerange(aes(xmin = hbefore_bt_yes, xmax = bt_yes, y = user_id), color = "blue1") + 
  geom_linerange(aes(xmin = bt_yes, xmax = wt_today, y = user_id), color = "green2")


# save timeframes ---------------------------------------------------------

if(F) write_csv(timeframes, "data/processed/timeframes.csv")


