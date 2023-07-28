# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           1.1 prepare esm data.R
#
# description:
# This script reads the sleep data from the ESM, filters out missings, and 
# cleans the sleep data (by filtering out incorrect responses and mutating
# unlikely responses).
#
# bt_yes = bedtime yesterday
# wt_today = wake-up time today
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(chron)
library(plotly)
library(kimisc)
library(lubridate)
library(glue)


# prepare ESM dataset -----------------------------------------------------

# read data and select onlyl relevant variables
esm2_raw <- read_csv2("data/input/cleaned esm data - wave2_no attrition.csv")

# select only relevant variables 
esm2_select <- esm2_raw %>% 
  transmute(
    user_id = ID,
    # Sex, BirthD, ETH01, ETH03, EduLevel, 
    # BeepNE2, DayNrE2, BeepTE2, 
    
    # time_sch = as_datetime(TmSchE2), # scheduled time
    time_res = as_datetime(TmResE2), # response time
    day_res = as_date(time_res), # response day
    
    weekday = ordered(WeekDE2, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")), 
    StateE2, DuratE2,
    
    sleep_qual = SLE01E2, # Hoe heb je vannacht geslapen? (helemaal niet lekker - heel lekker)
    bed_item = SLE02E2, # Hoe laat ging je gisteren naar bed?
    wake_item = SLE03E2) # Hoe laat was je vanmorgen wakker?


# filter out rows (non-tracking & incompleted surveys) --------------------
# N = 312; 39,312 obs.

# get a list of participants whose smartphone use was tracked (N = 160)
participants <- unique(read_csv("data/processed/cleaned_logdata.csv")$user_id)

# only keep participants whose smartphone activities were tracked
esm2_filter1 <- esm2_select %>% 
  filter(user_id %in% participants) # --> N = 160; 20,160 obs.

# filter our expired surveys and all surveys other than morning surveys
esm2_filter <- esm2_filter1 %>% 
  filter(StateE2 %in% c("partially completed", "completed"),
         sleep_qual != 6666) %>% 
  select(-StateE2) # -->  N = 156; 1,953 obs.


# In a few instances, participants confused bedtime and wake-up time. 
# If bedtime is before 12 and wake-up time after 12 --> switch values

# create function to rewrite time class to numeric
num_time <- function(x) {
  hour(x) + minute(x)/60
  }

# show instances where bt and wt should be swapped
esm2_filter %>%
  filter(num_time(bed_item) > 0 & num_time(bed_item) < 12, 
         num_time(wake_item) > 12 & num_time(wake_item) < 24)

# create function to swap bt and wt
swap_to_wt <- function(bt, wt) {
  bt_num <- num_time(bt)
  wt_num <- num_time(wt)
  
  ifelse((bt_num > 0 & bt_num < 12) & (wt_num > 12 & wt_num < 24), wt, bt)
}

swap_to_bt <- function(bt, wt) {
  bt_num <- as.numeric(str_extract(as.character(bt), "[0-9]+"))
  wt_num <- as.numeric(str_extract(as.character(wt), "[0-9]+"))
  
  ifelse((bt_num > 0 & bt_num < 12) & (wt_num > 12 & wt_num < 24), bt, wt)
}

# apply functions
esm2_swapped <- esm2_filter %>% 
  mutate(bed_item_temp = bed_item,
         bed_item = swap_to_wt(bed_item, wake_item) %>% 
           as_datetime() %>% format("%H:%M"),
         wake_item = swap_to_bt(bed_item_temp, wake_item) %>% 
           as_datetime() %>% format("%H:%M")) %>% 
  select(-bed_item_temp)


# change time responses (hours and minutes) into datetime formats ---------

# Participants responded to the survey items 'bt_yes' and 'wake-up time' by
# indicating only hours and minutes. The answers should be changed into datetimes 
# to make it useful for the analyses. Therefore, below, we added the date of the 
# response time to both the bt_yess and wake-up times. If the datetime format of 
# the bt_yes comes after wake-up time, one day is subtracted from the bt_yess.
# This procedures account for the fact that some bt_yess were after midnight,
# which would otherwise mess up the dates, causing incorrect datetimes.

# so: mutate bt_yes and wake-up time variables
esm2 <- esm2_swapped %>%
  mutate(wt_today = paste0(as.character(day_res), " ", wake_item, ":00") %>% as_datetime(), # Hoe laat was je vanmorgen wakker?
         bt_yes_temp = paste0(as.character(day_res), " ", bed_item, ":00") %>% as_datetime(), # Hoe laat ging je gisteren naar bed?
         bt_yes = ifelse(hours(bt_yes_temp) > hours(time_res), 
                         bt_yes_temp - ddays(1), 
                         bt_yes_temp) %>%
           as_datetime()) %>% 
  relocate(bt_yes, .before = wt_today)


# filter sleep data -------------------------------------------------------
# N = 156; 1,953 obs.

# This data on bt_yes and wake-up time allow us to filter out inappropriate 
# responses.

# Incorrect default, based on response time:
# survey response time are about equal to bedtime and wake-up time
false <- esm2 %>% 
  filter(bt_yes_temp < time_res + dminutes(3) & bt_yes_temp > time_res - dminutes(3) |
         bt_yes_temp < wt_today + dminutes(3) & bt_yes_temp > wt_today - dminutes(3)) # 65 obs.

# categorize correctness of answer responses
esm2_cat <- esm2 %>% 
  anti_join(false) %>% 
  mutate(
    correct_bedtime = case_when(
      hours(bt_yes) >= 0 & hours(bt_yes) < 6 ~ "bedtime after midnight",
      hours(bt_yes) >= 6 & hours(bt_yes) < 13 ~ "12-24h clock confused",
      hours(bt_yes) >= 13 & hours(bt_yes) < 18 ~ "incorrect", # no possible bedtime
      hours(bt_yes) >= 18 & hours(bt_yes) < 24 ~ "bedtime before midnight"),
    correct_waketime = case_when(
      hours(wt_today) >= 0 & hours(wt_today) < 4 ~ "incorrect", # morning has not started
      hours(wt_today) >= 4 & hours(wt_today) <= hours(time_res) ~ "correct", 
      wt_today >= time_res + dminutes(5) & hours(wt_today) < 24 ~ "incorrect") # cannot complete survey while sleeping
  )

# recover 12-24h clock confusion by adding 12h to bedtimes, and set NA to incorrect responses
esm2_df <- esm2_cat %>% 
  mutate(
    bt_yes = case_when(
      correct_bedtime == "12-24h clock confused" & hour(time_res) > as.numeric(str_extract(bed_item, "[0-9][0-9]")) ~ bt_yes - dhours(12), 
      correct_bedtime == "12-24h clock confused" & hour(time_res) <= as.numeric(str_extract(bed_item, "[0-9][0-9]")) ~ bt_yes + dhours(12), 
      correct_bedtime == "incorrect" ~ NA,
      TRUE ~ bt_yes),
    bt_yes = case_when(
      bt_yes > wt_today ~ bt_yes - ddays(1), # correction for 12-24h clock confusion when bed_item is before wake_item
      TRUE ~ bt_yes),
    wt_today = case_when(
      correct_waketime == "incorrect" ~ NA,
      TRUE ~ wt_today),
    sleep_duration = difftime(wt_today, bt_yes, units = "hours")
  )


# visualize data ----------------------------------------------------------

# bt_yess and wake-up times
if(F) esm2_df %>%
  ggplot() +
  geom_histogram(aes(x = num_time(bt_yes)),
                 fill = "maroon4", color = "white") +
  geom_histogram(aes(x = num_time(wt_today)),
                 fill = "#046f04", color = "white") +
  coord_polar(theta = "x", start = -.001) +
  labs(x = "Hour", y = "Count", title = "bt_yess and Wake-up Times") +
  facet_wrap(~weekday, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6.5))

# bt_yes
if(F) ggplot(esm2_df) + 
  geom_histogram(aes(x = hour(bt_yes) + (minute(bt_yes)/60), fill = weekday), 
                 position = "dodge", color = NA) +
  labs(title = "bt_yes", x = "time") +
  scale_x_continuous(breaks = seq(0, 24, 1))

# wake-up time
if(F) ggplot(esm2_df) + 
  geom_histogram(aes(x = hour(wt_today) + (minute(wt_today)/60), fill = weekday), 
                 position = "dodge", color = NA) +
  labs(title = "wt_today", x = "time") +
  scale_x_continuous(breaks = seq(0, 24, 1))

# sleep duration
if(F) ggplot(esm2_df) + 
  geom_histogram(aes(x = sleep_duration, fill = weekday), 
                 position = "dodge", color = NA) +
  labs(title = "sleep duration", x = "time") +
  scale_x_continuous(breaks = seq(0, 24, 1))

# sleep quality
if(F) ggplot(esm2_df) + 
  geom_histogram(aes(sleep_qual, fill = weekday), 
                 position = "dodge", binwidth = 1) +
  labs(title = "sleep quality", x = "quality")

# sleep pattern of subsample
if(F) esm2_df %>% 
  filter(user_id %in% esm2_df$user_id) %>% 
  ggplot() +
  geom_linerange(aes(xmin = bt_yes, xmax = wt_today, y = factor(user_id), 
                     color = weekday), size = 0.7) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "day") +
  theme(axis.text.y = element_text(size = 3))


# save processed sleep data -----------------------------------------------
# N = 156; 1,888 obs.

if(F) esm2_df %>% 
  select(user_id, time_res, bt_yes, wt_today, sleep_qual, weekday, 
         correct_bedtime, correct_waketime) %>% 
  write_csv("data/processed/sleepdata.csv")
