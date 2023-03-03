# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         datman_sleep paper.Rproj
# title:           ESM survey.R
# date created:    Mon Feb  6 09:15:29 2023
# last updated:    Fri Feb 10 13:32:36 2023
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# remove all variables (start from scratch)
rm(list = ls())

# load packages
library(tidyverse)
library(chron)
library(plotly)
library(kimisc)
library(lubridate)
library(glue)


# prepare ESM dataset -----------------------------------------------------

# read data and select onlyl relevant variables
esm2_raw <- read_csv2("input/cleaned esm data - wave2_no attrition.csv")
  
# select only relevant variables 
esm2_select <- esm2_raw %>% 
  transmute(
    ID,
    # Sex, BirthD, ETH01, ETH03, EduLevel, 
    # BeepNE2, DayNrE2, BeepTE2, 
    
    # time_sch = as_datetime(TmSchE2), # scheduled time
    time_res = as_datetime(TmResE2), # response time
    day_res = as_date(time_res), # response time
    
    weekday = ordered(WeekDE2, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")), 
    StateE2, DuratE2,
    
    sleep_qual = SLE01E2, # Hoe heb je vannacht geslapen? (helemaal niet lekker - heel lekker)
    bed_item = SLE02E2, # Hoe laat ging je gisteren naar bed?
    wake_item = SLE03E2, # Hoe laat was je vanmorgen wakker?
    
    nu_late = MPB01E2, # Hoe lang zat jij gisteren op je telefoon in het uur voordat je in slaap viel?
    nu_inter = MPN01E2) # Hoe lang zat jij vannacht op je telefoon tussen het slapen door?
    
# filter out rows with incompleted surveys
esm2_filter <- esm2_select %>% 
  filter(StateE2 %in% c("partially completed", "completed"),
         sleep_qual != 6666,
         nu_late != 9999,
         nu_inter != 9999) %>% 
  select(-StateE2)

# mutate bedtime and wakeup time variabels
esm2 <- esm2_filter %>%
  mutate(waketime = paste(day_res, format(wake_item, "%H:%M:%S")) %>% # Hoe laat was je vanmorgen wakker?
           as_datetime(), 
         bedtime = paste(day_res, format(bed_item, "%H:%M:%S")) %>% # Hoe laat ging je gisteren naar bed?
           as_datetime() %>% 
           ifelse(. > waketime, . - ddays(1), .) %>% 
           as_datetime()) %>% 
  relocate(bedtime, .before = waketime)

esm2


# filter out inappropriate responses --------------------------------------

# check whose bedtime was the same as their wakeup time
false1 <- filter(esm2, hours(bedtime) == hours(waketime))

# check whose survey response time aligns with the time they went to bed last night
false2 <- esm2 %>% 
  mutate(temp = paste(day_res, format(bed_item, "%H:%M:%S")) %>% as_datetime()) %>% 
  filter(time_res > temp & time_res < temp + dminutes(5)) %>% select(-temp)


# check whose wake-up times were more than 30 minutes later than the response time
false3 <- filter(esm2, waketime > time_res + dminutes(30))

# check whose wake-up time was before 1:00
false4 <- filter(esm2, hours(waketime) < 1)

# combine false rows 
false_rows <- bind_rows(false1, false2, false3, false4) %>% unique() %>% 
  select(ID, time_res, DuratE2, bed_item, wake_item, bedtime, waketime)

# filter out false rows
esm_no_false <- anti_join(esm2, false_rows)


# mutate data -------------------------------------------------------------

# add 12 hours when bedtime is between 8 and 19, and calculate sleep duration
df_esm <- esm_no_false %>% 
  mutate(bedtime = case_when(
    hours(bedtime) > 8  & hours(bedtime) < 15 ~ bedtime + dhours(12),
    TRUE ~ bedtime),
         s_duration = difftime(waketime, bedtime, units = "hour"))

# filter out people who slept more than 18 hours
df_esm <- filter(df_esm, !s_duration > 18) 


# variable descriptives ---------------------------------------------------

library(vtable)

df_esm %>% 
  select(sleep_qual, nu_late, nu_inter) %>% 
  vtable::sumtable()


# visualizations ----------------------------------------------------------

# bedtimes and wake-up times
df_plot <- df_esm %>% 
  mutate(bedtime_plot = glue("1970-01-01 {hours(bedtime)}:{minutes(bedtime)}:{seconds(bedtime)}") %>% as_datetime,
         bedtime_plot = ifelse(hours(bedtime_plot) < 17, bedtime_plot + dhours(24), bedtime_plot) %>% as_datetime,
         waketime_plot = glue("1970-01-01 {hours(waketime)}:{minutes(waketime)}:{seconds(waketime)}") %>% as_datetime)

df_plot %>% 
  ggplot() +
  geom_histogram(aes(x = bedtime_plot), 
                 fill = "maroon4", color = "white") +
  geom_histogram(aes(x = waketime_plot), 
                 fill = "#046f04", color = "white") +
  coord_polar(theta = "x", start = -.001) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%Hh", 
                   limits = c(as_datetime("1970-01-01 00:00:00"), 
                              as_datetime("1970-01-01 23:59:00"))) +
  labs(x = "Hour", y = "Count", title = "Bedtimes and Wake-up Times") +
  facet_wrap(~weekday, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6.5))
  

# sleep-interruptive use
ggplot(df_plot) + 
  geom_histogram(aes(nu_inter), fill = "darkgreen") + 
  labs(title = "sleep-interruptive use", x = "frequency")

# late-night use
ggplot(df_plot) + 
  geom_histogram(aes(nu_late), fill = "darkorange") + 
  labs(title = "late-night use", x = "time spent")

# bedtime
ggplot(df_plot) + 
  geom_density(aes(bedtime_plot, fill = weekday), alpha = .3, color = NA) +
  labs(title = "bedtime", x = "time") +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H")

# wake-up time
ggplot(df_plot) + 
  geom_density(aes(waketime_plot, fill = weekday), alpha = .3, color = NA) +
  labs(title = "wake-up time", x = "time") +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H")

# sleep duration
ggplot(df_plot) + 
  geom_density(aes(s_duration, fill = weekday), alpha = .3, color = NA) +
  labs(title = "sleep duration", x = "hours")

# sleep quality
ggplot(df_plot) + 
  geom_histogram(aes(sleep_qual, fill = weekday), 
                 position = "dodge", binwidth = 1) +
  labs(title = "sleep quality", x = "quality")


# export bedtimes ---------------------------------------------------------

df_plot %>% 
  mutate(waketime_plot = ifelse(waketime_plot < bedtime_plot, 
                                waketime_plot + ddays(1), 
                                waketime_plot) %>% 
           as_datetime) %>% 
  filter(ID %in% c(1:100)) %>% 
  ggplot() +
  geom_linerange(aes(xmin = bedtime_plot, xmax = waketime_plot, 
                     y = day_res, color = weekday), size = 1) +
  facet_wrap(~ ID)

if(F) df_plot %>% 
  select(ID, time_res, bedtime, waketime, sleep_qual, nu_late, nu_inter) %>% 
  write_csv2("input/sleepdata.csv")


