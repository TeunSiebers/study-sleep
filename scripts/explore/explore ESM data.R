# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           explore ESM data.R
#
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(vtable)


# read sleep data ---------------------------------------------------------

df_esm <- read_csv("data/processed/sleepdata.csv") %>% 
  mutate(day_res = time_res %>% as_date,
         weekday = weekdays(day_res)) %>% 
  mutate(s_duration = (bedtime %--% waketime) %>% as.duration())


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

# check individual bedtimes
df_plot %>% 
  mutate(waketime_plot = ifelse(waketime_plot < bedtime_plot, 
                                waketime_plot + ddays(1), 
                                waketime_plot) %>% 
           as_datetime) %>% 
  filter(ID %in% sample(unique(df_plot$ID), 20)) %>% 
  ggplot() +
  geom_linerange(aes(xmin = bedtime_plot, xmax = waketime_plot, 
                     y = day_res, color = weekday), size = 1) +
  facet_wrap(~ ID)
