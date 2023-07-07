# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           explore logdata.R
#
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(lubridate)
library(scales)
library(reticulate)


# read logdata ------------------------------------------------------------

df <- read_csv("data/processed/cleaned_logdata.csv")

# find extreme values
if(F) df %>% 
  arrange(desc(duration)) %>%
  ggplot()+
  geom_histogram(aes(x = duration), binwidth = 500) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

if(F) df %>% 
  filter(duration > 3600*4) %>% 
  arrange(desc(duration)) %>% 
  select(user_id, apk, day) %>% unique() %>% write_csv("data/long_durations.csv")



sm <- "snapchat|whatsapp|tiktok|facebook|instagram|discord|twitter|reddit|twitch|youtube|messages"

# df_app_game %>% 
#   mutate(social_media = ifelse(is.na(app_name), "no",
#                                ifelse(str_detect(tolower(app_name), sm), "yes", "no"))) %>% 
#   group_by(social_media) %>% 
#   summarize(duration =  duration(sum(duration)),
#             duration_perc = 
#             frequency = n(),
#             frequency_perc = )


# scrape Google playstore appnames and categories -------------------------

# create function to scrape app name and category based on apk
if(F) { 
  source_python("scripts/google-play-scraper.py") # load scrape function from python script
  
  apk_lookup <- tibble(apk = unique(df$apk)) %>% # create df with Android packages (apks), app names, and app categories
    head(n = 20) %>% 
    mutate(scraped_info = map(apk, ~scrape_app(.))) %>%
    unnest_wider(scraped_info)
  
    write_csv2(apk_lookup, "data/input/apk_lookup_data.csv") # save app_lookup data
} # this function does not work anymore, so I have copied the apk_lookup_data.csv from our previous research project

# read scraped apps and categories
apk_lookup <- read_csv2("data/input/apk_lookup_data.csv")

# merge app name and category with original dataset, based on apk
df_app <- df %>% left_join(apk_lookup)


# merge gaming categories -------------------------------------------------

# define game categories
game_cats <- c("Action", "Adventure", "Arcade", "Board", "Card", "Casino",
               "Casual", "Educational", "Music", "Puzzle", "Racing",
               "Role Playing", "Simulation", "Strategy", "Trivia", "Word")

# change specific gaming categories in general gaming category
df_app_game <- df_app %>% 
  mutate(app_cat = ifelse(app_cat %in% game_cats, "Gaming", app_cat))


# manually add info -------------------------------------------------------

# add system apps (_ as manual indicator)
system_apps <- c("launcher|(?<=\\.)home", "settings", "camera", "documents", 
                 "calendar", "contacts", "email", "phone|telecom|dialer",
                 "calculator", "clock", "packageinstaller", "compass", 
                 "weather", "systemui", "gallery", "vending", "incallui",
                 "wssyncmldm") %>% 
  paste(collapse = '|')

# add systems
systems <- c("android", "samsung", "huawei", "lge", "motorola", "oneplus", "miui") %>% 
  paste(collapse = '|')

# define launchers
launchers <- str_subset(unique(str_subset(df_app_game$apk, "\\.launcher|\\.home")), systems)

df_app_game <- df_app_game %>% 
  mutate(
    app_name = case_when(
      is.na(app_name) & str_detect(apk, system_apps) ~ paste0("_", str_extract_all(apk, system_apps)),
      TRUE ~ app_name),
    app_name = case_when(
      app_name == "_telecom" | app_name == "_phone" ~ "_dialer",
      app_name == "_wssyncmldm" ~ "_softwareupdater",
      apk %in% launchers ~ "_launcher",
      TRUE ~ app_name),
    app_cat = case_when(
      apk %in% launchers ~ "_launcher",
      is.na(app_cat) & str_detect(apk, systems) ~ "_system",
      TRUE ~ app_cat)
  )

# undefined system apps
df_app_game %>% 
  filter(is.na(app_name), str_detect(apk, systems)) %>% 
  group_by(apk) %>%
  count() %>%
  arrange(desc(n))

# inspect system apps
df_app_game %>% 
  filter(app_cat == "_system") %>% 
  group_by(app_name) %>%
  count() %>%
  arrange(desc(n))


# visualize ---------------------------------------------------------------

df_app_game %>% 
  filter(user_id %in% tail(unique(df_app_game$user_id))) %>%
  mutate(user_id = as.factor(user_id)) %>% 
  ggplot() + 
  geom_linerange(mapping = aes(xmin = start_segment, xmax = end_segment, y = user_id, color = app_cat),
                 size = 16)

# define nights
dateRanges <- tibble(
  start = seq(as.POSIXct("2020-06-01 22:30:00"), as.POSIXct("2020-06-21 22:30:00"), "1 day"),
  end = seq(as.POSIXct("2020-06-02 07:30:00"), as.POSIXct("2020-06-22 07:30:00"), "1 day")
)

df_app_game %>% 
  filter(user_id %in% head(unique(df_app_game$user_id), n = 30)) %>%
  filter(duration > 36000) %>% 
  #filter(app_name != "_systemui") %>%
  #filter(app_cat != "_system") %>%
  #filter(day > "2020-06-10") %>%
  mutate(user_id = as.factor(user_id)) %>% 
  ggplot() + 
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  geom_linerange(mapping = aes(xmin = start_segment, xmax = end_segment, y = user_id, color = app_name),
                 size = 8) +
  scale_x_datetime(limits = c(as_datetime("2020-06-01 22:30:00"), as_datetime("2020-06-07 22:30:00")), 
                   date_breaks = "1 day")


# add sleep data ----------------------------------------------------------

# read sleep data
sleep <- read_csv2("data/processed/sleepdata.csv") %>% 
  mutate(date_res = as_date(time_res)) %>% 
  rename(user_id = ID)

# how a full dataframe looks like
full_sleep_df <- crossing(user_id = unique(sleep$user_id), 
         date_res = seq(as_date("2020-06-02"), as_date("2020-06-23"), 1))

# merge existing sleep data with full sleep df to generate missing rows
sleep_df <- full_join(sleep, full_sleep_df) %>% arrange(user_id, date_res) %>% 
  # transfer wake-up times to same day as bedtimes
  group_by(user_id) %>%
  mutate(waketime_sameday = lag(waketime,  order_by = user_id), .before = bedtime)

sleep_df <- sleep_df %>% 
  mutate(sleep_time = bedtime %--% waketime,
         daytime = waketime_sameday %--% bedtime,
         prebedtime_hour = (bedtime - dhours(1)) %--% bedtime,
         postbedtime_hour = bedtime %--% (bedtime + dhours(1)))


# smartphone use within one hour before bedtime 
df_daytime <- map_df(
  unique(df_app_game$user_id),
  function(x) {
    daytime_times <- sleep_df %>%
      filter(user_id == x) %>%
      .$daytime %>%
      as.list()
    
    df_app_game %>%
      filter(user_id == x,
             start_segment %within% daytime_times) %>% 
      mutate(end_segment = ifelse(end_segment > bedtime, bedtime, end_segment))
    })

sm <- "snapchat|whatsapp|tiktok|facebook|instagram|discord|twitter|reddit|twitch|youtube|messages"

df_daytime %>% 
  mutate(sm_or_not = ifelse(str_detect(tolower(app_name), sm), "sm", "no sm")) %>% 
  group_by(sm_or_not) %>% 
  summarise(duration =  duration(sum(duration)),
                        frequency = n())

# smartphone use within one hour before bedtime 
df_prebed <- map_df(
  unique(df_app_game$user_id),
  function(x) {
    prebed_times <- sleep_df %>%
      filter(user_id == x) %>%
      .$prebedtime_hour %>%
      as.list()
    
    df_app_game %>%
      filter(user_id == x,
             start_segment %within% prebed_times) 
    })

df_prebed %>% 
  mutate(sm_or_not = ifelse(str_detect(tolower(app_name), sm), "sm", "no sm")) %>% 
  group_by(sm_or_not) %>% 
  summarise(duration =  duration(sum(duration)),
            frequency = n())

# smartphone use within one hour after bedtime 
df_postbed <- map_df(
  unique(df_app_game$user_id),
  function(x) {
    postbed_times <- sleep_df %>%
      filter(user_id == x) %>%
      .$postbedtime_hour %>%
      as.list()
    
    df_app_game %>%
      filter(user_id == x,
             start_segment %within% postbed_times) 
  })

df_postbed %>% 
  mutate(sm_or_not = ifelse(str_detect(tolower(app_name), sm), "sm", "no sm")) %>% 
  group_by(sm_or_not) %>% 
  summarise(duration =  duration(sum(duration)),
            frequency = n())

df_sleeptime %>% 
  group_by(user_id) %>%
  summarize(mean_dur = mean(duration, na.rm = T)) %>% 
  arrange(desc(mean_dur)) %>% 
  ggplot() +
  geom_density(aes(x = mean_dur))


df_nighttime %>% 
  group_by(user_id) %>%
  summarize(mean_dur = mean(duration, na.rm = T)) %>% 
  arrange(desc(mean_dur)) %>% 
  ggplot() +
  geom_density(aes(x = mean_dur))


df_sleeptime %>% 
  group_by(app_name) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  head() %>% 
  ggplot() +
  geom_bar(aes(x = app_name, y = n), stat = "identity")


ggplot() + 
  geom_linerange(data = sleep %>% 
                   filter(user_id %in% c(1:50), 
                          bedtime > as_datetime("2020-06-08"),
                          bedtime < as_datetime("2020-06-15")),
                 aes(xmin = int_start(sleep_time), 
                                xmax = int_end(sleep_time), 
                                y = as.factor(user_id)), size = 7, color = "grey", alpha = .5) +
  geom_linerange(data = df_sleeptime %>% 
                   filter(user_id %in% c(1:50),
                          end_segment > as_datetime("2020-06-08"),
                          end_segment < as_datetime("2020-06-15")),
                 aes(xmin = start_segment,
                     xmax = end_segment,
                     color = app_cat,
                     y = as.factor(user_id)), size = 6) +
  geom_linerange(data = df_nighttime %>% 
                   filter(user_id %in% c(1:50),
                          end_segment > as_datetime("2020-06-08"),
                          end_segment < as_datetime("2020-06-15")),
                 aes(xmin = start_segment,
                     xmax = end_segment,
                     y = as.factor(user_id)), size = 4, color = "black")

int_plot2 <- ggplot() + 
  geom_linerange(data = sleep %>% 
                   filter(user_id %in% c(1:50), 
                          bedtime > as_datetime("2020-06-08"),
                          bedtime < as_datetime("2020-06-15")),
                 aes(xmin = int_start(sleep_time), 
                     xmax = int_end(sleep_time), 
                     y = as.factor(user_id)), size = 7, color = "grey", alpha = .5) +
  geom_linerange(data = df_sleeptime %>% 
                   filter(user_id %in% c(1:50),
                          end_segment > as_datetime("2020-06-08"),
                          end_segment < as_datetime("2020-06-15")),
                 aes(xmin = start_segment,
                     xmax = end_segment,
                     color = app_cat,
                     y = as.factor(user_id)), size = 4, color = "black") +
  geom_linerange(data = df_nighttime %>% 
                   filter(user_id %in% c(1:50),
                          end_segment > as_datetime("2020-06-08"),
                          end_segment < as_datetime("2020-06-15")),
                 aes(xmin = start_segment,
                     xmax = end_segment,
                     color = app_cat,
                     y = as.factor(user_id)), size = 6)

ggplotly(int_plot2)
  

