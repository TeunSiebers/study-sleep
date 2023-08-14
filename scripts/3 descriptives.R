# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           3 descriptives.R
#
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(valr) # for flattening intervals with bed_merge
library(lme4)
library(Hmisc) # for rcorr()
library(psych)
library(Matrix)
library(glue)
library(data.table)
library(knitr) # for formatting table
library(kableExtra) # for formatting table
library(hrbrthemes)
library(ggridges)
library(scales)


# create function to rename user_id,
flatten <- function(...) {
  rename(..., chrom = user_id) %>% 
    bed_merge() %>%
    mutate_at(vars(start, end), as_datetime) %>% 
    mutate(duration = as.double(end - start)) %>% 
    ungroup() %>% 
    rename(user_id = chrom)
}

# define categories
social_media <- c("Instagram", "WhatsApp Messenger", "Snapchat", "TikTok", 
                  "Twitter", "Facebook", "Messenger", "Reddit", 
                  "Discord: Talk, Chat & Hang Out")

video_ent <- c("YouTube", "Netflix", "Twitch: Live Game Streaming", 
               "Disney+", "Videoland", "Ziggo GO", "V LIVE", "HUAWEI Video", 
               "Pokémon TV", "MTV Play", "Amazon Prime Video", "NPO")

# read data ---------------------------------------------------------------

# read data and set column types
df <- read_csv("data/processed/df_app.csv") %>% 
  mutate_at(vars(user_id, apk, app_name, app_cat), as.factor) %>% 
  mutate_at(vars(start, end), as_datetime) %>% 
  mutate(duration = as.numeric(duration)) %>% 
  arrange(user_id, start) # 758,646 obs.; N = 155



# timeframe data ----------------------------------------------------------


# read timeframes
timeframes <- read_csv("data/processed/timeframes.csv") %>% 
  select(user_id, weekday, bt_yes, wt_today) %>% 
  mutate(bt_yes = hour(bt_yes) + (minute(bt_yes)/60),
         wt_today = hour(wt_today) + (minute(wt_today)/60),
         weekend = ifelse(weekday %in% c("za", "zo"), "weekend day", "weekday"))

timeframes %>% 
  mutate(bt_yes = ifelse(bt_yes < 15, bt_yes + 24, bt_yes)) %>% 
  group_by(weekend) %>% 
  summarise_at(vars(bt_yes, wt_today), list(M = mean, SD = sd)) %>% 
  mutate_at(-1, ~seconds_to_period(. * 3600))

# daily smartphone use ----------------------------------------------------

# create function to split app activities per day
generate_daily_time <- function(x, y) {
  if(as_date(x) == as_date(y)){
    tibble(start_d = x, end_d = y)
  } else {
    end <- ceiling_date(x, 'day')
    end2 <- seq(end, floor_date(y, 'day'), by = 'day')
    tibble(start_d = c(x, end2), end_d = c(end2, y))
  }
}

# split smartphone use at every day
day_split <- df %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_daily_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_d - start_d)) %>%
  select(-start, -end)

# flatten dataset
daily_time <- day_split %>% 
  group_by(user_id, as_date(start_d)) %>% 
  summarise(sum_duration = sum(duration))

mean(daily_time$sum_duration) %>% seconds_to_period # Mean = 6 hr 25 min
sd(daily_time$sum_duration) %>% seconds_to_period # SD = 3 hr 35 min


# create plot to visualize time spent on smartphone per day
ggplot(data = daily_time) +
  geom_histogram(aes(x = sum_duration/60/60), 
                 fill = "#69b3a2", color = "white", binwidth = 0.5) +
  scale_x_continuous(breaks = seq(0, 24, 2), 
                     name = "time spent on smartphone on one day (in hours)") + 
  theme_ipsum()


# daily use of app categories --------------------------------------------

# split smartphone use at every day
day_split_social <- df %>% 
  filter(app_name %in% social_media) %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_daily_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_d - start_d)) %>%
  select(-start, -end) %>% 
  mutate(app_type = "social media")

# split smartphone use at every day
day_split_game <- df %>% 
  filter(app_cat == "Gaming") %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_daily_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_d - start_d)) %>%
  select(-start, -end)  %>% 
  mutate(app_type = "game")

# split smartphone use at every day
day_split_video <- df %>% 
  filter(app_name %in% video_ent) %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_daily_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_d - start_d)) %>%
  select(-start, -end) %>% 
  mutate(app_type = "video player")


# hourly smartphone use ---------------------------------------------------

# create function to split app activities per hour
generate_hourly_time <- function(x, y) {
  if(hour(x) == hour(y)){
    tibble(start_h = x, end_h = y)
  } else {
    end <- ceiling_date(x, 'hour')
    end2 <- seq(end, floor_date(y, 'hour'), by = 'hour')
    tibble(start_h = c(x, end2), end_h = c(end2, y))
  }
}

# split smartphone use at every hour of the day
hour_split <- df %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_hourly_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_h - start_h)) %>%
  select(-start, -end)

# create hourly smartphone use durations
use_per_hour <- hour_split %>% 
  filter(as_date(start_h) >= "2020-06-02") %>% 
  filter(duration <= 3600) %>% 
  group_by(hour = hour(start_h), 
           weekday = factor(weekdays(start_h, T), 
                            levels = c("ma", "di", "wo", "do", "vr", "za", "zo"))) %>% 
  summarise(use_time = sum(duration)) %>% 
  mutate(weekend = ifelse(weekday %in% c("za", "zo"), "weekend day", "weekday"))

# visualize how smartphone use and bed- and wakeup times are distributed across the day
ggplot() + 
  geom_vline(data = timeframes, aes(xintercept = bt_yes), alpha = .05, color = "black") +
  geom_vline(data = timeframes, aes(xintercept = wt_today), alpha = .05, color = "orange") +
  geom_line(data = use_per_hour,
            aes(x = hour+0.5, y = use_time, color = weekday),
               alpha = 1, stat = "identity") +
  scale_y_continuous(name = "smartphone activity in sample", breaks = NULL) + 
  scale_x_continuous(breaks = seq(0, 24, 1), expand = c(.02,.02),
                     name = "hour of the day", labels = function(x) str_c(x, ':00')) + 
  geom_text(data = timeframes, aes(x = 1, y = 700000, label = weekend), 
            hjust = "left")+
  guides(colour = guide_legend(nrow = 1), title = NULL) +
  theme_ipsum() +
  facet_wrap(~weekend, ncol = 1) +
  theme(legend.position = "top", legend.box.just = "left",
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.title= element_blank(),
        strip.text.x = element_blank())


# hourly use of app categories --------------------------------------------

# split smartphone use at every hour of the day
hour_split_social <- df %>% 
  filter(app_name %in% social_media) %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_hourly_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_h - start_h)) %>%
  select(-start, -end) %>% 
  mutate(app_type = "social media")

# split smartphone use at every hour of the day
hour_split_game <- df %>% 
  filter(app_cat == "Gaming") %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_hourly_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_h - start_h)) %>%
  select(-start, -end) %>% 
  mutate(app_type = "game")

# split smartphone use at every hour of the day
hour_split_video <- df %>% 
  filter(app_name %in% video_ent) %>% 
  flatten %>% 
  mutate(time = purrr::map2(start, end, generate_hourly_time)) %>% 
  tidyr::unnest(time) %>%
  mutate(duration = as.numeric(end_h - start_h)) %>%
  select(-start, -end) %>% 
  mutate(app_type = "video player")

# create hourly smartphone use durations
appcat_per_hour <- bind_rows(hour_split_social, hour_split_game, hour_split_video) %>% 
  filter(as_date(start_h) >= "2020-06-02") %>% 
  filter(duration <= 3600) %>% 
  group_by(hour = hour(start_h),
           app_type) %>% 
  summarise(use_time = sum(duration))

# create ggplot
appcat_per_hour %>% 
  ggplot() + 
  geom_density(aes(x = hour + 0.5, y = use_time, color = app_type), 
               stat = 'identity') +
  scale_y_continuous(name = "app activity in sample", breaks = 0, labels = NULL) + 
  scale_x_continuous(breaks = seq(0, 24, 2), expand = c(.05,.05),
                     name = "hour of the day", labels = function(x) str_c(x, ':00')) + 
  labs(x = "hour of the day", y = "app activity in sample", color = "App category") +
  theme_ipsum()
  

# descriptives per category -----------------------------------------------

timeframe_per_cat <- map_df(
  list.files("data/output/", full.names = T, pattern = ".csv"),
  function(x) {
    read_csv(x) %>% 
      mutate_all(~ifelse(. == -999, NA, .)) %>%
      mutate(cat = str_remove_all(x, "data/output/|\\.csv")) %>% 
      na.omit()
    }) 

timeframe_per_cat %>% 
  mutate(cat = case_when(cat == "phone" ~ "1. smartphone",
                         cat == "social" ~ "2. social media app",
                         cat == "game" ~ "3. game app",
                         cat == "video" ~ "4. video player app")) %>% 
  group_by(cat) %>% 
  summarise_at(.vars = c("daytime_use", "prebed_use", "postbed_use"), 
               .funs = list(m = ~seconds_to_period(mean(.)*3600), 
                            sd = ~seconds_to_period(sd(.)*3600))) %>% 
  mutate_if(is.period, ~round(., 0))


# ICCs --------------------------------------------------------------------

# create a function to extract descriptives from specified variables
fun_descriptives <- function(x, data = phone) {
  tibble(var = all_of(x),
         # obs = na.omit(pull(data, all_of(x))) %>% length(),
         mean = mean(pull(data, all_of(x)), na.rm = T),
         sd = sd(pull(data, all_of(x)), na.rm = T))
         # range = paste0(min(pull(data, all_of(x)), na.rm = T) %>% round(2), "-", max(pull(data, all_of(x)), na.rm = T) %>% round(2)),
         # median = median(pull(data, all_of(x)), na.rm = T)) 
}

# map function to obtain descriptives from ESM variables
descriptives <- phone %>% 
  select(daytime_use, prebed_use, postbed_use, sleep_qual) %>% 
  names %>% # specify variables
  map_df(fun_descriptives) %>% 
  mutate_if(is.numeric, ~round(., 2))

# inspect variances and ICC (intra-class correlation) -------------------------------

# create a function to extract within and between subject variance and ICC
fun_variance <- function(x) {
  
  # create table with between and within subject variances with lmer() function and VarCorr()
  variance_table <- lmer(formula = paste0(x, "~ 1 + (1 | user_id)"), data = phone) %>% 
    VarCorr() %>% 
    print(comp = "Variance") %>% 
    as_tibble() %>% 
    select(grp, vcov) %>%
    pivot_wider(names_from = grp, values_from = vcov)
  
  # create data frame with item names, icc value, number of groups (n), number of observations (obs),
  # within subject variance, between subject variance, total variance, and icc check (= between_var/ total_var)
  tibble(
    var = all_of(x),
    obs = phone %>% select(all_of(x)) %>% na.omit() %>% nrow(),
    n = phone %>% select(user_id, all_of(x)) %>% na.omit() %>% group_by(user_id) %>% count() %>% nrow(),
    icc = lmer(formula = paste0(x, "~ 1 + (1 | user_id)"), # 'x' here is replace by the variables of interest specified above, paste0() is needed because otherwise it won't be recognized as a variable, but as an object
               data = phone) %>%
      performance::icc() %>% # performance::icc() is used to extract icc values. This comes from the sjstats package.
      .$ICC_adjusted,
    within_var = variance_table$Residual,
    between_var = variance_table$user_id,
    total_var = within_var + between_var,
    icc_check = between_var / total_var)
}

# map function to obtain variances and ICCs from the variables specified
iccs <- map_df(select(phone, daytime_use, prebed_use, postbed_use, sleep_qual) %>% names(), # specify variables
               fun_variance) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  select(var, within_var, between_var, icc) %>%
  mutate(icc = as.character(icc) %>% str_remove("^0")) # select icc column and remove "0" in "0."


# Correlations ------------------------------------------------------------

# within- and between-person correlations (wbc) of fragment, sticky, distraction and task delay
wbc <- phone %>% 
  select(user_id, daytime_use, prebed_use, postbed_use, sleep_qual) %>%
  mutate_all(~ as.numeric(.)) %>% 
  statsBy(group = "user_id", cors = F) # if cors is true, the correlation matrix within each group is included in the results

print(wbc, short = F, digits = 3) # print correlation matrix if short is false. If short = true, only ICCs are printed.

# inpsect associations
as_tibble(wbc$rwg); as_tibble(wbc$pwg) # within-person associations
as_tibble(wbc$rbg); as_tibble(wbc$pbg) # between-person associations

# create correlation matrix
cor_matrix <- as_tibble(
  as.matrix(tril(wbc$rbg, -1) + triu(wbc$rwg, 1))
) %>% 
  mutate(var = str_remove(names(.), "\\.[bw]g")) %>% 
  rename_all(~str_remove(.x, "\\.[bw]g")) %>% 
  rename("1." = daytime_use,
         "2." = prebed_use,
         "3." = postbed_use,
         "4." = sleep_qual) %>% 
  mutate_if(is.numeric, ~round(., 2) %>% 
              str_replace("^0$", "—") %>%
              str_remove("^0|(?<=-)0")) %>%
  mutate_all(
    ~ifelse(
      str_detect(., "\\.") & str_length(str_extract(., "(?<=.)[0-9]+")) == 1, 
      paste0(., "0"), 
      .) )

# set pvalues in a matrix
pvalues <- as.matrix(tril(wbc$pbg, -1) + triu(wbc$pwg, 1)) %>% 
  as_tibble() %>% 
  mutate_all(~case_when(
    . < .001 ~ "***",
    . < .01 ~ "**",
    . < .05 ~ "*")) %>% 
  rename_all(~case_when(str_detect(., "daytime") ~ "1.y",
                        str_detect(., "prebed") ~ "2.y",
                        str_detect(., "postbed") ~ "3.y",
                        str_detect(., "sleep") ~ "4.y",
                        TRUE ~ "-"))

# unite two dataframes and funetune strings
cor_p <- bind_cols(cor_matrix, pvalues) %>% 
  unite(col = "1.", c("1.", "1.y"), sep = "") %>% 
  unite(col = "2.", c("2.", "2.y"), sep = "") %>% 
  unite(col = "3.", c("3.", "3.y"), sep = "") %>% 
  unite(col = "4.", c("4.", "4.y"), sep = "") %>% 
  mutate_all(~str_remove(., "NA|(?<=—).+"))


# Create table ------------------------------------------------------------

# merge descriptives and the correlation matrix
table_prep <- descriptives %>% 
  left_join(iccs %>% select(var, within_var, between_var, icc)) %>%
  left_join(cor_p) %>% 
  
  # change variable names
  mutate(var = case_when( 
    var == "daytime_use" ~ "daytime use",
    var == "prebed_use" ~ "pre-bedtime use",
    var == "postbed_use" ~ "post-bedtime use",
    var == "sleep_qual" ~ "sleep quality",
    TRUE ~ var)) %>%  
  rowid_to_column() %>% 
  unite(col = var, rowid:var, sep = ". ") %>%  # add numbers to the variable names
  mutate(" " = " ", .after = icc) %>% 
  rename(`Study variable` = var)

table_prep

# add footnote marker to the correlations header
corr_header <- paste0("Correlations", 
                      footnote_marker_alphabet(1, double_escape = T))

# create superscript asterisks
table_footn_prep <- table_prep %>% 
  mutate_all(funs(
    ifelse(str_detect(., "\\*\\*\\*"), 
           str_replace_all(., "\\*\\*\\*", "<sup>***</sup>"),
           ifelse(str_detect(., "\\*\\*"), 
                  str_replace_all(., "\\*\\*", "<sup>**</sup>"),
                  ifelse(str_detect(., "\\*"), 
                         str_replace_all(., "\\*", "<sup>*</sup>"),
                         .)))))

# format Table 1
table <- table_footn_prep %>%
  rename(M = mean, SD = sd, 
         `Var(W)` = within_var, `Var(B)` = between_var, 
         ICC = icc) %>% 
  kable(caption = "Table. Descriptives and Correlations for All Study Variables", 
        align = "lccccllllll",
        escape = F) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  add_header_above(c(" " = 1, 
                     "Descriptives" = ncol(descriptives)-1 + ncol(iccs)-1,
                     " " = 1,
                     setNames(ncol(cor_p)-1, corr_header)),
                   escape = F) %>% # create the correlations header with footnote marker
  column_spec(ncol(descriptives) + 1 + ncol(iccs) + 1, width = "2em") %>% 
  footnote(general_title = "Note.",
           general = glue("Var(W) = within-person variance; Var(B) = between-person variance; ICC = intra-class correlation."),
           symbol = c("$p < .05.$", "$p < .01.$", "$p < .001.$"),
           symbol_manual = c("*", "**", "***"),
           alphabet = c("Within-person correlations are depicted above the diagonal and between-person correlations below the diagonal."),
           footnote_as_chunk = TRUE)
table

# save table
if(F) save_kable(table, file = "data/output/tables/descriptives_and_correlations.html")
