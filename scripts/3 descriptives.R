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

# create function to rename user_id,
flatten <- function(...) {
  rename(..., chrom = user_id) %>% 
    bed_merge() %>%
    mutate_at(vars(start, end), as_datetime) %>% 
    mutate(duration = as.double(end - start)) %>% 
    ungroup() %>% 
    rename(user_id = chrom)
}

# read data ---------------------------------------------------------------

# read data and set column types
df <- read_csv("data/processed/df_app.csv") %>% 
  mutate_at(vars(user_id, apk, app_name, app_cat), as.factor) %>% 
  mutate_at(vars(start, end), as_datetime) %>% 
  mutate(duration = as.numeric(duration)) %>% 
  arrange(user_id, start) # 758,646 obs.; N = 155


# smartphone descriptives -------------------------------------------------

# split app activities at midnight
df_split <- df %>% 
  filter(duration < 24*60*60) %>% # remove observations that last longer than 24 hours
  mutate(end = ifelse(as_date(start) != as_date(end), 
                      paste(as.character(as_date(end)), "00:00:00") %>% 
                        as_datetime, 
                      end) %>% as_datetime) %>% 
  bind_rows(df %>% 
              filter(as_date(start) != as_date(end)) %>% 
              mutate(start = as_datetime(paste0(as_date(end), 
                                                " 00:00:00")))) %>% 
  arrange(user_id, start) %>%
  mutate(duration = as.double(end - start))

# flatten dataset
df_split %>% 
  flatten %>% 
  mutate(duration = as.double(end - start)) %>% 
  group_by(user_id, as_date(start)) %>% 
  summarise(sum_duration = sum(duration)/60/60) %>% 
  filter(sum_duration < 24) %>% 
  summarise(m_pp = mean(sum_duration),
            sd_pp = sd(sum_duration)) %>% 
  summarise(m = mean(m_pp),
            sd = sd(m_pp, na.rm = T))
  

# descriptives per category -----------------------------------------------

# phone
phone <- read_csv("data/output/phone_99th.csv") %>% 
  mutate_all(~ifelse(. == -999, NA, .)) %>% mutate(cat = "1. smartphone") %>% na.omit()

# social 
social <- read_csv("data/output/social_99th.csv") %>% 
  mutate_all(~ifelse(. == -999, NA, .)) %>% mutate(cat = "2. social") %>% na.omit()

# game 
game <- read_csv("data/output/game_99th.csv") %>% 
  mutate_all(~ifelse(. == -999, NA, .)) %>% mutate(cat = "3. game") %>% na.omit()

# video 
video <- read_csv("data/output/video_99th.csv") %>% 
  mutate_all(~ifelse(. == -999, NA, .)) %>% mutate(cat = "4. video") %>% na.omit()

# music 
music <- read_csv("data/output/music_99th.csv") %>% 
  mutate_all(~ifelse(. == -999, NA, .)) %>% mutate(cat = "5. music") %>% na.omit()

bind_rows(phone, social, game, video, music) %>% 
  group_by(cat) %>% 
  summarise(m_day = mean(daytime_use, na.rm = T), sd_day = sd(daytime_use, na.rm = T),
            m_pre = mean(prebed_use, na.rm = T), sd_pre = sd(prebed_use, na.rm = T),
            m_post = mean(postbed_use, na.rm = T), sd_post = sd(postbed_use, na.rm = T))


# ICCs --------------------------------------------------------------------

# create a function to extract descriptives from specified variables
fun_descriptives <- function(x, data = phone) {
  tibble(var = all_of(x),
         obs = na.omit(pull(data, all_of(x))) %>% length(),
         mean = mean(pull(data, all_of(x)), na.rm = T),
         sd = sd(pull(data, all_of(x)), na.rm = T),
         range = paste0(min(pull(data, all_of(x)), na.rm = T) %>% round(2), "-", max(pull(data, all_of(x)), na.rm = T) %>% round(2)),
         median = median(pull(data, all_of(x)), na.rm = T)) 
}

# map function to obtain descriptives from ESM variables
descriptives <- map_df(select(phone, daytime_use, prebed_use, postbed_use, sleep_qual) %>% names(), # specify variables
                       fun_descriptives) %>% 
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
  rename(M = mean, SD = sd, Range = range, Median = median, 
         `Var(W)` = within_var, `Var(B)` = between_var, ICC = icc) %>% 
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
