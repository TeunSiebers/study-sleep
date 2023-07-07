# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           3 demographics-compliance.R
#
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)


# read files  ------------------------------------------------------------

# read keyfile and filter only participants in the user_ids.txt file
esm2 <- read_csv2("data/input/cleaned esm data - wave2_no attrition.csv") %>% 
  filter(ID %in% read.table("data/processed/user_ids.txt")$x)


# extract demographic information -----------------------------------------
# 19,530 obs.; N = 155

# select only relevant variables
sample_details <- esm2 %>% 
  select(ID, Sex, BirthD, ETH01, ETH03, EduLevel) %>% 
  unique() %>% 
  mutate(age = time_length(difftime(as_date("2020-06-02"), BirthD), 
                           unit = "years")) %>% 
  filter(ID %in% user_ids)

#  155 obs. N = 155
mean(sample_details$age) # 14.54
sd(sample_details$age) # 0.67
min(sample_details$age); max(sample_details$age) # 13.38 - 16.46

table(sample_details$Sex) # anders: 1, jongen: 80, meisje: 74
prop.table(table(sample_details$Sex)) # anders: 0.6%, jongen: 51,6%, meisje: 47,7%


# calculate compliance ----------------------------------------------------

# calculate overall compliance
study_rows <- esm2 %>% 
  select(ID, BeepTE2, StateE2, SLE01E2) %>% 
  filter(BeepTE2 == 1)

# total surveys sent out
study_rows %>% nrow # 3255

# total surveys completed
study_rows %>% 
  filter(SLE01E2 %in% c(1:7)) %>% 
  group_by(SLE01E2) %>% 
  count() %>% .$n %>% sum # 1950

# overall compliance
1950/3255 # = 60%

# calculate average compliance
compl_per_person <- esm2 %>% 
  select(ID, BeepTE2, StateE2, SLE01E2) %>% 
  filter(BeepTE2 == 1, SLE01E2 %in% c(1:7)) %>% 
  group_by(ID) %>% 
  count()

mean(compl_per_person$n, na.rm = T) # M = 12.58
sd(compl_per_person$n, na.rm = T) # SD = 5.53

