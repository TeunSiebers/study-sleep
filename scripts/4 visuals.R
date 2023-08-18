# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           4 visuals.R
#
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(hrbrthemes)
library(ggridges)
library(scales)
library(ggh4x)

# define participants
participants <- read.table("data/processed/user_ids.txt")$x


# read data ---------------------------------------------------------------

# read app usage data
df <- map_df(
  c(list.files("data/processed/exploratory/", full.names = T), 
    list.files("data/processed/main/", full.names = T)), 
  ~ mutate(read_csv(.), cat = str_remove_all(., "data/processed/(exploratory|main)/|\\.csv"))
  ) %>% 
  mutate(cat = ifelse(cat == "phone", "other", cat),
         cat = ordered(cat, levels = c("social", "game", "video", "other"))) %>% 
  filter(user_id %in% participants)

# read timeframe data
timeframe <- read_csv("data/processed/timeframes.csv")

# read durations data per timeframe, per category
cofluc_df <- map_df(
  list.files("data/output/", full.names = T, pattern = ".csv"), 
  ~ mutate(read_csv(.), cat = str_remove_all(., "data/output/|\\.csv"))
) %>% 
  mutate_all(~ifelse(. == -999, NA, .)) %>% 
  na.omit() %>% 
  pivot_longer(cols = c(postbed_use, prebed_use, daytime_use, sleep_qual), 
               names_to = "variable", 
               values_to = "value") %>% 
  mutate(variable = factor(variable, 
                           levels = c("sleep_qual", "daytime_use", "prebed_use", "postbed_use"))) %>% 
  mutate(cat = ifelse(variable == "sleep_qual", "sleep_qual", cat)) %>% 
  unique() %>% 
  arrange(user_id, study_day, variable)



# prepare density distribution plots ----------------------------------------

timeframe_per_cat <- map_df(list.files("data/output/", full.names = T, pattern = ".csv"),
                            function(x) {
                              read_csv(x) %>% 
                                mutate_all(~ifelse(. == -999, NA, .)) %>%
                                mutate(cat = str_remove_all(x, "data/output/|\\.csv")) %>% 
                                na.omit()
                            })

prep_figures <- timeframe_per_cat %>% 
  mutate(cat = case_when(cat == "phone" ~ "Smartphone",
                         cat == "social" ~ "Social media",
                         cat == "game" ~ "Game",
                         cat == "video" ~ "Video player"),
         cat = factor(cat, levels = c("Smartphone", 
                                      "Social media", 
                                      "Game", 
                                      "Video player")),
         daily_use = daytime_use + prebed_use + postbed_use) %>% 
  group_by(user_id, cat) %>% 
  summarise_at(.vars = c("daytime_use", "prebed_use", "postbed_use", "daily_use"), 
               .funs = mean) %>% 
  ungroup %>% 
  pivot_longer(cols = -c(1,2), values_to = "duration", names_to = "timeframe") %>% 
  mutate(timeframe = case_when(timeframe == "daily_use" ~ "Daily use (in hours)",
                               timeframe == "daytime_use" ~ "Daytime use (in hours)",
                               timeframe == "prebed_use" ~ "Pre-bedtime use (in minutes)",
                               timeframe == "postbed_use" ~ "Post-bedtime use (in hours)"),
         duration = ifelse(timeframe == "Pre-bedtime use (in minutes)", duration * 60, duration))


# create smartphone density plot ---------------------------------------

figure_1 <- prep_figures %>% 
  filter(cat == "Smartphone") %>% 
  ggplot() +
  geom_density_ridges_gradient(aes(x = duration, y = cat, fill = factor(after_stat(quantile))),
                               quantiles = c(.25, .5, .75), quantile_lines = F,
                               calc_ecdf = TRUE, scale = .95) +
  scale_fill_manual(labels = c("0-25th", "25-50th", "50-75th", "75-100th"),
                    values = c("#cfd8dc","#78909c","#546e7a","#263238")) +
  scale_x_continuous(name = "Average time spent on smartphone",
                     breaks = pretty_breaks(8), limits = c(0, NA)) + 
  scale_y_discrete(name = NULL, limits=rev,expand = c(0, 0)) +
  labs(fill = "Percentile of N") + 
  theme_minimal() +
  theme(panel.spacing = unit(0.5,'lines'),
        legend.position = c(.99, .85), 
        legend.justification = "right", 
        legend.direction = "vertical", 
        legend.background = element_rect(fill = "white", color = NA), 
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank())

design_1 <- matrix(c(1, 1, 2, 1, 1, 4, 1, 1, 3), 3, 3)

figure_1 <- figure_1 + 
  facet_manual(vars(factor(timeframe)), design = design_1, scales = "free") +
  theme(plot.margin = grid::unit(c(1,1,1,1), "mm"))

figure_1

  # save plot
if(F) ggsave(plot = figure_1, "data/output/figures/Figure 1 - smartphone density.svg", width = 22, height = 11, units = "cm")


# create app density plot ----------------------------------------------

figure_2 <- prep_figures %>% 
  filter(cat != "Smartphone", 
         !(timeframe == "Post-bedtime use (in hours)" & duration > 3)) %>% 
  ggplot() +
  geom_density_ridges_gradient(aes(x = duration, y = cat, fill = factor(after_stat(quantile))),
                               quantiles = c(.25, .5, .75), quantile_lines = F,
                               calc_ecdf = TRUE, scale = .95) +
  scale_fill_manual(labels = c("0-25th", "25-50th", "50-75th", "75-100th"),
                    values = c("#cfd8dc","#78909c","#546e7a","#263238")) +
  scale_x_continuous(name = "Average time spent on app",
                     breaks = pretty_breaks(8), limits = c(0, NA)) + 
  scale_y_discrete(name = NULL, limits=rev,expand = c(0, 0)) +
  labs(fill = "Percentile of N") + 
  theme_minimal() +
  theme(panel.spacing = unit(0.5,'lines'),
        legend.position = c(.99, .85), 
        legend.justification = "right", 
        legend.direction = "vertical", 
        legend.background = element_rect(fill = "white", color = NA), 
        panel.grid.minor.y = element_blank())

design_2 <- matrix(c(1, 1, 2, 1, 1, 4, 1, 1, 3), 3, 3)

figure_2 <- figure_2 + 
  facet_manual(vars(factor(timeframe)), design = design_2, scales = "free_x") +
  theme(plot.margin = grid::unit(c(1,1,1,1), "mm"))

figure_2

# save plot
if(F) ggsave(plot = figure_2, "data/output/figures/Figure 2 - app density.svg", width = 22, height = 15, units = "cm")


# create app usage plot ---------------------------------------------------

sample_user <- c(191, 30, 271, 259, 65, 265, 222, 150, 288, 100)

start_int <- as_datetime("2020-06-4 9:00:00")
period <- ddays(1) 

sample_days <- as.interval(start_int, start_int + period)


figure <- ggplot() +
  geom_rect(data = timeframe %>%
              filter(user_id %in% sample_user,
                     bt_yes %within% sample_days),
            mapping = aes(xmin = bt_yes-hours(1), xmax = bt_yes, ymin = -Inf, ymax = Inf),
            alpha = .1) +
  geom_rect(data = timeframe %>%
              filter(user_id %in% sample_user,
                     bt_yes %within% sample_days),
            mapping = aes(xmin = bt_yes, xmax = wt_today, ymin = -Inf, ymax = Inf),
            alpha = .3) +
  geom_vline(xintercept = as_datetime("2020-06-5 00:00:00")) +
  geom_linerange(data = df %>%
                   filter(user_id %in% sample_user,
                          start %within% sample_days,
                          cat == "other"),
                 mapping = aes(xmin = start, xmax = end,
                               y = as.factor(user_id), color = cat),
                 size = 5) +
  geom_linerange(data = df %>%
                   filter(user_id %in% sample_user,
                          start %within% sample_days,
                          cat %in% c("social", "game", "video")),
                 mapping = aes(xmin = start, xmax = end,
                               y = as.factor(user_id), color = cat),
                 size = 5) +
  labs(y = "Participant", color = "App category:") + 
  scale_x_datetime(name = "Time",
                   breaks = seq(as.POSIXct(start_int),
                                as.POSIXct(start_int + period), "3 hours"),
                   date_labels = "%H:%M h\n%e %b",
                   minor_breaks = "1 hours",
                   expand = c(0, 0),
                   limits = c(
                     as.POSIXct(start_int),
                     as.POSIXct(start_int + period)
                   )) +
  scale_color_manual(breaks = c("social", "game", "video", "other"),
                     values = c(social = "#FC4E07",
                                game = "#E7B800",
                                video = "#00AFBB",
                                other = "grey39")) +
  scale_y_discrete(limits=rev) +
  facet_wrap(~as.factor(user_id), ncol = 1, scales = "free_y") + 
  theme_minimal() +
  theme(strip.text.x = element_blank(),
        panel.spacing = unit(0.0,'lines'),
        legend.position = "top", legend.justification ="right",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# strip whitespace
figure <- figure_2 + theme(plot.margin = grid::unit(c(0,6,1,1), "mm"))
figure

# save plot
if(F) ggsave(plot = figure, "data/output/figures/Figure - app use.svg", width = 22, height = 11, units = "cm")


# create co-fluctuation plot ----------------------------------------------

selection <- sample(unique(cofluc_df$user_id), 3)
selection <- sample(c(108, 191, 55, 114, 30, 55, 191, 161), 4)

cofluc_df %>% 
  mutate(variable = case_when(variable == "sleep_qual" ~ "sleep quality",
                              variable == "daytime_use" ~ "daytime use",
                              variable == "prebed_use" ~ "pre-bedtime use",
                              variable == "postbed_use" ~ "post-bedtime use"),
         cat = case_when(cat == "social" ~ "social media app",
                         cat == "game" ~ "game app",
                         cat == "video" ~ "video player app",
                         cat == "sleep_qual" ~ "sleep quality")) %>% 
  filter(user_id %in% selection, cat != "phone") %>% 
  ggplot() +
  geom_line(aes(x = study_day, y = value, color = cat)) +
  scale_color_manual(values = c("black",
                                "social media app" = "#FC4E07",
                                "game app" = "#E7B800",
                                "video player app" = "#00AFBB")) +
  facet_grid2(variable ~ paste("ID:", as.factor(user_id)), 
              scales = "free_y", 
              strip = strip_themed(text_y = elem_list_text(angle = c(0)),)) +
  scale_x_continuous(name = "study day",
                     minor_breaks = seq(1,21,1), breaks = seq(1,21, 5)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  theme_bw() +
  theme(panel.spacing = unit(c(0),'lines'),
        panel.spacing.y = unit(c(0, 0, 1),'lines'),
        legend.position = "top", legend.justification ="right",
        legend.title=element_blank())


cofluc_df %>% 
  filter(cat != "phone") %>% 
  ggplot() + 
  geom_smooth(aes(x = duration, y = sleep_qual, color = cat)) +
  labs(x = "duration (hours)", y = "sleep quality") +
  scale_color_manual(values = c(social = "#FC4E07",
                                game = "#E7B800",
                                video = "#00AFBB")) +
  facet_grid(cat~variable, scales = "free_x") +
  theme_minimal()