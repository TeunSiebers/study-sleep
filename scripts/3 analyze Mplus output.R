# >>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>----
# project:         study-sleep.Rproj
# title:           3 analyze Mplus output.R
#
# description:
# ...
# ---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>---->>>>

# load packages
library(tidyverse)
library(MplusAutomation)
library(glue)
library(kableExtra)


# create function to tidy Mplus data --------------------------------------

# create a function that reads the Mplus output and creates an APA table from that output
tidy_mplus <- function(path) {
  
  # load Mplus model
  mplus <- readModels(target = path, what = "all")
  
  # extract model specification
  mplus_model <- mplus$input$model %>% 
    str_remove("!.*") %>% # remove comments
    str_trim() %>%  # remove white space
    str_remove(";") %>% # remove end-of-line mark
    str_subset(".+") # remove empty lines
  
  # extract within part and between part of model specification
  between_index <- which(mplus_model %in% tolower("%between%")) # get location of %between% in the vector
  
  model_within <- mplus_model[c(1:between_index-1)][-1] %>% toupper()
  model_between <- mplus_model[c(between_index:length(mplus_model))][-1] %>% 
    toupper()
  
  # extract Random Effect Variable (REV)
  REV <- str_subset(model_within, "\\|") %>% 
    str_extract("[^\\|]+") %>% # select everything before the vertical line
    str_trim() %>% # trim whitespace
    str_subset("PHI", negate = T)
  
  # extract unstandardized parameters
  unstan <- tibble(mplus$parameters$unstandardized)
  
  # extract standardized parameters
  stdyx <- tibble(mplus$parameters$stdyx.standardized)
  stdy <- tibble(mplus$parameters$stdy.standardized)
  
  # CREATE: statistics table 
  categorical_variables <- "WEEKDAY"
  
  # create function to make values APA style, with greater than one (gto) as option
  apa <- function(x, digits = 3, gto = T) {
    output <- x %>% 
      format(nsmall = digits, digits = digits) %>% 
      as.character()
    
    if(!gto) output <- str_remove(output, "0(?=\\.)")
    
    output
  }
  
  # extract unstandardized parameter estimates
  random <- unstan %>% 
    transmute(paramHeader, param, BW = BetweenWithin, B = est, p = pval, 
              '95% CI' = glue("[{apa(lower_2.5ci)}, {apa(upper_2.5ci)}]")) %>% 
    filter(str_detect(paramHeader, "Variances")) %>% 
    mutate(random_fixed = "random")
  
  fixed_unstan <- unstan %>% 
    transmute(paramHeader, param, BW = BetweenWithin, B = est, p = pval, 
              '95% CI' = glue('[{apa(lower_2.5ci, gto = F)}, {apa(upper_2.5ci, gto = F)}]')) %>% 
    filter(!str_detect(paramHeader, "Variances")) %>% 
    transmute(paramHeader, param, BW, B)
  
  # extract standardized parameter estimates 
  fixed_stan <- stdyx %>% # stdyx standardization for all paramters except for categorical variables
    mutate(standardization = "stdxy") %>% 
    filter(param != categorical_variables) %>%
    add_row(stdy %>%  # stdy standardization only for categorical variables
              filter(param == categorical_variables) %>% 
              mutate(standardization = "stdy")) %>% 
    filter(!str_detect(paramHeader, "Variances")) %>% 
    transmute(paramHeader, 
              param, 
              BW = BetweenWithin, 
              standardization, 
              Beta = est, 
              p = pval, 
              '95% CI' = glue('[{apa(lower_2.5ci, gto = F)}, {apa(upper_2.5ci, gto = F)}]')) %>% 
    mutate(BW = str_extract(BW, "Within|Between"))
  
  fixed <- left_join(fixed_unstan, fixed_stan)
  
  # unstandardized within-person effect
  within_unstan <- unstan %>% 
    filter(BetweenWithin == "Between", 
           paramHeader %in% c("Means", "Intercepts"), param == REV) %>% 
    transmute(paramHeader, param, BW= BetweenWithin, B = est)
  
  # standardized within-person effect
  within_stan <- fixed_stan %>% 
    filter(BW == "Within", str_detect(paramHeader, REV))
  
  # combine the unstandardized and standardized parameters
  within_row <- tibble(paramHeader = str_remove(within_stan$paramHeader, 
                                                "[^\\|]+\\|") %>% str_trim(),
                       param = within_stan$param,
                       BW = "Within",
                       B = within_unstan$B,
                       standardization = within_stan$standardization,
                       Beta = within_stan$Beta,
                       p = within_stan$p,
                       `95% CI` = within_stan$`95% CI`)
  
  # combine unstandardized and standardized columns
  stat_table <- fixed %>% 
    add_row(within_row) %>% 
    filter(!(param == REV & paramHeader == "Means")) %>% 
    arrange(desc(BW), desc(paramHeader)) %>% 
    select(-standardization) %>% 
    mutate(random_fixed = "fixed") %>% 
    add_row(random) %>% 
    filter(str_detect(paramHeader, "\\.") | param == REV) #filter  parameters of interest
  
  # prepare table
  table_prep <- stat_table %>%
    arrange(random_fixed, desc(BW), paramHeader) %>% 
    mutate(effect = str_to_title(paste(paramHeader, param)) %>% 
             str_replace("\\.", " "),
           .before = paramHeader) %>%
    mutate_at(vars(B, Beta), ~format(., nsmall = 3)) %>% 
    mutate(Beta = str_remove(Beta, "0(?=\\.)")) %>% 
    mutate(
      Beta = case_when(
        str_detect(Beta, "NA") ~ " ", 
        p < .001 ~ paste0(Beta, "<sup>***</sup>"),
        p < .01 ~ paste0(Beta, "<sup>**</sup> "),
        p < .05 ~ paste0(Beta, "<sup>*</sup>  "),
        TRUE ~ paste0(Beta, "   ")),
      Beta = case_when(
        str_detect(Beta, "^\\-", negate = T) ~ paste0(" ", Beta),
        TRUE ~ Beta),
      B = case_when(
        str_detect(Beta, "[0-9]") ~ B,
        p < .001 ~ paste0(B, "<sup>***</sup>"),
        p < .01 ~ paste0(B, "<sup>**</sup> "),
        p < .05 ~ paste0(B, "<sup>*</sup>  "),
        TRUE ~ paste0(B, "   ")),
      B = case_when(
        str_detect(B, "^\\-", negate = T) ~ paste0(" ", B),
        TRUE ~ B),
      p = ifelse(p != 0, apa(p, digits = 3, gto = F), "< .001")) %>% 
    select(-c(paramHeader, param)) %>% 
    rename("$\bb$" = B, "$\\beta$" = Beta)
  
  table_prep
}


# apply function to Mplus output ------------------------------------------

# run function on all models for main analyses
out_main <- map_df(list.files(path = "mplus/main/", 
                                 pattern = "model_[123].out", 
                                 full.names = T), 
                      ~tidy_mplus(.)) %>% mutate(analysis = "main")

# run function on all models for sensitivity analyses
out_sens_24h <- map_df(list.files(path = "mplus/sens_24h/", 
                                 pattern = "model_[123].out", 
                                 full.names = T), 
                      ~tidy_mplus(.)) %>% mutate(analysis = "sens_24h")

# run function on all models for sensitivity analyses
out_sens_99th <- map_df(list.files(path = "mplus/sens_99th//", 
                                 pattern = "model_[123].out", 
                                 full.names = T), 
                      ~tidy_mplus(.)) %>% mutate(analysis = "sens_99th")

# run function on all models for sensitivity analyses
out_sens_99th_GP <- map_df(list.files(path = "mplus/sens_99th_GP//", 
                                 pattern = "model_[123].out", 
                                 full.names = T), 
                      ~tidy_mplus(.)) %>%  mutate(analysis = "sens_99th_GP")
 
combined <- bind_rows(out_main, out_sens_24h, out_sens_99th, out_sens_99th_GP) %>% 
  filter(str_detect(effect, "[Vv]ariances|[Pp]hi|effe", negate = T)) %>% 
  arrange(desc(BW)) %>% 
  relocate(effect, analysis, random_fixed, BW) %>% 
  mutate_all(as.character) %>% 
  replace(is.na(.), "") %>% 
  arrange(random_fixed, desc(BW), effect) %>% 
  mutate(effect = str_replace(effect, "on", "\u2192"),
         effect = str_replace(effect, "with", "\u2194"))


# create table ------------------------------------------------------------


# create indices for fixed/random and within/between
n_within_fixed_sens <- nrow(filter(combined, random_fixed == "fixed", BW == "Within"))
n_between_fixed_sens <- nrow(filter(combined, random_fixed == "fixed", BW == "Between"))

# deselect variables that indicate random/fixed and between/within
combined <- combined %>% 
  rename(" " = effect) %>% 
  select(-random_fixed, -BW)

# format table
models_table_sens <- combined %>% 
  kable(caption = "Table. Model Parameters of the Fixed Effects Models for Sensitivity Analyses", 
        align = "lllcc",
        escape = F) %>%
  kable_classic(full_width = F, html_font = "") %>% 
  pack_rows(index = c("Within-person" = n_within_fixed_sens, 
                      "Between-person" = n_between_fixed_sens)) %>% 
  footnote(general_title = "Note.",
           general = "bs are unstandardized effects. βs are standardized effects using STDY for the categorical and STDYX for the continuous variables. p-values represent one-tailed Bayesian p-values (McNeish & Hamaker, 2020).",
           symbol = c("p < .05.", "p < .01.", "p < .001."),
           symbol_manual = c("$^{*}$", "$^{**}$", "$^{***}$"),
           footnote_as_chunk = TRUE)

models_table_sens
