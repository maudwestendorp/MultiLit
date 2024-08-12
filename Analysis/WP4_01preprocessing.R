---
title: "WP4 (North) - Preprocessing script"
author: "Maud Westendorp"
date: "May 2023"
---

############ Load in data and packages: #########
# Set options:
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)

# Path for main analysis:
mainDir <- "~/Dropbox/MultiLit/MultiLitExperiment/WP4_CognateFlanker"
setwd(mainDir)

# Load main data:
jatos <- read_csv2("./data_north/North.csv")

############ Cleaning main data frame: #########
# Select relevant columns:
data <- jatos %>% dplyr::select(Flanker_condition, Flanker_correct, LD_condition, 
                                LD_correct, LD_nr, LD_stimulus, bm_response,
                                code_number, condition, practice, procedure,
                                jatosStudyResultId, list,
                                response_Flanker_response, response_LD_response,
                                response_time_Flanker_response, 
                                response_time_LD_response,
                                loopcount)

# Make column names comprehensible and/or shorter:
data <- rename(data,
               participant = code_number,
               F_resp = response_Flanker_response,
               F_direction = Flanker_correct,
               LD_language = LD_correct,
               F_condition = Flanker_condition,
               LD_resp_button = response_LD_response,
               F_RT = response_time_Flanker_response,
               LD_RT = response_time_LD_response,
               order_exp = loopcount)

# Drop practice round items:
data <- filter(data,
               practice == "no")

# Make "None" responses NA
data <- data %>%
  mutate(F_resp = 
           na_if(F_resp, "None")) %>% 
  mutate(LD_resp_button =
           na_if(LD_resp_button, "None")) %>%
  mutate(LD_resp_button =
           na_if(LD_resp_button, "")) %>% 
  mutate(F_RT = 
           na_if(F_RT, "None")) %>% 
  mutate(LD_RT =
           na_if(LD_RT, "None"))

# Make sure Flanker correct_responses are lower case:
data <- mutate(data,
              F_direction = tolower(F_direction),
              F_condition = tolower(F_condition),
              condition = tolower(condition))

# Change cleaned df into 'FLD' data frame:
FLD <- data


############ Clean up FLD-data: #########
# Determine which part of the experiment responses are from:
FLD <- FLD %>% 
    mutate(part = case_when(
               order_exp > 200 & order_exp < 301  ~ 'part3',
               order_exp > 100 & order_exp < 201  ~ 'part2',
               order_exp < 101  ~ 'part1')
        )

# Change LD_condition to remove redundant information:
FLD <- FLD %>% 
    mutate(LD_condition = case_when(
               LD_condition == "fill_cog" ~ "cog",
               LD_condition == "fill_noncog" ~ "noncog",
               LD_condition == "fill_ident" ~ "ident",
               TRUE ~ as.character(LD_condition))
        )

# Assign correct responses '1' and incorrect responses '0': 
FLD <- FLD %>% 
    mutate(correct.F_resp = case_when(
              procedure != "LD_loop" & F_resp == F_direction ~ 1,
              procedure != "LD_loop" & F_resp != F_direction ~ 0)
              ) %>% 
    mutate(correct.LD_resp = case_when(
              procedure != "Flanker_loop" & LD_condition == "ident" & !is.na(LD_resp_button) ~ 1,
              procedure != "Flanker_loop" & LD_resp_button == LD_language ~ 1,
              procedure != "Flanker_loop" & LD_resp_button != LD_language ~ 0)
              ) 

# Change LD responses to language:
FLD <- FLD %>% 
    mutate(LD_resp = case_when(
               bm_response == "left" & LD_resp_button == "left" ~ 'BM',
               bm_response == "left" & LD_resp_button == "right" ~ 'NN',
               bm_response == "right" & LD_resp_button == "right" ~ 'BM',
               bm_response == "right" & LD_resp_button == "left" ~ 'NN')
        )

# Change "LD_correct" to language:
FLD <- FLD %>% 
    mutate(LD_corr_button = LD_language,
           LD_language = case_when(
               bm_response == "left" & LD_corr_button == "left" ~ 'BM',
               bm_response == "left" & LD_corr_button == "right" ~ 'NN',
               bm_response == "right" & LD_corr_button == "right" ~ 'BM',
               bm_response == "right" & LD_corr_button == "left" ~ 'NN')
        )

# Tag trials without any response:
FLD <- FLD %>% 
  mutate(no_resp = case_when(
    is.na(F_resp) & procedure == "Flanker_loop" ~ "yes",
    is.na(LD_resp) & procedure == "LD_loop" ~ "yes",
    (is.na(LD_resp) | is.na(F_resp)) & procedure == "Target_loop" ~ "yes")) %>% 
  mutate(no_resp = ifelse(
    is.na(no_resp), "no", no_resp))

########### Drop participants without (realistic) code_number (n = 2) or with incomplete data (n = 0): #########
# Calculate number of unique participants:
n_distinct(FLD$participant)

# Drop participants without code (or with impossible ones) (n = 2):
FLD <- filter(FLD,
              participant > 100,
              participant < 990)

# Drop participants that have not completed even the first part of the exp (n = 0):
drop.incomplete <- FLD %>%
        group_by(participant) %>% 
        count() %>% 
        filter(n < 100)

# Join tibble to main df and drop participants that do not have 300 trails:
FLD <- left_join(FLD, drop.incomplete)

FLD <- filter(FLD, 
              is.na(n))

FLD$n <- NULL

########### Drop participants too many non-responses (n = 12): #########
# Calculate number of unique participants:
n_distinct(FLD$participant)

# Determine which participants failed to respond (in time) on >20% of Flanker trials:
drop.F_na <- FLD %>%
      filter(procedure != "LD_loop") %>% 
        group_by(participant) %>% 
        count(correct.F_resp) %>% 
        mutate(prop = n/sum(n),
          prop = round(prop, digits = 3)) %>% 
      filter(is.na(correct.F_resp)) %>% 
        select(participant, prop) %>% 
        filter(prop > 0.199) %>% 
       rename(prop.F_na = prop)

# Join tibble to main df and drop participants with more than 20% NA (n = 8, prop = 0.201/.417/.508):
FLD <- left_join(FLD, drop.F_na)

FLD <- filter(FLD, 
              is.na(prop.F_na))

FLD$prop.F_na <- NULL  

# Determine which participants failed to respond (in time) on >20% of LD trials:
drop.LD_na <- FLD %>%
      filter(procedure != "Flanker_loop") %>% 
        group_by(participant) %>% 
        count(correct.LD_resp) %>% 
        mutate(prop = n/sum(n),
          prop = round(prop, digits = 3)) %>% 
      filter(is.na(correct.LD_resp)) %>% 
        select(participant, prop) %>% 
        filter(prop > 0.199) %>% 
       rename(prop.LD_na = prop)

# Join tibble to main df and drop participants with more than 20% NA (n = 4, prop = .204/.204/.338/.725):
FLD <- left_join(FLD, drop.LD_na)

FLD <- filter(FLD, 
              is.na(prop.LD_na))

FLD$prop.LD_na <- NULL

# Calculate number of unique participants:
n_distinct(FLD$participant)

# Do the above for both Lists in the experiment and then bind the 2 dataframes:
# Calculate number of participants per counter-balanced list (and button allocation):
FLD %>% group_by(bm_response, list) %>% 
  summarise(count = n_distinct(participant)) %>% 
  arrange(list)

######### Re-order all columns and write data: #########
FLD <- select(FLD,
              participant, list, bm_response, part, 
              order_exp, no_resp, procedure, condition,
              F_condition, F_direction,
              F_resp, F_RT, correct.F_resp,
              LD_nr, LD_stimulus, LD_condition, LD_language,
              LD_corr_button,LD_resp_button, LD_resp, 
              correct.LD_resp, LD_RT)

setwd(mainDir)
write.table(FLD, "./data_north/FLD_north_clean.csv", sep = ',', row.names = F)
