---
## Maud Westendorp
## October 2023
## Preprocessing script WP1 perception task MultiLit
---

######### Setup #########
# Load libraries:
library(pacman)
library(tidyverse)
library(jsonlite)

# Set path for main analysis:
mainDir <- "~/Dropbox/MultiLit/MultiLitExperiment/Perception"
setwd(mainDir)

### Load data:
# Read the text file from JATOS
read_file('./Porsgrunn/BMNN/list1.txt') %>%
    #  split it into lines 
    str_split('\n') %>% first() %>%
    # filter empty rows 
    discard(function(x) x == '') %>%
    # parse JSON into a data.frame
    map_dfr(fromJSON, flatten=T) -> jatos

# Get the participant number out
jatos <- jatos %>%
    # select only code_number input from survey and target trials
    filter(trial_type == "survey-html-form" | task == "button_response") %>% 
    # chop df into chuncks of 49 rows
    group_by(chunk = rep(1:(n() %/% 49), each = 49)) %>%
    # make a column for partipant number based on survey input
    mutate(participant = first(response)) %>%
    # remove grouping structure
    ungroup() %>%
    # remove "chunck" column
    select(-chunk)

# Repeat for all lists in the experiment (x3):
read_file('./Porsgrunn/BMNN/list2.txt') %>%
    str_split('\n') %>% first() %>%
    discard(function(x) x == '') %>%
    map_dfr(fromJSON, flatten=T) -> data2

data2 <- data2 %>%
    filter(trial_type == "survey-html-form" | task == "button_response") %>% 
    group_by(chunk = rep(1:(n() %/% 49), each = 49)) %>%
    mutate(participant = first(response)) %>%
    ungroup() %>%
    select(-chunk)

jatos <- rbind(jatos, data2)

read_file('./Porsgrunn/BMNN/list3.txt') %>%
    str_split('\n') %>% first() %>%
    discard(function(x) x == '') %>%
    map_dfr(fromJSON, flatten=T) -> data2

data2 <- data2 %>%
    filter(trial_type == "survey-html-form" | task == "button_response") %>%
    group_by(chunk = rep(1:(n() %/% 49), each = 49)) %>%
    mutate(participant = first(response)) %>%
    ungroup() %>%
    select(-chunk)
jatos <- rbind(jatos, data2)

read_file('./Porsgrunn/BMNN/list4.txt') %>%
    str_split('\n') %>% first() %>%
    discard(function(x) x == '') %>%
    map_dfr(fromJSON, flatten=T) -> data2

data2 <- data2 %>%
    filter(trial_type == "survey-html-form" | task == "button_response") %>% 
    group_by(chunk = rep(1:(n() %/% 49), each = 49)) %>%
    mutate(participant = first(response)) %>%
    ungroup() %>%
    select(-chunk)
jatos <- rbind(jatos, data2)

######### Data cleaning #########
# Create dataframe from JSON data:
east <- jatos %>% 
  # keep only the target trials
  filter(task == "button_response") %>% 
  # take the participant code out of its "list"
  unnest(cols = participant) %>% 
  unnest(cols = response) %>% 
  # keep only the useful columns
  select(participant, rt, response, correct_response, correct, item, 
         condition, sound, sentence, list)

# Make sure the columns have the right structure:
east <- east %>% 
  mutate_at(vars(participant, condition, response, correct_response),
              as.factor) %>% 
  mutate_at(vars(rt), as.numeric)

# Mutate some stuff to clear up conditions:
east <- mutate(east,
              response = recode(response,
                                "0" = "mismatch",
                                "1" = "match"),
              correct_response = recode(correct_response,
                                 "0" = "mismatch",
                                 "1" = "match"))

east <- east %>%
     mutate(location="East",
            .before=participant)

            
######### Save and write #########
setwd(mainDir)
write.table(east, "./Porsgrunn/BMNN_east_clean.csv", sep = ',', row.names = F)
