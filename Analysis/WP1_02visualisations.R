---
## Maud Westendorp
## October 2023 (work in progress)
## Descriptive statistics and visualisations WP1 perception
---

######### Set-up ######### 
# Load packages:
library(tidyverse)
library(plotly)
library(stats)
library(lme4)

# Path for main analysis:
mainDir <- "~/Dropbox/MultiLit/MultiLitExperiment/Perception"
setwd(mainDir)

# Declare condition names for graphs:
condition_names <- c(North = "Northern Norway",
                    West = "Western Norway",
                    East = "Eastern Norway")


########## Get data from all locations together #########
# Load main data location #1:
west <- read_csv("./data/data_west.csv")
west$...1 <- NULL

# Add location to df:
west <- west %>%
     mutate(location="West",
            .before=participant)

# Load main data location #2:
north <- read_csv("./data/BMNN_north.csv")
north$...1 <- NULL

# Add location to df:
north <- north %>%
     mutate(location="North",
            .before=participant)

# Load main data location #3:
east <- read_csv("./East/BMNN_east_clean.csv")

# Add location to df:
east <- east %>%
     mutate(location="East",
            .before=participant)

# Change column names to match:
east <- rename(east,
               RT = rt,
               text = sentence,
               stimuli_list = list)

# Combine dfs from locations:
perc <- bind_rows(east, north, west)

# Make sure numeric variables are numeric:
perc$RT <- as.numeric(perc$RT)

# Participants by location:
perc %>% group_by(location) %>% 
        summarise(count = n_distinct(participant))

# Combine location and participant numbers:
perc <- perc %>% mutate(participant = as.character(participant))

perc <- perc %>%
  mutate(participant = case_when(
    location == "North" ~ paste0("N", participant),
    location == "West" ~ paste0("W", participant),
    location == "East" ~ paste0("E", participant),
    TRUE ~ participant  # Keep the original participant code for other values of location
  ))

# Make participant and location variables factor:
perc <- perc %>% mutate_at(vars(participant,
                              location),
                         as.factor)

# Streamline naming conventions:
perc <- perc %>%
  mutate(stimuli_list = case_when(
    stimuli_list == "list1" ~ "1bmnn",
    stimuli_list == "list2" ~ "2bmnn",
    stimuli_list == "list3" ~ "3bmnn",
    stimuli_list == "list4" ~ "4bmnn",
    TRUE ~ stimuli_list),
        response = case_when(
    response == "x" ~ "mismatch",
    response == "m" ~ "match",
    is.na(response) ~ "None",
    TRUE ~ response),
        )

perc$correct <- as.numeric(perc$correct)

# reorder Conditions
perc$condition <- factor(perc$condition, levels=c('BM+East', 'NN+West', 'BM+West', 'NN+East'))


######### Some basic numbers (n = 356 participants) #########
# Get total nr of participants
perc %>% group_by(location) %>%   
  summarise(count = n_distinct(participant))

# Get participants per list
perc %>% group_by(stimuli_list) %>% 
  summarise(count = n_distinct(participant))


#### Filter out participants with too many mistakes #########

# Incorrect percentages mismatch condition per participant
mismatch_perc <- BMNord %>% 
#mismatch_perc <- BMNN %>% 
      filter(correct_response == "mismatch") %>%
      group_by(participant) %>% 
      count(correct) %>% 
      mutate(prop = n/sum(n),
           prop = round(prop, digits = 3)) %>% 
      filter(correct == 0) %>% 
      filter(prop > .5)

# compute tibble with participants with >50% mistakes in mismatch condition
drop_mismatch <- mismatch_perc %>% 
                    select(participant, prop) %>% 
                    filter(prop > 0.499) %>% 
                    rename(mismatch_inc = prop)

#add mismatch percentages to main df
BMNN <- left_join(BMNN, drop_mismatch)

#remove participants with more >50% mismatch mistakes (n = 24, 2 East/10 North/11 West)
BMNN <- filter(BMNN, 
              is.na(mismatch_inc))

BMNN$mismatch_inc <- NULL


############ Correct vs. incorrect responses #########
# Get correct percentages per condition
correct <- perc %>% 
      filter(correct_response == "match") %>% 
      group_by(condition, location) %>% 
      count(correct) %>% 
      mutate(prop = n/sum(n),
           prop = round(prop, digits = 3)) %>% 
      arrange(prop)

#plot data correct vs. incorrect for MATCH 
correct %>% ggplot(aes(x = condition, 
          y = prop*100, 
          fill = factor(correct))) + 
      geom_bar(stat="identity", width = 0.7) +
      labs(x = "", 
           y = "percent", 
           fill = "") +
      theme(legend.position = "bottom") +
      scale_fill_viridis_d(alpha = 0.6, labels = c("incorrect", "correct")) +
      facet_wrap(~location)

#incorrect percentages per item
perc %>%
      select(correct, item, condition, correct_response, participant) %>% 
      group_by(correct_response, condition, item) %>% 
      filter(correct_response == "match") %>% 
      count(correct) %>%
           mutate(prop = n/sum(n),
           prop = round(prop, digits = 3)) %>% 
      filter(correct == 0) %>% 
      arrange(desc(prop))

############ Trim outliers RT and clean up data (z-transform): #########
BMNN_all <- BMNN
BMENG_all <- BMENG
BMNord_all <- BMNord

#remove RTs that are below 200ms
#BMNord <- BMNord %>% 
BMENG <- BMENG %>% 
#BMNN_z <- BMNN %>% 
  filter(RT > 200)

#z-transform RTs per participant
BMNord_z <- BMNord %>% 
#BMENG_z <- BMENG %>% 
BMNN_z <- BMNN_z %>% 
              #filter(correct_response == "match",
              #       correct == "1") %>%
              group_by(participant) %>% 
              mutate(RT_z = c(scale(RT)))


#remove z-transformed RTs that are > 2.5
BMNord_z <-  BMNord_z %>%
#BMENG_z <- BMENG_z %>% 
BMNN_z <- BMNN_z %>% 
  filter(RT_z < 2.5)

######### Response times per condition: #########

# Mean reaction times per condition
BMNN_z %>%
  group_by(location, condition) %>% 
  summarize(M = mean(RT)) %>% 
  arrange(condition)

# RT but only from correct responses
RT_correct <- BMNN_z %>% 
  filter(correct == 1, correct_response == "match") %>% 
  group_by(condition, participant, location) %>% 
  summarise(Mean_Time = mean(RT_z, na.rm = TRUE)) 

# RT but only from correct responses
RT_correct_nonz <- BMNN %>% 
  filter(correct == 1,
#         location != "East",
         correct_response == "match") %>% 
  group_by(condition, location) %>% 
  summarise(Mean_Time = mean(RT, na.rm = TRUE)) 

ggplot(data = RT_correct_nonz, 
       aes(x = location, 
           y = Mean_Time, 
           fill = location)) + 
  geom_boxplot() +
  facet_wrap(~condition, ncol = 4) +
  xlab('') +
  ylab('Reaction time in ms')

RT_correct_nonz %>% 
  #filter(correct_response == "match") %>% 
ggplot(aes(x = condition, 
           y = Mean_Time, 
           fill = location)) + 
 # scale_fill_manual(values = c("#E69F00", "#0072B2")) +
  geom_bar(stat="identity", position=position_dodge()) +
#  facet_wrap(~correct_response, ncol = 4) +
  xlab('') +
  ylab('Reaction time in ms')

# RT but only from correct responses
BMNN_z %>% 
  filter(correct == 1) %>% 
  group_by(participant, condition) %>% 
  ggplot(aes(x = location, 
           y = RT_z,
           fill = location)) + 
  geom_boxplot() +
facet_wrap(~condition, ncol = 4) +
  theme_minimal() +
  xlab('') +
  ylab('z-transformed reaction time') +
  ylim(-2,2)
