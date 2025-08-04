rm(list = ls())
library(here)
library(dplyr)
# load data
d_raw <- read.csv(here("data/dutilhetal-09/raw/gillesdutilh.com_materials_DutilhPB&R2009_practice_data.txt"), sep = " ")

d_clean <- d_raw %>%
  # Only include first session (first 5 blocks) & RTs less than 10 seconds  
  filter(block <= 5 & RT < 10000) %>% 
  group_by(subj) %>% 
  mutate(
    # Rename RT and convert to seconds  
    Time = RT / 1000,
    Trial = row_number(),  # Compute cumulative trial number within each subject  
    # For our code, stim actually needs to be wnw (word/non-word)  
    Stim = wnw,
    Resp = correct + 1,  # Adjust so that 2 is correct, 1 is incorrect  
    Cond = 1,
    Emphasis = ifelse(substr(subj, 1, 1) == "S", "Speed", "Accuracy")
  ) %>%
  rename(
    "Subj" = subj,
    "Block" = block
  ) %>%
  select(
    Subj,
    Block,
    Trial,
    Time,
    Resp,
    Stim,
    Cond,
    Emphasis
  ) %>%
  ungroup()  # Ensure calculations don’t persist across groups  



# Figure out whether their were any bad subject (accuracy < .7)
subj_accuracy <- d_clean %>%
  group_by(Subj) %>%
  summarise(n = n(), accuracy = sum(Resp-1)/n )

bad_subs = filter(subj_accuracy, accuracy < .7) %>%
  select(Subj) %>%
  unlist()

# remove bad subs if there are any
if(length(bad_subs > 0)){
  d_clean <- d_clean %>%
    filter(!Subj %in%bad_subs )
}

# save a different data set for each subject
subs <- unique(d_clean$Subj)
n_subs <- length(subs)

for (i in 1:n_subs) {
  data <- NULL
  sub <- subs[i]
  data <- d_clean %>%
    filter(Subj == sub)
  save(data, file = here(paste0("data/dutilhetal-09/clean/P",i,".Rdata")))
}

