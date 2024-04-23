library(stats) 
library(dplyr) 
library(ggplot2) 
library(tidyr) 
library(patchwork)

#import the dyad raw logs
{
  setwd("/Users/anastasia/Desktop/ D.S.logs/Dyad/Dyad logs")
  
  Dyad_1D <- read.csv("logDyad1D.csv")
  Dyad_1D$Subject <- "D1_T"
  Dyad_3D <- read.csv("logDyad3D.csv")
  Dyad_3D$Subject <- "D3_T"
  Dyad_5D <- read.csv("logDyad5D.csv")
  Dyad_5D$Subject <- "D5_T"
  Dyad_8D <- read.csv("logDyad8D.csv")
  Dyad_8D$Subject <- "D8_T"
  
  Dyad_T <- rbind(Dyad_1D,Dyad_3D,Dyad_5D,Dyad_8D)
  colnames(Dyad_T) <- c("Time", "Behavior","TankID")
  Dyad_T$Assay <- "Dyad"
  Dyad_T$Subject <- "Territorial"
  
  Dyad_1ND <- read.csv("logDyad1ND.csv")
  Dyad_1ND$Subject <- "D1_NT"
  Dyad_3ND <- read.csv("logDyad3ND.csv")
  Dyad_3ND$Subject <- "D3_NT"
  Dyad_5ND <- read.csv("logDyad5ND.csv")
  Dyad_5ND$Subject <- "D5_NT"
  Dyad_8ND <- read.csv("logDyad8ND.csv")
  Dyad_8ND$Subject <- "D8_NT"
  
  Dyad_NT <- rbind(Dyad_1ND,Dyad_3ND,Dyad_5ND,Dyad_8ND)
  colnames(Dyad_NT) <- c("Time", "Behavior","TankID")
  Dyad_NT$Assay <- "Dyad"
  Dyad_NT$Subject <- "Non-territorial"
  
  AllDyad <- rbind(Dyad_NT, Dyad_T)
  #write.csv(AllDyad, "AllDyad.csv", row.names = TRUE)
}

## Transitions ##
#T
{
Dyad_1D <- Dyad_1D %>%
  mutate(NextBehavior = lead(Behavior)) %>%
  filter(!is.na(NextBehavior))

#Count the transitions within each TankID
behavior_transitions_Dyad_1D <- Dyad_1D %>%
  count(Behavior, NextBehavior, name = "CountWithinTank")
behavior_transitions_Dyad_1D$TankID <- "D1_T"

Dyad_3D <- Dyad_3D %>%
  mutate(NextBehavior = lead(Behavior)) %>%
  filter(!is.na(NextBehavior))

#Count the transitions within each TankID
behavior_transitions_Dyad_3D <- Dyad_3D %>%
  count(Behavior, NextBehavior, name = "CountWithinTank")
behavior_transitions_Dyad_3D$TankID <- "D3_T"

Dyad_5D <- Dyad_5D %>%
  mutate(NextBehavior = lead(Behavior)) %>%
  filter(!is.na(NextBehavior))

#Count the transitions within each TankID
behavior_transitions_Dyad_5D <- Dyad_5D %>%
  count(Behavior, NextBehavior, name = "CountWithinTank")
behavior_transitions_Dyad_5D$TankID <- "D5_T"

Dyad_8D <- Dyad_8D %>%
  mutate(NextBehavior = lead(Behavior)) %>%
  filter(!is.na(NextBehavior))

#Count the transitions within each TankID
behavior_transitions_Dyad_8D <- Dyad_8D %>%
  count(Behavior, NextBehavior, name = "CountWithinTank")
behavior_transitions_Dyad_8D$TankID <- "D8_T"

All_raw_T <- rbind(Dyad_1D,Dyad_3D,Dyad_5D,Dyad_8D)
# Count occurrences of each behavior
behavior_counts <- table(All_raw_T$Behavior)
print(behavior_counts)
# Calculate the percentage of each behavior
total_behaviors <- sum(behavior_counts)
print(total_behaviors)
behavior_percentages <- prop.table(behavior_counts) * 100
print(behavior_percentages)
# Create a dataframe to store count and percentage
behavior_data <- data.frame(
  Behavior = names(behavior_counts),
  Count = as.numeric(behavior_counts),
  Percentage = as.numeric(behavior_percentages)
)
print(behavior_data)
#write.csv(behavior_data, "DyadT.transition.perc.csv", row.names = TRUE)
          
behavior_transitions_T <- rbind(behavior_transitions_Dyad_1D,behavior_transitions_Dyad_3D,behavior_transitions_Dyad_5D,behavior_transitions_Dyad_8D)

behavior_transitions_T <- behavior_transitions_T %>%
  group_by(Behavior, NextBehavior) %>%
  summarize(TotalCount = sum(CountWithinTank))

behavior_transitions_T <- behavior_transitions_T %>%
  group_by(Behavior) %>%
  mutate(TotalCount_Behavior = sum(TotalCount)) %>%
  ungroup()

# Create Probability_T column
behavior_transitions_T <- behavior_transitions_T %>%
  mutate(Probability_T = TotalCount / TotalCount_Behavior)
}
#NT
{
  Dyad_1ND <- Dyad_1ND %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Dyad_1ND <- Dyad_1ND %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Dyad_1ND$TankID <- "D1_NT"
  
  Dyad_3ND <- Dyad_3ND %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Dyad_3ND <- Dyad_3ND %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Dyad_3ND$TankID <- "D3_NT"
  
  Dyad_5ND <- Dyad_5ND %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Dyad_5ND <- Dyad_5ND %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Dyad_5ND$TankID <- "D5_NT"
  
  Dyad_8ND <- Dyad_8ND %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Dyad_8ND <- Dyad_8ND %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Dyad_8ND$TankID <- "D8_NT"
  
  All_raw_NT <- rbind(Dyad_1ND,Dyad_3ND,Dyad_5ND,Dyad_8ND)
  # Count occurrences of each behavior
  behavior_counts <- table(All_raw_NT$Behavior)
  print(behavior_counts)
  # Calculate the percentage of each behavior
  total_behaviors <- sum(behavior_counts)
  print(total_behaviors)
  behavior_percentages <- prop.table(behavior_counts) * 100
  print(behavior_percentages)
  # Create a dataframe to store count and percentage
  behavior_data <- data.frame(
    Behavior = names(behavior_counts),
    Count = as.numeric(behavior_counts),
    Percentage = as.numeric(behavior_percentages)
  )
  print(behavior_data)
  #write.csv(behavior_data, "DyadNT.transition.perc.csv", row.names = TRUE)
  
  behavior_transitions_NT <- rbind(behavior_transitions_Dyad_1ND,behavior_transitions_Dyad_3ND,behavior_transitions_Dyad_5ND,behavior_transitions_Dyad_8ND)
  
  behavior_transitions_NT <- behavior_transitions_NT %>%
    group_by(Behavior, NextBehavior) %>%
    summarize(TotalCount = sum(CountWithinTank))
  
  behavior_transitions_NT <- behavior_transitions_NT %>%
    group_by(Behavior) %>%
    mutate(TotalCount_Behavior = sum(TotalCount)) %>%
    ungroup()
  
  # Create Probability_NT column
  behavior_transitions_NT <- behavior_transitions_NT %>%
    mutate(Probability_NT = TotalCount / TotalCount_Behavior)
}
#combine
{
# Perform full join on Behavior and NextBehavior columns
combined_behavior_transitions <- full_join(
  behavior_transitions_T,
  behavior_transitions_NT,
  by = c("Behavior", "NextBehavior")
) %>%
  mutate_all(~replace(., is.na(.), 0))

#write.csv(combined_behavior_transitions, "Full.Dyad.trrans.prob.csv", row.names = TRUE)
}          
