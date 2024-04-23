library(stats) 
library(dplyr) 
library(ggplot2) 
library(tidyr) 
library(patchwork)

#import the spawn raw logs (and fix the errors)
{
  setwd("/Users/anastasia/Desktop/ D.S.logs/Spawn/Spawn logs")
  
  Spawn_1D <- read.csv("logSpawn1D.csv")
  Spawn_1D$Subject <- "S1_T"
  Spawn_3D <- read.csv("logSpawn3D.csv")
  Spawn_3D$Subject <- "S3_T"
  Spawn_5D <- read.csv("logSpawn5D.csv")
  Spawn_5D$Subject <- "S5_T"
  Spawn_8D <- read.csv("logSpawn8D.csv")
  Spawn_8D$Subject <- "S8_T"
  
  ###NON-TERRITORIAL
  Spawn_1ND <- read.csv("logSpawn1ND.csv")
  Spawn_1ND$Subject <- "S1_NT"
  Spawn_3ND <- read.csv("logSpawn3ND.csv")
  Spawn_3ND$Subject <- "S3_NT"
  Spawn_5ND <- read.csv("logSpawn5ND.csv")
  Spawn_5ND$Subject <- "S5_NT"
  Spawn_8ND <- read.csv("logSpawn8ND.csv")
  Spawn_8ND$Subject <- "S8_NT"
  
  #Fix the Spawn time stamps
  {
    # Function to add minutes to time in mm:ss.0 format
    add_minutes <- function(time_string, minutes) {
      time_in_seconds <- as.numeric(as.POSIXct(time_string, format = "%M:%OS")) + minutes * 60
      milliseconds <- round((time_in_seconds - floor(time_in_seconds)) * 10)
      milliseconds <- ifelse(milliseconds == 10, 0, milliseconds) # Round up to 0 if 
      # milliseconds reach 10
      updated_time <- format(as.POSIXct(time_in_seconds, origin = "1970-01-01"), format = "%M:%OS")
      return(paste0(updated_time, ".", milliseconds))
    }
    
    
    #Territorial
    #Spawn_1D
    {
      # Add 09:55.3 to rows 51-62 in mm:ss.0 format
      fixed_time1 <- "09:55.3"
      for (i in 51:62)
        Spawn_1D$Time[i] <- add_minutes(Spawn_1D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time1, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      
      
      #Add 19:02.5 to rows 63-143 in mm:ss.0 format
      fixed_time2 <- "19:02.5" 
      for (i in 63:143) {
        Spawn_1D$Time[i] <- add_minutes(Spawn_1D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time2, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }
    #Spawn_3D
    { 
      # Update rows 39 to 68 (add 09:59.9 minutes)
      fixed_time3 <- "09:59.9" 
      for (i in 39:68) {
        Spawn_3D$Time[i] <- add_minutes(Spawn_3D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time3, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      # Update rows 69 to 145 (add 19:48.9 minutes)
      fixed_time4 <- "19:48.9" 
      for (i in 69:145) {
        Spawn_3D$Time[i] <- add_minutes(Spawn_3D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time4, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }
    #Spawn_5D
    {
      # Update rows 15 to 44 (add 09:42.3 minutes)
      fixed_time5 <- "09:42.3" 
      for (i in 15:44) {
        Spawn_5D$Time[i] <- add_minutes(Spawn_5D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time5, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      # Update rows 45 to 205 (add 19:36.8 minutes)
      fixed_time6 <- "19:36.8" 
      for (i in 45:205) {
        Spawn_5D$Time[i] <- add_minutes(Spawn_5D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time6, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }
    #Spawn_8D
    {
      # Update rows 22 to 168 (add 09:47.0 minutes)
      fixed_time7 <- "09:47.0" 
      for (i in 22:168) {
        Spawn_8D$Time[i] <- add_minutes(Spawn_8D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time7, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      # Update rows 169 to 261 (add 19:52.7 minutes)
      fixed_time8 <- "19:52.7" 
      for (i in 169:261) {
        Spawn_8D$Time[i] <- add_minutes(Spawn_8D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time8, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }
    
    #Non-Territorial
    #Spawn_1ND
    {
      # Add 09:57.1 to rows 103-340 in mm:ss.0 format
      fixed_time9 <- "09:57.1"
      for (i in 103:340) {
        Spawn_1ND$Time[i] <- add_minutes(Spawn_1ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time9, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      #Add 19:53.9 to rows 341-509 in mm:ss.0 format
      fixed_time10 <- "19:53.9" 
      for (i in 341:509) {
        Spawn_1ND$Time[i] <- add_minutes(Spawn_1ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time10, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }
    #Spawn_3ND
    { 
      # Update rows 52 to 202 (add 10:00.0 minutes)
      fixed_time11 <- "10:00.0" 
      for (i in 52:202) {
        Spawn_3ND$Time[i] <- add_minutes(Spawn_3ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time11, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      # Update rows 203 to 340 (add 19:58.3 minutes)
      fixed_time12 <- "19:58.3" 
      for (i in 203:340) {
        Spawn_3ND$Time[i] <- add_minutes(Spawn_3ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time12, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }
    #Spawn_5ND
    {
      # Update rows 26 to 57 (add 09:19.9 minutes)
      fixed_time13 <- "09:19.9" 
      for (i in 26:57) {
        Spawn_5ND$Time[i] <- add_minutes(Spawn_5ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time13, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      # Update rows 58 to 119 (add 19:13.8 minutes)
      fixed_time14 <- "19:13.8" 
      for (i in 58:119) {
        Spawn_5ND$Time[i] <- add_minutes(Spawn_5ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time14, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }  
    #Spawn_8ND
    {
      # Update rows 38 to 132 (add 10:00.4 minutes)
      fixed_time15 <- "10:00.4" 
      for (i in 38:132) {
        Spawn_8ND$Time[i] <- add_minutes(Spawn_8ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time15, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      # Update rows 133 to 202 (add 19:55.2 minutes)
      fixed_time16 <- "19:55.2" 
      for (i in 133:202) {
        Spawn_8ND$Time[i] <- add_minutes(Spawn_8ND$Time[i], 
                                         as.numeric(difftime(as.POSIXct(fixed_time16, format = "%M:%OS"), 
                                                             as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
    }
    
    # Combine the dataframes
    Spawn_T <- rbind(Spawn_1D,Spawn_3D,Spawn_5D,Spawn_8D)
    colnames(Spawn_T) <- c("Time", "Behavior","TankID")
    Spawn_T$Assay <- "Spawn"
    Spawn_T$Subject <- "Territorial"
    
    Spawn_NT <- rbind(Spawn_1ND,Spawn_3ND,Spawn_5ND,Spawn_8ND)
    colnames(Spawn_NT) <- c("Time", "Behavior","TankID")
    Spawn_NT$Assay <- "Spawn"
    Spawn_NT$Subject <- "Non_Territorial"
    
    AllSpawn <- rbind(Spawn_T,Spawn_NT)
  }
  # fix the logging errors in AllSpawn
  {
    #check for behaviors in question
    {
      #["chase male"] 
      
      value_to_check1 <- "chase male" 
      is_present <- value_to_check1 %in% AllSpawn$Behavior 
      
      if (is_present) {
        print(paste(value_to_check1, "is present in column Behavior"))
      } else {
        print(paste(value_to_check1, "is not present in column Behavior"))
      }
      
      #["attack male"]
      value_to_check2 <- "attack male"
      is_present <- value_to_check2 %in% AllSpawn$Behavior
      
      if (is_present) {
        print(paste(value_to_check2, "is present in column Behavior"))
      } else {
        print(paste(value_to_check2, "is not present in column Behavior"))
      }
      
      #["quiver at male"]
      value_to_check3 <- "quiver at male" 
      is_present <- value_to_check3 %in% AllSpawn$Behavior
      
      if (is_present) {
        print(paste(value_to_check3, "is present in column Behavior"))
      } else {
        print(paste(value_to_check3, "is not present in column Behavior"))
      }
      
      #["flee from male"] 
      value_to_check4 <- "flee from male" 
      is_present <- value_to_check4 %in% AllSpawn$Behavior
      
      if (is_present) {
        print(paste(value_to_check4, "is present in column Behavior"))
      } else {
        print(paste(value_to_check4, "is not present in column Behavior"))
      }
    }
    # replace "male" with "female"
    {
      #["chase male"] - Row 1192
      # Value to replace and the new value
      value_to_replace1 <- "chase male"
      new_value1 <- "chase female"
      
      # Find rows where the value is "orange" and replace with "pear"
      rows_to_replace1 <- which(AllSpawn$Behavior == value_to_replace1)
      AllSpawn$Behavior[rows_to_replace1] <- new_value1
      
      
      #["attack male"] - Rows 26  812  886 1022 1357 1415 1454 1598 1767
      
      # Value to replace and the new value
      value_to_replace2 <- "attack male"
      new_value2 <- "attack female"
      
      # Find rows where the value is "orange" and replace with "pear"
      rows_to_replace2 <- which(AllSpawn$Behavior == value_to_replace2)
      AllSpawn$Behavior[rows_to_replace2] <- new_value2
    }
  }
}

SpawnT <- subset(AllSpawn, Subject == "Territorial")
SpawnNT <- subset(AllSpawn, Subject == "Non_Territorial")

#T
{
  # Get unique TankIDs
  unique_tank_ids <- unique(SpawnT$TankID)
  
  # Create separate dataframes for each TankID
  for (tank_id in unique_tank_ids) {
    df_name <- paste0("Tank_", tank_id)
    assign(df_name, SpawnT %>% filter(TankID == tank_id))}
  
  Tank_S1_T <- Tank_S1_T %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S1_T <- Tank_S1_T %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S1_T$TankID <- "D1_T"
  
  Tank_S3_T <- Tank_S3_T %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S3_T <- Tank_S3_T %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S3_T$TankID <- "D3_T"
  
  Tank_S5_T <- Tank_S5_T %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S5_T <- Tank_S5_T %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S5_T$TankID <- "D5_T"
  
  Tank_S8_T <- Tank_S8_T %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S8_T <- Tank_S8_T %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S8_T$TankID <- "D8_T"
  
  All_raw_T <- rbind(Tank_S1_T,Tank_S3_T,Tank_S5_T,Tank_S8_T)
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
  #write.csv(behavior_data, "SpawnT.transition.perc.csv", row.names = TRUE)
  
  behavior_transitions_T <- rbind(behavior_transitions_Tank_S1_T,behavior_transitions_Tank_S3_T,behavior_transitions_Tank_S5_T,behavior_transitions_Tank_S8_T)
  
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
  # Get unique TankIDs
  unique_tank_ids <- unique(SpawnNT$TankID)
  
  # Create separate dataframes for each TankID
  for (tank_id in unique_tank_ids) {
    df_name <- paste0("Tank_", tank_id)
    assign(df_name, SpawnNT %>% filter(TankID == tank_id))}
  
  
  Tank_S1_NT <- Tank_S1_NT %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S1_NT <- Tank_S1_NT %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S1_NT$TankID <- "D1_NT"
  
  Tank_S3_NT <- Tank_S3_NT %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S3_NT <- Tank_S3_NT %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S3_NT$TankID <- "D3_NT"
  
  Tank_S5_NT <- Tank_S5_NT %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S5_NT <- Tank_S5_NT %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S5_NT$TankID <- "D5_NT"
  
  Tank_S8_NT <- Tank_S8_NT %>%
    mutate(NextBehavior = lead(Behavior)) %>%
    filter(!is.na(NextBehavior))
  
  #Count the transitions within each TankID
  behavior_transitions_Tank_S8_NT <- Tank_S8_NT %>%
    count(Behavior, NextBehavior, name = "CountWithinTank")
  behavior_transitions_Tank_S8_NT$TankID <- "D8_NT"
  
  All_raw_NT <- rbind(Tank_S1_NT,Tank_S3_NT,Tank_S5_NT,Tank_S8_NT)
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
  #write.csv(behavior_data, "SpawnNT.transition.perc.csv", row.names = TRUE)
  
  behavior_transitions_NT <- rbind(behavior_transitions_Tank_S1_NT,behavior_transitions_Tank_S3_NT,behavior_transitions_Tank_S5_NT,behavior_transitions_Tank_S8_NT)
  
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
  
  #write.csv(combined_behavior_transitions, "FULLSpawn.trrans.prob.csv", row.names = TRUE)
}  