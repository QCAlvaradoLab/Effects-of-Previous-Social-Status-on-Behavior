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
  Dyad_NT$Subject <- "Non_territorial"
  
  AllDyad <- rbind(Dyad_NT, Dyad_T)
  #write.csv(AllDyad, "AllDyad.csv", row.names = TRUE)
}
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
    Spawn_NT$Subject <- "Non_territorial"
    
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
All <- rbind(AllDyad,AllSpawn)

#T
{
Territorials <- subset(All, Subject == "Territorial")

# Create the Incidence_tank dataframe: how may times each behavior accurred in each tank
Incidence_tank <- Territorials %>%
  group_by(TankID, Behavior,Assay) %>%
  summarise(Count = n(), .groups = "drop")

# Calculate total behaviors for each TankID : how many behaviors occurred in total in each tank 
total_behaviors_per_tank <- Territorials %>%
  group_by(TankID) %>%
  summarise(TotalBehaviors = n(), .groups = "drop")

# Join this total back to the Incidence_tank dataframe
Incidence_tank <- Incidence_tank %>%
  left_join(total_behaviors_per_tank, by = "TankID")

# Add the relative_incidence column to the Incidence_tank dataframe: #of each behavior in a tank/ total # of behaviors in that tank
Incidence_tank <- Incidence_tank %>%
  mutate(relative_incidence = Count / TotalBehaviors)
#write.csv(Incidence_tank, "relative.incidence.csv", row.names = TRUE)

# Calculate mean and SEM for each behavior
mean_sem_perc <- Incidence_tank %>%
  group_by(Behavior,Assay) %>%
  summarise(
    Mean = mean(relative_incidence, na.rm = TRUE),
    SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
    .groups = "drop"
  )
# Create a dataframe with all combinations of TankIDs and behaviors
complete_data <- expand.grid(TankID = unique(Incidence_tank$TankID),
                             Behavior = unique(Incidence_tank$Behavior))

# Perform a full join to ensure all combinations are present
complete_incidence <- full_join(Incidence_tank, complete_data, by = c("TankID", "Behavior"))

# Fill missing values with 0
complete_incidence <- replace_na(complete_incidence, list(TotalBehaviors = 0, relative_incidence = 0))

complete_incidence <- complete_incidence %>%
  mutate(Assay = ifelse(grepl("^D", TankID), "Dyad", 
                          ifelse(grepl("^S", TankID), "Spawn", "Unknown")))
complete_incidence <- complete_incidence %>%
  filter(!grepl("attack male|chase male|flee from male|quiver at male", Behavior))



# Perform paired t-tests for each behavior
results <- lapply(unique(complete_incidence$Behavior), function(behavior) {
  t_test_result <- pairwise.t.test(
    complete_incidence$relative_incidence[complete_incidence$Behavior == behavior],
    complete_incidence$Assay[complete_incidence$Behavior == behavior],
    paired = TRUE
  )
  # Extract the p-values
  p_values <- t_test_result$p.value
  
  # Apply Benjamini-Hochberg correction
  p_adjusted <- p.adjust(p_values, method = "BH")
  
  # Store the results in a data frame
  data.frame(
    Behavior = behavior,
    P_Value = p_values,
    Adjusted_P_Value = p_adjusted
  )
})

# Combine the results into a single dataframe
results_df <- do.call(rbind, results)

# Print the results
print(results_df)
}
#NT
{
  Non_Territorials <- subset(All, Subject == "Non_territorial")
  
  # Create the Incidence_tank dataframe: how may times each behavior accurred in each tank
  Incidence_tank <- Non_Territorials %>%
    group_by(TankID, Behavior,Assay) %>%
    summarise(Count = n(), .groups = "drop")
  
  # Calculate total behaviors for each TankID : how many behaviors occurred in total in each tank 
  total_behaviors_per_tank <- Non_Territorials %>%
    group_by(TankID) %>%
    summarise(TotalBehaviors = n(), .groups = "drop")
  
  # Join this total back to the Incidence_tank dataframe
  Incidence_tank <- Incidence_tank %>%
    left_join(total_behaviors_per_tank, by = "TankID")
  
  # Add the relative_incidence column to the Incidence_tank dataframe: #of each behavior in a tank/ total # of behaviors in that tank
  Incidence_tank <- Incidence_tank %>%
    mutate(relative_incidence = Count / TotalBehaviors)
  #write.csv(Incidence_tank, "relative.incidence.csv", row.names = TRUE)
  
  # Calculate mean and SEM for each behavior
  mean_sem_perc <- Incidence_tank %>%
    group_by(Behavior,Assay) %>%
    summarise(
      Mean = mean(relative_incidence, na.rm = TRUE),
      SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
      .groups = "drop"
    )
  # Create a dataframe with all combinations of TankIDs and behaviors
  complete_data <- expand.grid(TankID = unique(Incidence_tank$TankID),
                               Behavior = unique(Incidence_tank$Behavior))
  
  # Perform a full join to ensure all combinations are present
  complete_incidence <- full_join(Incidence_tank, complete_data, by = c("TankID", "Behavior"))
  
  # Fill missing values with 0
  complete_incidence <- replace_na(complete_incidence, list(TotalBehaviors = 0, relative_incidence = 0))
  
  complete_incidence <- complete_incidence %>%
    mutate(Assay = ifelse(grepl("^D", TankID), "Dyad", 
                          ifelse(grepl("^S", TankID), "Spawn", "Unknown")))
  complete_incidence <- complete_incidence %>%
    filter(!grepl("attack male|chase male|flee from male|quiver at male", Behavior))
  
  

# Perform paired t-tests for each behavior
results <- lapply(unique(complete_incidence$Behavior), function(behavior) {
  t_test_result <- pairwise.t.test(
    complete_incidence$relative_incidence[complete_incidence$Behavior == behavior],
    complete_incidence$Assay[complete_incidence$Behavior == behavior],
    paired = TRUE
  )
  # Extract the p-values
  p_values <- t_test_result$p.value
  
  # Apply Benjamini-Hochberg correction
  p_adjusted <- p.adjust(p_values, method = "BH")
  
  # Store the results in a data frame
  data.frame(
    Behavior = behavior,
    P_Value = p_values,
    Adjusted_P_Value = p_adjusted
  )
})

# Combine the results into a single dataframe
results_df <- do.call(rbind, results)

# Print the results
print(results_df)
}