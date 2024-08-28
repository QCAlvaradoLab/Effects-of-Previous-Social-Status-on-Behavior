####DATA WRANGLING####
{
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
  library(tidyverse)
  library(readr)
  library(stringr)
  library(data.table)
  library(reshape2)
  library(pwr)
}
{
#Import the old dyad raw logs
{
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
}
#Import the old spawn raw logs (and fix the errors)
{
  # TERRITORIAL
  Spawn_1D <- read.csv("logSpawn1D.csv")
  Spawn_1D$TankID <- "S1_T"
  Spawn_3D <- read.csv("logSpawn3D.csv")
  Spawn_3D$TankID <- "S3_T"
  Spawn_5D <- read.csv("logSpawn5D.csv")
  Spawn_5D$TankID <- "S5_T"
  Spawn_8D <- read.csv("logSpawn8D.csv")
  Spawn_8D$TankID <- "S8_T"
  
  ###NON-TERRITORIAL
  Spawn_1ND <- read.csv("logSpawn1ND.csv")
  Spawn_1ND$TankID <- "S1_NT"
  Spawn_3ND <- read.csv("logSpawn3ND.csv")
  Spawn_3ND$TankID <- "S3_NT"
  Spawn_5ND <- read.csv("logSpawn5ND.csv")
  Spawn_5ND$TankID <- "S5_NT"
  Spawn_8ND <- read.csv("logSpawn8ND.csv")
  Spawn_8ND$TankID <- "S8_NT"
  
  # Fix the Spawn time stamps
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
      # Update rows 22 to 169 (add 09:47.0 minutes)
      fixed_time7 <- "09:47.0" 
      for (i in 22:169) {
        Spawn_8D$Time[i] <- add_minutes(Spawn_8D$Time[i], 
                                        as.numeric(difftime(as.POSIXct(fixed_time7, format = "%M:%OS"), 
                                                            as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
      }
      
      # Update rows 170 to 263 (add 19:52.7 minutes)
      fixed_time8 <- "19:52.7" 
      for (i in 170:263) {
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
#Combine dyad and spawn dataframes
BachelorVol1 <- rbind(AllDyad,AllSpawn)
#Convert time from minutes into seconds 
{
# Function to convert mm:ss.0 format to seconds
convert_to_seconds <- function(time_str) {
  # Extract minutes and seconds from the mm:ss.0 format
  time_parts <- str_split(time_str, ":") %>% unlist()
  minutes <- as.numeric(time_parts[1])
  seconds <- as.numeric(time_parts[2])
  
  # Convert to total seconds
  total_seconds <- (minutes * 60) + seconds
  return(total_seconds)
}

# Convert the Time column in BachelorVol1 to seconds
BachelorVol1 <- BachelorVol1 %>%
  mutate(Time = sapply(Time, convert_to_seconds))
}
#Remove observations that were recorded before 15 minute mark
{
  # Filter out rows where Time is less than 900 seconds for each TankID
  BachelorVol1 <- BachelorVol1 %>%
    filter(Time >= 900)
  
  # View the filtered result
  print(BachelorVol1)
}
#Import the new dyad raw logs
{
  Dyad9D <- read_csv("logDyad9D.csv")
  Dyad9ND <- read_csv("logDyad9ND.csv")
  Dyad10D <- read_csv("logDyad10D.csv")
  Dyad10ND <- read_csv("logDyad10ND.csv")
  Dyad11D <- read_csv("logDyad11D.csv")
  Dyad11ND <- read_csv("logDyad11ND.csv")
  Dyad12D <- read_csv("logDyad12D.csv")
  Dyad12ND <- read_csv("logDyad12ND.csv")
  Dyad13D <- read_csv("logDyad13D.csv")
  Dyad13ND <- read_csv("logDyad13ND.csv")
  Dyad14D <- read_csv("logDyad14D.csv")
  Dyad14ND <- read_csv("logDyad14ND.csv")
  
  # Add Assay and update Subject columns
  Dyad9D <- Dyad9D %>% mutate(Assay = "Dyad", Subject = "Territorial", TankID = "D9_T")
  Dyad9ND <- Dyad9ND %>% mutate(Assay = "Dyad", Subject = "Non-territorial", TankID = "D9_NT")
  
  Dyad10D <- Dyad10D %>% mutate(Assay = "Dyad", Subject = "Territorial", TankID = "D10_T")
  Dyad10ND <- Dyad10ND %>% mutate(Assay = "Dyad", Subject = "Non-territorial", TankID = "D10_NT")
  
  Dyad11D <- Dyad11D %>% mutate(Assay = "Dyad", Subject = "Territorial", TankID = "D11_T")
  Dyad11ND <- Dyad11ND %>% mutate(Assay = "Dyad", Subject = "Non-territorial", TankID = "D11_NT")
  
  Dyad12D <- Dyad12D %>% mutate(Assay = "Dyad", Subject = "Territorial", TankID = "D12_T")
  Dyad12ND <- Dyad12ND %>% mutate(Assay = "Dyad", Subject = "Non-territorial", TankID = "D12_NT")
  
  Dyad13D <- Dyad13D %>% mutate(Assay = "Dyad", Subject = "Territorial", TankID = "D13_T")
  Dyad13ND <- Dyad13ND %>% mutate(Assay = "Dyad", Subject = "Non-territorial", TankID = "D13_NT")
  
  Dyad14D <- Dyad14D %>% mutate(Assay = "Dyad", Subject = "Territorial", TankID = "D14_T")
  Dyad14ND <- Dyad14ND %>% mutate(Assay = "Dyad", Subject = "Non-territorial", TankID = "D14_NT")
}
#Import the new spawn raw logs
{
  Spawn9D <- read_csv("logSpawn9D.csv")
  Spawn9ND <- read_csv("logSpawn9ND.csv")
  Spawn10D <- read_csv("logSpawn10D.csv")
  Spawn10ND <- read_csv("logSpawn10ND.csv")
  Spawn11D <- read_csv("logSpawn11D.csv")
  Spawn11ND <- read_csv("logSpawn11ND.csv")
  Spawn12D <- read_csv("logSpawn12D.csv")
  Spawn12ND <- read_csv("logSpawn12ND.csv")
  Spawn13D <- read_csv("logSpawn13D.csv")
  Spawn13ND <- read_csv("logSpawn13ND.csv")
  Spawn14D <- read_csv("logSpawn14D.csv")
  Spawn14ND <- read_csv("logSpawn14ND.csv")
  
  # Add Subject, Assay, and TankID columns
  Spawn9D <- Spawn9D %>% mutate(Subject = "Territorial", Assay = "Spawn", TankID = "S9_T")
  Spawn9ND <- Spawn9ND %>% mutate(Subject = "Non-Territorial", Assay = "Spawn", TankID = "S9_NT")
  
  Spawn10D <- Spawn10D %>% mutate(Subject = "Territorial", Assay = "Spawn", TankID = "S10_T")
  Spawn10ND <- Spawn10ND %>% mutate(Subject = "Non-Territorial", Assay = "Spawn", TankID = "S10_NT")
  
  Spawn11D <- Spawn11D %>% mutate(Subject = "Territorial", Assay = "Spawn", TankID = "S11_T")
  Spawn11ND <- Spawn11ND %>% mutate(Subject = "Non-Territorial", Assay = "Spawn", TankID = "S11_NT")
  
  Spawn12D <- Spawn12D %>% mutate(Subject = "Territorial", Assay = "Spawn", TankID = "S12_T")
  Spawn12ND <- Spawn12ND %>% mutate(Subject = "Non-Territorial", Assay = "Spawn", TankID = "S12_NT")
  
  Spawn13D <- Spawn13D %>% mutate(Subject = "Territorial", Assay = "Spawn", TankID = "S13_T")
  Spawn13ND <- Spawn13ND %>% mutate(Subject = "Non-Territorial", Assay = "Spawn", TankID = "S13_NT")
  
  Spawn14D <- Spawn14D %>% mutate(Subject = "Territorial", Assay = "Spawn", TankID = "S14_T")
  Spawn14ND <- Spawn14ND %>% mutate(Subject = "Non-Territorial", Assay = "Spawn", TankID = "S14_NT")
}
#combine new logs
{
  BachelorVol2 <- bind_rows(
    Dyad9D, Dyad9ND, Dyad10D, Dyad10ND, Dyad11D, Dyad11ND, Dyad12D, Dyad12ND,
    Dyad13D, Dyad13ND, Dyad14D, Dyad14ND,
    Spawn9D, Spawn9ND, Spawn10D, Spawn10ND, Spawn11D, Spawn11ND, Spawn12D, Spawn12ND,
    Spawn13D, Spawn13ND, Spawn14D, Spawn14ND
  )
}
#Remove observations that were recorded before 15 minute mark
{
  # Filter out rows where Time is less than 900 seconds for each TankID
  BachelorVol2_filtered <- BachelorVol2 %>%
    filter(Time >= 900)
  
  # View the filtered result
  print(BachelorVol2_filtered)
}
# Combine the two dataframes
FullBachelorData <- bind_rows(BachelorVol1, BachelorVol2_filtered)
}
####AN ANALYSIS OF RELATIVE FREQUENCY IN DYAD ASSAY####
{
  # Filter FullBachelorData to include only rows where Assay == "Dyad"
  AllDyad <- FullBachelorData %>%
    filter(Assay == "Dyad")
  #relative incidence 
  {
    # Create the Incidence_tank dataframe: how may times each behavior occurred in each tank
    Incidence_tank <- AllDyad %>%
      group_by(TankID, Behavior) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Calculate total behaviors for each TankID : how many behaviors occurred in total in each tank 
    total_behaviors_per_tank <- AllDyad %>%
      group_by(TankID, Subject) %>%
      summarise(TotalBehaviors = n(), .groups = "drop")
    
    # Join this total back to the Incidence_tank dataframe
    Incidence_tank <- Incidence_tank %>%
      left_join(total_behaviors_per_tank, by = "TankID")
    
    # Add the relative_incidence column to the Incidence_tank dataframe: #of each behavior in a tank/ total # of behaviors in that tank
    Incidence_tank <- Incidence_tank %>%
      mutate(relative_incidence = Count / TotalBehaviors)
    #write.csv(Incidence_tank, "dyad.relative.incidence.csv", row.names = TRUE)
    
    # Calculate mean and SEM for each behavior
    mean_sem_perc <- Incidence_tank %>%
      group_by(Behavior,Subject) %>%
      summarise(
        Mean = mean(relative_incidence, na.rm = TRUE),
        SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
        .groups = "drop"
      )
    #write.csv(mean_sem_perc, "dyad.mean.sem.csv", row.names = TRUE)
    }
  #paired t-test (Dyad T vs Dyad NT)
  {
    #add 0s for behaviors that are missing
    {
      # Create a dataframe with all combinations of TankIDs and behaviors
      complete_data <- expand.grid(TankID = unique(Incidence_tank$TankID),
                                   Behavior = unique(Incidence_tank$Behavior))
      
      # Perform a full join to ensure all combinations are present
      complete_incidence <- full_join(Incidence_tank, complete_data, by = c("TankID", "Behavior"))
      
      # Fill missing values with 0
      complete_incidence <- replace_na(complete_incidence, list(TotalBehaviors = 0, relative_incidence = 0))
      
      complete_incidence <- complete_incidence %>%
        mutate(Subject = case_when(
          grepl("_T$", TankID) ~ "Territorial",
          grepl("_NT$", TankID) ~ "Non-territorial",
          TRUE ~ "Unknown" # In case of unexpected TankID format
        ))
    }
    
    # Perform paired t-tests for each behavior
    perform_paired_t_test_details <- function(behavior, data) {
      subset_data <- data[data$Behavior == behavior, ]
      t_test_result <- t.test(
        subset_data$relative_incidence ~ subset_data$Subject, 
        paired = TRUE
      )
      
      # Return a dataframe with detailed results
      return(data.frame(
        Behavior = behavior,
        Mean_Difference = t_test_result$estimate,
        Test_Statistic = t_test_result$statistic,
        DF = t_test_result$parameter,
        P_Value = t_test_result$p.value,
        Lower_CI = t_test_result$conf.int[1],
        Upper_CI = t_test_result$conf.int[2]
      ))
    }
    
    # Perform t-tests for each behavior
    results <- lapply(unique(complete_incidence$Behavior), function(behavior) {
      perform_paired_t_test_details(behavior, complete_incidence)
    })
    
    # Combine the results into a single dataframe
    results_df <- do.call(rbind, results)
    
    # Extract p-values for correction
    all_p_values <- results_df$P_Value
    
    # Apply Benjamini-Hochberg correction to p-values
    results_df$Adjusted_P_Value <- p.adjust(all_p_values, method = "BH")
    
    # Print the complete results
    print(results_df)
    #write.csv(results_df, "paired_t_DyadT_vsDyadNT.csv", row.names = FALSE)
  }
  #Individual Behavior Plot
  {
    Plot.1 <- ggplot(complete_incidence, aes(x = Behavior, y = relative_incidence, fill = Subject)) +
      geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
      stat_summary(fun = mean, geom = "crossbar", 
                   position = position_dodge(width = 0.75), width = 0.5) +
      labs(title = "All T/NT Behaviors in Dyad Assay: Relative Incidence", 
           x = "Behavior", y = "Relative Incidence") +
      theme(
        axis.text.x = element_text(angle = 55, hjust = 1, family = "Times", size = 30),  # X-axis text size
        axis.text.y = element_text(family = "Times", size = 30),  # Y-axis text size
        axis.title.x = element_text(family = "Times", size = 30),  # X-axis title size
        axis.title.y = element_text(family = "Times", size = 30),  # Y-axis title size
        plot.title = element_text(family = "Times", size = 30, margin = margin(b = 30)),  # Plot title size with bottom margin
        legend.text = element_text(family = "Times", size = 30),  # Legend text size
        legend.title = element_text(family = "Times", size = 30),  # Legend title size
        text = element_text(family = "Times", size = 30),  # General text size
        plot.margin = unit(c(2, 1, 4, 1), "cm"),  # Increase margins
        panel.background = element_rect(fill = "white"),  # White background for the panel
        plot.background = element_rect(fill = "white")  # White background for the plot
      ) + 
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(values = c("Territorial" = "red", "Non-territorial" = "blue")) +
      geom_text(aes(label = "*"), x = 2, y = 0.8, vjust = -0.5, size = 10) +
      geom_text(aes(label = "*"), x = 3, y = 0.8, vjust = -0.5, size = 10) +
      geom_text(aes(label = "*"), x = 4, y = 0.8, vjust = -0.5, size = 10) +
      geom_text(aes(label = "*"), x = 6, y = 0.8, vjust = -0.5, size = 10) +
      geom_text(aes(label = "*"), x = 7, y = 0.8, vjust = -0.5, size = 10) +
      geom_text(aes(label = "*"), x = 10, y = 0.8, vjust = -0.5, size = 10) +
      geom_text(aes(label = "*"), x = 11, y = 0.8, vjust = -0.5, size = 10) +
      geom_text(aes(label = "*"), x = 12, y = 0.8, vjust = -0.5, size = 10)
    
    print(Plot.1)
    #ggsave("30Dyad.white.relative.Incidence.fig2.png", plot = Plot.1, width = 14, height = 10, units = "in")
  }
  #Categorical Analysis
  {
    #Assign categories to behaviors
    AllData <- complete_incidence %>%
      mutate(Category = case_when(
        grepl("pot exit", Behavior) ~ "Reproductive", 
        grepl("pot entry", Behavior) ~ "Reproductive",
        grepl("attack female", Behavior) ~ "Reproductive",
        grepl("lead swim", Behavior) ~ "Reproductive",
        grepl("dig", Behavior) ~ "Reproductive",
        grepl("quiver at female", Behavior) ~ "Reproductive",
        grepl("chase female", Behavior) ~ "Agressive",
        grepl("lateral display", Behavior) ~ "Agressive",
        grepl("chase male", Behavior) ~ "Agressive",
        grepl("attack male", Behavior) ~ "Agressive",
        grepl("quiver at male", Behavior) ~ "Agressive",
        grepl("flee from female", Behavior) ~ "Aversive",
        grepl("flee from male", Behavior) ~ "Aversive"
      ))
    #Get mean+SEM dataframe
    {
      # Calculate Mean and SEM for each combination of Category and Subject
      summary_stats <- AllData %>%
        group_by(Category, Subject) %>%
        summarise(
          Mean = mean(relative_incidence, na.rm = TRUE), 
          SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()),  # Standard Error of the Mean
          .groups = 'drop'  # Drops the grouping structure after summarizing
        )
      #write.csv(summary_stats, "Dyad_meanSEM_by_category_and_subject.csv", row.names = TRUE)
      }
    # Separate data by categories
    aggressive <- subset(AllData, Category == "Agressive")
    aversive <- subset(AllData, Category == "Aversive")
    reproductive <- subset(AllData, Category == "Reproductive")
    
    # Define the significance level
    significance_level <- 0.05
    
    # Function to perform paired t-tests and extract all relevant details
    perform_paired_t_test <- function(data) {
      t_test_results <- t.test(data$relative_incidence ~ data$Subject, paired = TRUE)
      
      return(data.frame(
        Mean_Difference = t_test_results$estimate,
        Test_Statistic = t_test_results$statistic,
        DF = t_test_results$parameter,
        P_Value = t_test_results$p.value,
        Lower_CI = t_test_results$conf.int[1],
        Upper_CI = t_test_results$conf.int[2]
      ))
    }
    
    # Perform t-tests for each category
    aggressive_results <- perform_paired_t_test(aggressive)
    aversive_results <- perform_paired_t_test(aversive)
    reproductive_results <- perform_paired_t_test(reproductive)
    
    # Combine results into a single dataframe
    all_results <- bind_rows(
      cbind(Category = "Aggressive", aggressive_results),
      cbind(Category = "Aversive", aversive_results),
      cbind(Category = "Reproductive", reproductive_results)
    )
    
    # Apply Benjamini-Hochberg correction to p-values
    all_results$Adjusted_P_Value <- p.adjust(all_results$P_Value, method = "BH")
    
    # View the complete results
    print(all_results)

    #write.csv(all_results, "dyad_t_test_results_with_correction.csv", row.names = FALSE)
  }
  #Categorical Behavior Plot
  {
    categorical.1 <- ggplot(AllData, aes(x = Category, y = relative_incidence, fill = Subject)) +
      geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
      stat_summary(fun = mean, geom = "crossbar", 
                   position = position_dodge(width = 0.75), width = 0.5) +
      labs(title = "T/NT Behavioral Categories in Dyad Assay: Relative Incidence", 
           x = "Behavioral Category", y = "Relative Incidence") +
      theme(
        axis.text.x = element_text(angle = 55, hjust = 1, family = "Times", size = 30),  # X-axis text size
        axis.text.y = element_text(family = "Times", size = 30),  # Y-axis text size
        axis.title.x = element_text(family = "Times", size = 30),  # X-axis title size
        axis.title.y = element_text(family = "Times", size = 30),  # Y-axis title size
        plot.title = element_text(family = "Times", size = 30, margin = margin(b = 30)),  # Plot title size with more bottom margin
        legend.text = element_text(family = "Times", size = 30),  # Legend text size
        legend.title = element_text(family = "Times", size = 30),  # Legend title size
        text = element_text(family = "Times", size = 30),  # General text size
        plot.margin = unit(c(2, 1, 4, 1), "cm"),  # Increase bottom margin even further
        panel.background = element_rect(fill = "white"),  # White background for the panel
        plot.background = element_rect(fill = "white")  # White background for the plot
      ) + 
      scale_y_continuous(limits = c(0, 1)) +
      annotate("text", x = 1, y = 0.9, label = "*", size = 10, color = "black") +
      annotate("text", x = 2, y = 0.9, label = "*", size = 10, color = "black") +
      annotate("text", x = 3, y = 0.9, label = "*", size = 10, color = "black") +
      scale_fill_manual(values = c("Territorial" = "red", "Non-territorial" = "blue"))
    
    
    print(categorical.1)
    #ggsave("30Dyad.white.relative.Incidence.cat.png", plot = categorical.1, width = 14, height = 10, units = "in")
    
  }
}
####AN ANALYSIS OF RELATIVE FREQUENCY IN SPAWN ASSAY####
{
  # Filter FullBachelorData to include only rows where Assay == "Spawn"
  AllSpawn <- FullBachelorData %>%
    filter(Assay == "Spawn")
  #relative incidence 
  {
    # Create the Incidence_tank dataframe: how may times each behavior accurred in each tank
    Incidence_tank <- AllSpawn %>%
      group_by(TankID, Behavior) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Calculate total behaviors for each TankID : how many behaviors occurred in total in each tank 
    total_behaviors_per_tank <- AllSpawn %>%
      group_by(TankID, Subject) %>%
      summarise(TotalBehaviors = n(), .groups = "drop")
    
    # Join this total back to the Incidence_tank dataframe
    Incidence_tank <- Incidence_tank %>%
      left_join(total_behaviors_per_tank, by = "TankID")
    
    # Add the relative_incidence column to the Incidence_tank dataframe: #of each behavior in a tank/ total # of behaviors in that tank
    Incidence_tank <- Incidence_tank %>%
      mutate(relative_incidence = Count / TotalBehaviors)
    #write.csv(Incidence_tank, "Spawn.relative.incidence.csv", row.names = TRUE)
    
    # Calculate mean and SEM for each behavior
    mean_sem_perc <- Incidence_tank %>%
      group_by(Behavior,Subject) %>%
      summarise(
        Mean = mean(relative_incidence, na.rm = TRUE),
        SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
        .groups = "drop"
      )
    
    #write.csv(mean_sem_perc, "Spawn.mean.sem.csv", row.names = TRUE)
    }
  #paired t-test (Spawn T vs Spawn NT)
  {
    #add 0s for behaviros that are missing
    {
      # Create a dataframe with all combinations of TankIDs and behaviors
      complete_data <- expand.grid(TankID = unique(Incidence_tank$TankID),
                                   Behavior = unique(Incidence_tank$Behavior))
      
      # Perform a full join to ensure all combinations are present
      complete_incidence <- full_join(Incidence_tank, complete_data, by = c("TankID", "Behavior"))
      
      # Fill missing values with 0
      complete_incidence <- replace_na(complete_incidence, list(TotalBehaviors = 0, relative_incidence = 0))
      
      complete_incidence <- complete_incidence %>%
        mutate(Subject = case_when(
          grepl("_T$", TankID) ~ "Territorial",
          grepl("_NT$", TankID) ~ "Non-territorial",
          TRUE ~ "Unknown" # In case of unexpected TankID format
        ))
    }
    
    # Perform paired t-tests for each behavior
    perform_paired_t_test_details <- function(behavior, data) {
      subset_data <- data[data$Behavior == behavior, ]
      t_test_result <- t.test(
        subset_data$relative_incidence ~ subset_data$Subject, 
        paired = TRUE
      )
      
      # Return a dataframe with detailed results
      return(data.frame(
        Behavior = behavior,
        Mean_Difference = t_test_result$estimate,
        Test_Statistic = t_test_result$statistic,
        DF = t_test_result$parameter,
        P_Value = t_test_result$p.value,
        Lower_CI = t_test_result$conf.int[1],
        Upper_CI = t_test_result$conf.int[2]
      ))
    }
    
    # Perform t-tests for each behavior
    results <- lapply(unique(complete_incidence$Behavior), function(behavior) {
      perform_paired_t_test_details(behavior, complete_incidence)
    })
    
    # Combine the results into a single dataframe
    results_df <- do.call(rbind, results)
    
    # Extract p-values for correction
    all_p_values <- results_df$P_Value
    
    # Apply Benjamini-Hochberg correction to p-values
    results_df$Adjusted_P_Value <- p.adjust(all_p_values, method = "BH")
    
    # Print the complete results
    print(results_df)
    #write.csv(results_df, "paired_t_SpawnT_vsSpawnNT.csv", row.names = FALSE)
  }
  #Individual Behavior Plot
  {
    Plot.1 <- ggplot(complete_incidence, aes(x = Behavior, y = relative_incidence, fill = Subject)) +
      geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
      stat_summary(fun = mean, geom = "crossbar", 
                   position = position_dodge(width = 0.75), width = 0.5) +
      labs(title = "All T/NT Behaviors in Spawn Assay: Relative Incidence", 
           x = "Behavior", y = "Relative Incidence") +
      theme(
        axis.text.x = element_text(angle = 55, hjust = 1, family = "Times", size = 30),  # X-axis text size
        axis.text.y = element_text(family = "Times", size = 30),  # Y-axis text size
        axis.title.x = element_text(family = "Times", size = 30),  # X-axis title size
        axis.title.y = element_text(family = "Times", size = 30),  # Y-axis title size
        plot.title = element_text(family = "Times", size = 30),  # Plot title size
        legend.text = element_text(family = "Times", size = 30),  # Legend text size
        legend.title = element_text(family = "Times", size = 30),  # Legend title size
        text = element_text(family = "Times", size = 30),  # General text size
        plot.margin = unit(c(1, 1, 2, 1), "cm"),
        panel.background = element_rect(fill = "white"),  # White background for the panel
        plot.background = element_rect(fill = "white")  # White background for the plot
      ) + 
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(values = c("Territorial" = "red", "Non-territorial" = "blue"))
    
    print(Plot.1)
    #ggsave("30Spawn.white.relative.Incidence.fig2.png", plot = Plot.1, width = 10, height = 8, units = "in")
  }
  #Categorical Analysis
  {
    #Assign categories to behaviors
    AllData <- complete_incidence %>%
      mutate(Category = case_when(
        grepl("pot exit", Behavior) ~ "Reproductive", 
        grepl("pot entry", Behavior) ~ "Reproductive",
        grepl("attack female", Behavior) ~ "Reproductive",
        grepl("lead swim", Behavior) ~ "Reproductive",
        grepl("dig", Behavior) ~ "Reproductive",
        grepl("quiver at female", Behavior) ~ "Reproductive",
        grepl("chase female", Behavior) ~ "Aggressive",
        grepl("lateral display", Behavior) ~ "Aggressive",
        grepl("chase male", Behavior) ~ "Aggressive",
        grepl("attack male", Behavior) ~ "Aggressive",
        grepl("quiver at male", Behavior) ~ "Aggressive",
        grepl("flee from female", Behavior) ~ "Aversive",
        grepl("flee from male", Behavior) ~ "Aversive"
      ))
    
    #Get mean+SEM dataframe
    {
      # Calculate Mean and SEM for each combination of Category and Subject
      summary_stats <- AllData %>%
        group_by(Category, Subject) %>%
        summarise(
          Mean = mean(relative_incidence, na.rm = TRUE), 
          SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()),  # Standard Error of the Mean
          .groups = 'drop'  # Drops the grouping structure after summarizing
        )
      #write.csv(summary_stats, "Spawn_meanSEM_by_category_and_subject.csv", row.names = TRUE)
      }
    # Separate data by categories
    aggressive <- subset(AllData, Category == "Aggressive")
    aversive <- subset(AllData, Category == "Aversive")
    reproductive <- subset(AllData, Category == "Reproductive")
    
    # Define the significance level
    significance_level <- 0.05
    
    # Function to perform paired t-tests and extract all relevant details
    perform_paired_t_test <- function(data) {
      t_test_results <- t.test(data$relative_incidence ~ data$Subject, paired = TRUE)
      
      return(data.frame(
        Mean_Difference = t_test_results$estimate,
        Test_Statistic = t_test_results$statistic,
        DF = t_test_results$parameter,
        P_Value = t_test_results$p.value,
        Lower_CI = t_test_results$conf.int[1],
        Upper_CI = t_test_results$conf.int[2]
      ))
    }
    
    # Perform t-tests for each category
    aggressive_results <- perform_paired_t_test(aggressive)
    aversive_results <- perform_paired_t_test(aversive)
    reproductive_results <- perform_paired_t_test(reproductive)
    
    # Combine results into a single dataframe
    all_results <- bind_rows(
      cbind(Category = "Aggressive", aggressive_results),
      cbind(Category = "Aversive", aversive_results),
      cbind(Category = "Reproductive", reproductive_results)
    )
    
    # Apply Benjamini-Hochberg correction to p-values
    all_results$Adjusted_P_Value <- p.adjust(all_results$P_Value, method = "BH")
    
    # View the complete results
    print(all_results)
    # Save the dataframe to a CSV file
    #write.csv(all_results, "Spawn_t_test_results_with_correctionCAT.csv", row.names = FALSE)
  }
  #Categorical Behavior Plot
  {
    categorical.1 <- ggplot(AllData, aes(x = Category, y = relative_incidence, fill = Subject)) +
      geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
      stat_summary(fun = mean, geom = "crossbar", 
                   position = position_dodge(width = 0.75), width = 0.5) +
      labs(title = "T/NT Behavioral Categories in Spawn Assay: Relative Incidence", 
           x = "Behavioral Category", y = "Relative Incidence") +
      theme(
        axis.text.x = element_text(angle = 55, hjust = 1, family = "Times", size = 30),  # X-axis text size
        axis.text.y = element_text(family = "Times", size = 30),  # Y-axis text size
        axis.title.x = element_text(family = "Times", size = 30),  # X-axis title size
        axis.title.y = element_text(family = "Times", size = 30),  # Y-axis title size
        plot.title = element_text(family = "Times", size = 30, margin = margin(b = 30)),  # Plot title size with more bottom margin
        legend.text = element_text(family = "Times", size = 30),  # Legend text size
        legend.title = element_text(family = "Times", size = 30),  # Legend title size
        text = element_text(family = "Times", size = 30),  # General text size
        plot.margin = unit(c(2, 1, 4, 1), "cm"),  # Increase bottom margin even further
        panel.background = element_rect(fill = "white"),  # White background for the panel
        plot.background = element_rect(fill = "white")  # White background for the plot
      ) + 
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(values = c("Territorial" = "red", "Non-territorial" = "blue"))
    
    
    print(categorical.1)
    #ggsave("30Spawn.white.relative.Incidence.cat.png", plot = categorical.1, width = 14, height = 10, units = "in")
    
  }
}
####ANALYSIS OF TRANSITION TIME BETWEEN BEHAVIORS IN THE DYAD ASSAY#### 
{
  #Territorial
  {
    # Create Dyad_T dataframe with rows where Subject is "Territorial"
    Dyad_T <- AllDyad %>%
      filter(Subject == "Territorial")
    
    # Create separate dataframes for each TankID
    unique_tank_ids <- unique(Dyad_T$TankID)
    for (tank_id in unique_tank_ids) {
      df_name <- paste0("Tank_", tank_id)
      assign(df_name, filter(Dyad_T, TankID == tank_id))
    }
    
    for (tank_id in unique_tank_ids) {
      df_name <- paste0("Tank_", tank_id)
      assign(df_name, Dyad_T %>% filter(TankID == tank_id))
    }
    
    #D1_T
    #Calculate behavioral transitions and transition times
    D1_T_trans <- Tank_D1_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D3_T
    #Calculate behavioral transitions and transition times
    D3_T_trans <- Tank_D3_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D5_T
    #Calculate behavioral transitions and transition times
    D5_T_trans <- Tank_D5_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D8_T
    #Calculate behavioral transitions and transition times
    D8_T_trans <- Tank_D8_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D9_T
    #Calculate behavioral transitions and transition times
    D9_T_trans <- Tank_D9_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D10_T
    #Calculate behavioral transitions and transition times
    D10_T_trans <- Tank_D10_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D11_T
    #Calculate behavioral transitions and transition times
    D11_T_trans <- Tank_D11_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D12_T
    #Calculate behavioral transitions and transition times
    D12_T_trans <- Tank_D12_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D13_T
    #Calculate behavioral transitions and transition times
    D13_T_trans <- Tank_D13_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D14_T
    #Calculate behavioral transitions and transition times
    D14_T_trans <- Tank_D14_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    
    #combine
    T_trans <- rbind(D1_T_trans,D3_T_trans,D5_T_trans,D8_T_trans,D9_T_trans,D10_T_trans,
                     D11_T_trans,D12_T_trans,D13_T_trans,D14_T_trans)
  }
  #Non-territorial 
  {
    # Create Dyad_NT dataframe with rows where Subject is "Non-territorial"
    Dyad_NT <- AllDyad %>%
      filter(Subject == "Non-territorial")
    
    unique_tank_ids <- unique(Dyad_NT$TankID)
    for (tank_id in unique_tank_ids) {
      df_name <- paste0("Tank_", tank_id)
      assign(df_name, filter(Dyad_NT, TankID == tank_id))
    }
    #D1_NT
    #Calculate behavioral transitions and transition times
    D1_NT_trans <- Tank_D1_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D3_NT
    #Calculate behavioral transitions and transition times
    D3_NT_trans <- Tank_D3_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D5_NT
    #Calculate behavioral transitions and transition times
    D5_NT_trans <- Tank_D5_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D8_NT
    #Calculate behavioral transitions and transition times
    D8_NT_trans <- Tank_D8_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D9_NT
    #Calculate behavioral transitions and transition times
    D9_NT_trans <- Tank_D9_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D10_NT
    #Calculate behavioral transitions and transition times
    D10_NT_trans <- Tank_D10_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D11_NT
    #Calculate behavioral transitions and transition times
    D11_NT_trans <- Tank_D11_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D12_NT
    #Calculate behavioral transitions and transition times
    D12_NT_trans <- Tank_D12_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D13_NT
    #Calculate behavioral transitions and transition times
    D13_NT_trans <- Tank_D13_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #D14_NT
    #Calculate behavioral transitions and transition times
    D14_NT_trans <- Tank_D14_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    
    NT_trans <- rbind(D1_NT_trans,D3_NT_trans,D5_NT_trans,D8_NT_trans,D9_NT_trans,
                      D10_NT_trans,D11_NT_trans,D12_NT_trans,D13_NT_trans,D14_NT_trans)
  }
  ## combine 
  All_trans <- rbind(T_trans,NT_trans)
  
  ## mean and SEM ##
  {
    #Remove rows with NA values in the Transition_time column
    All_trans <- All_trans %>% filter(!is.na(Transition_time))
    # Step 2: Group the dataframe by the Transition column
    All_trans_grouped <- All_trans %>%
      group_by(Transition,Subject) %>%
      summarise(
        Mean_Transition_time = mean(Transition_time),
        SEM_Transition_time = sd(Transition_time) / sqrt(n())  # Calculate SEM
      )
    #write.csv(All_trans_grouped, "Dyad_decision_making_latency.mean.sem.csv", row.names = TRUE)
  }
  ## t-test ##
  {
    #make a separate dataframe with relevant columns
    LatSpawn <- All_trans %>%
      select(TankID, Subject, Transition, Transition_time)
    #get a list of unique transitions
    valid_transitions <- unique(LatSpawn$Transition)
    print(unique_transitions)
    # Group by Transition and check if both types exist
    valid_transitions <- LatSpawn %>%
      group_by(Transition) %>%
      summarize(Has_Territorial = any(Subject == "Territorial"),
                Has_Non_Territorial = any(Subject == "Non-territorial")) %>%
      filter(Has_Territorial & Has_Non_Territorial) %>%
      pull(Transition)
    
    # Print the valid transitions
    print(valid_transitions)
    
    # Filter LatSpawn for valid transitions
    LatSpawn_valid <- LatSpawn %>%
      filter(Transition %in% valid_transitions)
    
    # Initialize lists to store results
    results_list <- list()
    
    # Perform paired t-tests for each transition
    for (trans in valid_transitions) {
      # Filter data for the current transition
      data_sub <- LatSpawn_valid %>%
        filter(Transition == trans) %>%
        pivot_wider(names_from = Subject, values_from = Transition_time) %>%
        drop_na()
      
      # Print the filtered data for debugging
      print(paste("Transition:", trans))
      print(head(data_sub))
      
      # Check if there are enough data points for both subjects
      if (nrow(data_sub) > 0) {
        if ("Territorial" %in% colnames(data_sub) & "Non-territorial" %in% colnames(data_sub)) {
          # Perform paired t-test
          t_test <- t.test(data_sub$Territorial, data_sub$Non-territorial, paired = TRUE)
          
          # Extract results
          results_list[[trans]] <- tidy(t_test)
        } else {
          print(paste("Missing subject types for transition:", trans))
        }
      } else {
        print(paste("Not enough data for transition:", trans))
      }
    }
    
    # Combine results into a single dataframe
    results_df <- bind_rows(results_list, .id = "Transition")
    
    # Print the structure of results_df to check column names
    print(str(results_df))
    
    # Ensure the p.value column exists and then adjust p-values
    if ("p.value" %in% colnames(results_df)) {
      results_df <- results_df %>%
        mutate(p.adjusted = p.adjust(p.value, method = "BH"))
      
      # Save the results to a CSV file
      write.csv(results_df, "t_test_results.csv", row.names = FALSE)
      
      # Print results dataframe
      print(results_df)
    } else {
      stop("p.value column not found in the results.")
    }
  }
  ## heatmap ##
  {
    # Separate the Transition column into From_Behavior and To_Behavior
    All_trans_grouped <- All_trans_grouped %>%
      separate(Transition, into = c("From_Behavior", "To_Behavior"), sep = "\\|")
    
    # Create the heatmap with facets for each subject, tilted x-axis labels, customized gradient, and title
    p <- ggplot(All_trans_grouped, aes(x = From_Behavior, y = To_Behavior, fill = Mean_Transition_time)) +
      geom_tile() +
      scale_fill_gradient(low = "#E6F5FF", high = "#004D99") + # Customizing gradient colors
      labs(x = "From Behavior", y = "To Behavior", fill = "Time (seconds)") +
      ggtitle("Decision Making Latency in Dyad Assay") +  # Adding title
      theme_minimal() +
      facet_wrap(~ Subject) +
      theme(
        axis.text = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 16, family = "Times"),
        plot.title = element_text(size = 16, family = "Times"),
        legend.text = element_text(size = 16, family = "Times"),
        legend.title = element_text(size = 16, family = "Times"),
        strip.text = element_text(size = 16, family = "Times"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    
    # Save the plot as a PNG file
    #ggsave("Dyad.decision_making_heatmap.png", p, width = 10, height = 6, bg = "white")
  }
  ## point plot ##
  {
    # Reorder the levels of the interaction variable alphabetically
    All_trans_grouped$interaction_order <- with(All_trans_grouped, 
                                                reorder(interaction(From_Behavior, To_Behavior), 
                                                        as.character(interaction(From_Behavior, To_Behavior))))
    
    point_plot <- ggplot(All_trans_grouped, aes(x =Mean_Transition_time ,y = interaction_order, color = Subject)) +
      geom_point(position = position_dodge(width = 0.5)) +  # Add points with dodge position
      geom_errorbar(aes(xmin = Mean_Transition_time - SEM_Transition_time, xmax = Mean_Transition_time + SEM_Transition_time), 
                    width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
      labs(x = "Average Time (seconds)", y = "Behavioral Transitions", color = "Subject") +
      ggtitle("Average Decision Making Latency with SEM in Dyad") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10, hjust = 0,family = "Times")) +
      scale_color_manual(values = c("Territorial" = "red", "Non-territorial" = "blue"))
    
    
    # Print the point plot
    print(point_plot)
    #ggsave("Dyad_decision_making_point_plot.png", plot = point_plot, width = 8, height = 7, units = "in", dpi = 300)
  }
}
####ANALYSIS OF TRANSITION TIME BETWEEN BEHAVIORS IN THE SPAWN ASSAY#### 
{
  #Territorial
  {
    # Create Spawn_T dataframe with rows where Subject is "Territorial"
    Spawn_T <- AllSpawn %>%
      filter(Subject == "Territorial")
    
    # Create separate dataframes for each TankID
    unique_tank_ids <- unique(Spawn_T$TankID)
    for (tank_id in unique_tank_ids) {
      df_name <- paste0("Tank_", tank_id)
      assign(df_name, filter(Spawn_T, TankID == tank_id))
    }
    
    for (tank_id in unique_tank_ids) {
      df_name <- paste0("Tank_", tank_id)
      assign(df_name, Spawn_T %>% filter(TankID == tank_id))
    }
    
    #S1_T
    #Calculate behavioral transitions and transition times
    S1_T_trans <- Tank_S1_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S3_T
    #Calculate behavioral transitions and transition times
    S3_T_trans <- Tank_S3_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S5_T
    #Calculate behavioral transitions and transition times
    S5_T_trans <- Tank_S5_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S8_T
    #Calculate behavioral transitions and transition times
    S8_T_trans <- Tank_S8_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S9_T
    #Calculate behavioral transitions and transition times
    S9_T_trans <- Tank_S9_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S10_T
    #Calculate behavioral transitions and transition times
    S10_T_trans <- Tank_S10_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S11_T
    #Calculate behavioral transitions and transition times
    S11_T_trans <- Tank_S11_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S12_T
    #Calculate behavioral transitions and transition times
    S12_T_trans <- Tank_S12_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S13_T
    #Calculate behavioral transitions and transition times
    S13_T_trans <- Tank_S13_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S14_T
    #Calculate behavioral transitions and transition times
    S14_T_trans <- Tank_S14_T %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    
    #combine
    T_trans <- rbind(S1_T_trans,S3_T_trans,S5_T_trans,S8_T_trans,S9_T_trans,S10_T_trans,
                     S11_T_trans,S12_T_trans,S13_T_trans,S14_T_trans)
  }
  #Non-territorial 
  {
    # Create Spawn_NT dataframe with rows where Subject is "Non-territorial"
    Spawn_NT <- AllSpawn %>%
      filter(Subject == "Non-territorial")
    
    unique_tank_ids <- unique(Spawn_NT$TankID)
    for (tank_id in unique_tank_ids) {
      df_name <- paste0("Tank_", tank_id)
      assign(df_name, filter(Spawn_NT, TankID == tank_id))
    }
    
    #S1_NT
    #Calculate behavioral transitions and transition times
    S1_NT_trans <- Tank_S1_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S3_NT
    #Calculate behavioral transitions and transition times
    S3_NT_trans <- Tank_S3_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S5_NT
    #Calculate behavioral transitions and transition times
    S5_NT_trans <- Tank_S5_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S8_NT
    #Calculate behavioral transitions and transition times
    S8_NT_trans <- Tank_S8_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S9_NT
    #Calculate behavioral transitions and transition times
    S9_NT_trans <- Tank_S9_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S10_NT
    #Calculate behavioral transitions and transition times
    S10_NT_trans <- Tank_S10_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S11_NT
    #Calculate behavioral transitions and transition times
    S11_NT_trans <- Tank_S11_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S12_NT
    #Calculate behavioral transitions and transition times
    S12_NT_trans <- Tank_S12_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S13_NT
    #Calculate behavioral transitions and transition times
    S13_NT_trans <- Tank_S13_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    #S14_NT
    #Calculate behavioral transitions and transition times
    S14_NT_trans <- Tank_S14_NT %>%
      arrange(Time) %>%
      mutate(Transition = paste0(lag(Behavior), "|", Behavior),
             Transition_time = Time - lag(Time)) %>%
      filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
    
    NT_trans <- rbind(S1_NT_trans,S3_NT_trans,S5_NT_trans,S8_NT_trans,S9_NT_trans,
                      S10_NT_trans,S11_NT_trans,S12_NT_trans,S13_NT_trans,S14_NT_trans)
  }
  ## combine 
  All_trans <- rbind(T_trans,NT_trans)
  ## Mean and SEM ##
  {
    #Remove rows with NA values in the Transition_time column
    All_trans <- All_trans %>% filter(!is.na(Transition_time))
    #Group the dataframe by the Transition column
    All_trans_grouped <- All_trans %>%
      group_by(Transition,Subject) %>%
      summarise(
        Mean_Transition_time = mean(Transition_time),
        SEM_Transition_time = sd(Transition_time) / sqrt(n())  # Calculate SEM
      )
    #write.csv(All_trans_grouped, "Spawn_decision_making_latency.mean.sem.csv", row.names = TRUE)
  }
  ## t-test ##
  {
    #Verify data before performing t-tests
    print(All_trans)
    
    #Identify valid transitions and check data
    valid_transitions <- unique(All_trans$Transition)
    print(valid_transitions)
    
    
    #Perform paired t-tests on valid transitions
    paired_t_tests <- valid_transitions %>%
      map_df(~ {
        data <- filter(All_trans_filtered, Transition == .x)
        print(data)  # Debug: Print data for each transition
        if (nrow(data) < 2) {
          return(data.frame(Transition = .x, Mean_Difference = NA, Test_Statistic = NA, DF = NA, P_Value = NA, Lower_CI = NA, Upper_CI = NA))
        }
        # Perform t-test and calculate results
        t_test_result <- tryCatch(t.test(Transition_time ~ Subject, data = data),
                                  error = function(e) NULL)
        if (is.null(t_test_result)) {
          return(data.frame(Transition = .x, Mean_Difference = NA, Test_Statistic = NA, DF = NA, P_Value = NA, Lower_CI = NA, Upper_CI = NA))
        }
        data.frame(
          Transition = .x,
          Mean_Difference = t_test_result$estimate,
          Test_Statistic = t_test_result$statistic,
          DF = t_test_result$parameter,
          P_Value = t_test_result$p.value,
          Lower_CI = t_test_result$conf.int[1],
          Upper_CI = t_test_result$conf.int[2]
        )
      })
    
    #Apply Benjamini-Hochberg correction
    paired_t_tests <- paired_t_tests %>%
      filter(!is.na(P_Value)) %>%
      mutate(Corrected_P_Value = p.adjust(P_Value, method = "BH"))
    
    #Check results
    print(paired_t_tests)
    
    #Save results to CSV
    #write.csv(paired_t_tests, file = "spawn_paired_t_tests_results.csv", row.names = FALSE)
  }
  ## point plot ##
  {
    
    point_plot <- ggplot(All_trans_grouped, aes(x =Mean_Transition_time ,y = Transition, color = Subject)) +
      geom_point(position = position_dodge(width = 0.5)) +  # Add points with dodge position
      geom_errorbar(aes(xmin = Mean_Transition_time - SEM_Transition_time, xmax = Mean_Transition_time + SEM_Transition_time), 
                    width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
      labs(x = "Average Time (seconds)", y = "Behavioral Transitions", color = "Subject") +
      ggtitle("Average Decision Making Latency with SEM in Spawn") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8, hjust = 0,family = "Times")) +
      scale_color_manual(values = c("Territorial" = "red", "Non-territorial" = "blue"))
    
    
    # Print the point plot
    print(point_plot)
    #ggsave("Spawn_decision_making_point_plot.png", plot = point_plot, width = 8, height = 7, units = "in", dpi = 300)
  }
  
  ## heatmap ##
  {
    # Separate the Transition column into From_Behavior and To_Behavior
    All_trans_grouped <- All_trans_grouped %>%
      separate(Transition, into = c("From_Behavior", "To_Behavior"), sep = "\\|")
    
    # Create the heatmap with facets for each subject, tilted x-axis labels, customized gradient, and title
    p <- ggplot(All_trans_grouped, aes(x = From_Behavior, y = To_Behavior, fill = Mean_Transition_time)) +
      geom_tile() +
      scale_fill_gradient(low = "#E6F5FF", high = "#004D99") + # Customizing gradient colors
      labs(x = "From Behavior", y = "To Behavior", fill = "Time (seconds)") +
      ggtitle("Decision Making Latency in Spawn Assay") +  # Adding title
      theme_minimal() +
      facet_wrap(~ Subject) +
      theme(
        axis.text = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 16, family = "Times"),
        plot.title = element_text(size = 16, family = "Times"),
        legend.text = element_text(size = 16, family = "Times"),
        legend.title = element_text(size = 16, family = "Times"),
        strip.text = element_text(size = 16, family = "Times"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    
    # Save the plot as a PNG file
    #ggsave("Spawn.decision_making_heatmap.png", p, width = 10, height = 6, bg = "white")
  }
}
####ANALYSIS OF T AND NT SUBJECTS’ TRANSITION TIME ACROSS ASSAYS####
{
  #Dyad#
  {
    #Territorial
    {
      # Create Dyad_T dataframe with rows where Subject is "Territorial"
      Dyad_T <- AllDyad %>%
        filter(Subject == "Territorial")
      
      # Create separate dataframes for each TankID
      unique_tank_ids <- unique(Dyad_T$TankID)
      for (tank_id in unique_tank_ids) {
        df_name <- paste0("Tank_", tank_id)
        assign(df_name, filter(Dyad_T, TankID == tank_id))
      }
      
      for (tank_id in unique_tank_ids) {
        df_name <- paste0("Tank_", tank_id)
        assign(df_name, Dyad_T %>% filter(TankID == tank_id))
      }
      
      #D1_T
      #Calculate behavioral transitions and transition times
      D1_T_trans <- Tank_D1_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D3_T
      #Calculate behavioral transitions and transition times
      D3_T_trans <- Tank_D3_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D5_T
      #Calculate behavioral transitions and transition times
      D5_T_trans <- Tank_D5_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D8_T
      #Calculate behavioral transitions and transition times
      D8_T_trans <- Tank_D8_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D9_T
      #Calculate behavioral transitions and transition times
      D9_T_trans <- Tank_D9_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D10_T
      #Calculate behavioral transitions and transition times
      D10_T_trans <- Tank_D10_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D11_T
      #Calculate behavioral transitions and transition times
      D11_T_trans <- Tank_D11_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D12_T
      #Calculate behavioral transitions and transition times
      D12_T_trans <- Tank_D12_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D13_T
      #Calculate behavioral transitions and transition times
      D13_T_trans <- Tank_D13_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D14_T
      #Calculate behavioral transitions and transition times
      D14_T_trans <- Tank_D14_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      
      #combine
      T_trans <- rbind(D1_T_trans,D3_T_trans,D5_T_trans,D8_T_trans,D9_T_trans,D10_T_trans,
                       D11_T_trans,D12_T_trans,D13_T_trans,D14_T_trans)
    }
    #Non-territorial 
    {
      # Create Dyad_NT dataframe with rows where Subject is "Non-territorial"
      Dyad_NT <- AllDyad %>%
        filter(Subject == "Non-territorial")
      
      unique_tank_ids <- unique(Dyad_NT$TankID)
      for (tank_id in unique_tank_ids) {
        df_name <- paste0("Tank_", tank_id)
        assign(df_name, filter(Dyad_NT, TankID == tank_id))
      }
      #D1_NT
      #Calculate behavioral transitions and transition times
      D1_NT_trans <- Tank_D1_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D3_NT
      #Calculate behavioral transitions and transition times
      D3_NT_trans <- Tank_D3_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D5_NT
      #Calculate behavioral transitions and transition times
      D5_NT_trans <- Tank_D5_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D8_NT
      #Calculate behavioral transitions and transition times
      D8_NT_trans <- Tank_D8_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D9_NT
      #Calculate behavioral transitions and transition times
      D9_NT_trans <- Tank_D9_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D10_NT
      #Calculate behavioral transitions and transition times
      D10_NT_trans <- Tank_D10_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D11_NT
      #Calculate behavioral transitions and transition times
      D11_NT_trans <- Tank_D11_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D12_NT
      #Calculate behavioral transitions and transition times
      D12_NT_trans <- Tank_D12_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D13_NT
      #Calculate behavioral transitions and transition times
      D13_NT_trans <- Tank_D13_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #D14_NT
      #Calculate behavioral transitions and transition times
      D14_NT_trans <- Tank_D14_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      
      NT_trans <- rbind(D1_NT_trans,D3_NT_trans,D5_NT_trans,D8_NT_trans,D9_NT_trans,
                        D10_NT_trans,D11_NT_trans,D12_NT_trans,D13_NT_trans,D14_NT_trans)
    }
    ## combine 
    Dyad_trans <- rbind(T_trans,NT_trans)
  }
  #Spawn#
  {
    #Territorial
    {
      # Create Spawn_T dataframe with rows where Subject is "Territorial"
      Spawn_T <- AllSpawn %>%
        filter(Subject == "Territorial")
      
      # Create separate dataframes for each TankID
      unique_tank_ids <- unique(Spawn_T$TankID)
      for (tank_id in unique_tank_ids) {
        df_name <- paste0("Tank_", tank_id)
        assign(df_name, filter(Spawn_T, TankID == tank_id))
      }
      
      for (tank_id in unique_tank_ids) {
        df_name <- paste0("Tank_", tank_id)
        assign(df_name, Spawn_T %>% filter(TankID == tank_id))
      }
      
      #S1_T
      #Calculate behavioral transitions and transition times
      S1_T_trans <- Tank_S1_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S3_T
      #Calculate behavioral transitions and transition times
      S3_T_trans <- Tank_S3_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S5_T
      #Calculate behavioral transitions and transition times
      S5_T_trans <- Tank_S5_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S8_T
      #Calculate behavioral transitions and transition times
      S8_T_trans <- Tank_S8_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S9_T
      #Calculate behavioral transitions and transition times
      S9_T_trans <- Tank_S9_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S10_T
      #Calculate behavioral transitions and transition times
      S10_T_trans <- Tank_S10_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S11_T
      #Calculate behavioral transitions and transition times
      S11_T_trans <- Tank_S11_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S12_T
      #Calculate behavioral transitions and transition times
      S12_T_trans <- Tank_S12_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S13_T
      #Calculate behavioral transitions and transition times
      S13_T_trans <- Tank_S13_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S14_T
      #Calculate behavioral transitions and transition times
      S14_T_trans <- Tank_S14_T %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      
      #combine
      T_trans <- rbind(S1_T_trans,S3_T_trans,S5_T_trans,S8_T_trans,S9_T_trans,S10_T_trans,
                       S11_T_trans,S12_T_trans,S13_T_trans,S14_T_trans)
    }
    #Non-territorial 
    {
      # Create Spawn_NT dataframe with rows where Subject is "Non-territorial"
      Spawn_NT <- AllSpawn %>%
        filter(Subject == "Non-territorial")
      
      unique_tank_ids <- unique(Spawn_NT$TankID)
      for (tank_id in unique_tank_ids) {
        df_name <- paste0("Tank_", tank_id)
        assign(df_name, filter(Spawn_NT, TankID == tank_id))
      }
      
      #S1_NT
      #Calculate behavioral transitions and transition times
      S1_NT_trans <- Tank_S1_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S3_NT
      #Calculate behavioral transitions and transition times
      S3_NT_trans <- Tank_S3_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S5_NT
      #Calculate behavioral transitions and transition times
      S5_NT_trans <- Tank_S5_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S8_NT
      #Calculate behavioral transitions and transition times
      S8_NT_trans <- Tank_S8_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S9_NT
      #Calculate behavioral transitions and transition times
      S9_NT_trans <- Tank_S9_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S10_NT
      #Calculate behavioral transitions and transition times
      S10_NT_trans <- Tank_S10_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S11_NT
      #Calculate behavioral transitions and transition times
      S11_NT_trans <- Tank_S11_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S12_NT
      #Calculate behavioral transitions and transition times
      S12_NT_trans <- Tank_S12_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S13_NT
      #Calculate behavioral transitions and transition times
      S13_NT_trans <- Tank_S13_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      #S14_NT
      #Calculate behavioral transitions and transition times
      S14_NT_trans <- Tank_S14_NT %>%
        arrange(Time) %>%
        mutate(Transition = paste0(lag(Behavior), "|", Behavior),
               Transition_time = Time - lag(Time)) %>%
        filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
      
      NT_trans <- rbind(S1_NT_trans,S3_NT_trans,S5_NT_trans,S8_NT_trans,S9_NT_trans,
                        S10_NT_trans,S11_NT_trans,S12_NT_trans,S13_NT_trans,S14_NT_trans)
    }
    ## combine 
    Spawn_trans <- rbind(T_trans,NT_trans)
    
  }
  #Combine Dyad and Spawn
  All_trans <- rbind(Dyad_trans,Spawn_trans)
  #Remove rows with NA values in the Transition_time column
  All_trans <- All_trans %>% filter(!is.na(Transition_time))
  #Dyad T vs Spawn T
  {
    #Paired t test
    # Filter the data for Territorial subjects
    Territorials <- All_trans %>% 
      filter(Subject == "Territorial")
    
    # Identify all unique transitions
    unique_transitions <- unique(Territorials$Transition)
    
    # Ensure both subjects have transition times for all unique transitions
    # and remove rows with missing data for either assay
    All_trans_complete <- Territorials %>%
      group_by(Transition) %>%
      filter(n_distinct(Assay) == 2) %>%
      ungroup() %>%
      drop_na(Transition_time)
    
    # Make all values in the Subject column say "Territorial"
    All_trans_complete <- All_trans_complete %>%
      mutate(Subject = "Territorial")
    
    # Perform paired t-tests on Transition_time for each unique transition
    paired_t_tests <- unique_transitions %>%
      map_df(~ {
        data <- filter(All_trans_complete, Transition == .x)
        if (nrow(data) < 2) return(NULL)  # Skip if there are insufficient data
        t_test_result <- tryCatch(t.test(Transition_time ~ Assay, data = data),
                                  error = function(e) NULL)
        if (is.null(t_test_result)) return(NULL)  # Skip if t-test fails
        data.frame(
          Transition = .x,
          statistic = t_test_result$statistic,
          p.value = t_test_result$p.value,
          mean_diff = diff(t_test_result$estimate),  # Mean difference between groups
          conf_low = t_test_result$conf.int[1],      # Lower bound of the confidence interval
          conf_high = t_test_result$conf.int[2],     # Upper bound of the confidence interval
          df = t_test_result$parameter,              # Degrees of freedom
          n = nrow(data)                             # Number of samples in the test
        )
      })
    
    # Remove rows with NA p-values
    paired_t_tests <- paired_t_tests %>%
      filter(!is.na(p.value))
    
    # Apply Benjamini-Hochberg correction
    paired_t_tests$Corrected_p_value <- p.adjust(paired_t_tests$p.value, method = "BH")
    
    # View the results
    print(paired_t_tests)
    
    write.csv(paired_t_tests, "Ts.decision.latency.paired.t.csv", row.names = TRUE)
    
    #Heat-map
    {
      ## Mean and SEM ##
      #Remove rows with NA values in the Transition_time column
      Territorials <- Territorials %>% filter(!is.na(Transition_time))
      #Group the dataframe by the Transition column
      Territorials_trans_grouped <- Territorials %>%
        group_by(Transition,Assay) %>%
        summarise(
          Mean_Transition_time = mean(Transition_time),
          SEM_Transition_time = sd(Transition_time) / sqrt(n())  # Calculate SEM
        )
      #write.csv(Territorials_trans_grouped, "Territorials_decision_making_latency.mean.sem.csv", row.names = TRUE)
      
      # Separate the Transition column into From_Behavior and To_Behavior
      Territorials_trans_grouped <- Territorials_trans_grouped %>%
        separate(Transition, into = c("From_Behavior", "To_Behavior"), sep = "\\|")
      
      # Create the heatmap with facets for each subject, tilted x-axis labels, customized gradient, and title
      p <- ggplot(Territorials_trans_grouped, aes(x = From_Behavior, y = To_Behavior, fill = Mean_Transition_time)) +
        geom_tile() +
        scale_fill_gradient(low = "#E6F5FF", high = "#004D99") + # Customizing gradient colors
        labs(x = "From Behavior", y = "To Behavior", fill = "Time (seconds)") +
        ggtitle("Decision Making Latency of Territorial Males Across Assays") +  # Adding title
        theme_minimal() +
        facet_wrap(~ Assay) +
        theme(
          axis.text = element_text(size = 16, family = "Times"),
          axis.title = element_text(size = 16, family = "Times"),
          plot.title = element_text(size = 16, family = "Times"),
          legend.text = element_text(size = 16, family = "Times"),
          legend.title = element_text(size = 16, family = "Times"),
          strip.text = element_text(size = 16, family = "Times"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      # Save the plot as a PNG file
      #ggsave("Territorials.decision_making_heatmap.png", p, width = 10, height = 6, bg = "white")
    }
    #Point-Plot
    {
      # Reorder the levels of the interaction variable alphabetically
      Territorials_trans_grouped$interaction_order <- with(Territorials_trans_grouped, 
                                                           reorder(interaction(From_Behavior, To_Behavior), 
                                                                   as.character(interaction(From_Behavior, To_Behavior))))
      
      point_plot <- ggplot(Territorials_trans_grouped, aes(x =Mean_Transition_time ,y = interaction_order, color = Assay)) +
        geom_point(position = position_dodge(width = 0.5)) +  # Add points with dodge position
        geom_errorbar(aes(xmin = Mean_Transition_time - SEM_Transition_time, xmax = Mean_Transition_time + SEM_Transition_time), 
                      width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
        labs(x = "Average Time (seconds)", y = "Behavioral Transitions", color = "Assay") +
        ggtitle("Average Decision Making Latency of Territorial Males Across Assays") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8, hjust = 0,family = "Times")) +
        scale_color_manual(values = c("Dyad" = "black", "Spawn" = "#CC5500"))
      
      
      # Print the point plot
      print(point_plot)
      #ggsave("1apr24Territorials_decision_making_point_plot.png", plot = point_plot, width = 8, height = 8, units = "in", dpi = 300)
    }
  }  
  #Dyad NT vs Spawn NT
  {
    #Paired t test
    {
      # Filter the data for Non-territorial subjects
      Non_territorials <- All_trans %>% 
        filter(Subject == "Non-territorial")
      
      # Identify all unique transitions
      unique_transitions <- unique(Non_territorials$Transition)
      
      # Ensure both subjects have transition times for all unique transitions
      # and remove rows with missing data for either assay
      All_trans_complete <- Non_territorials %>%
        group_by(Transition) %>%
        filter(n_distinct(Assay) == 2) %>%
        ungroup() %>%
        drop_na(Transition_time)
      
      # Make all values in the Subject column say "Non-territorial"
      All_trans_complete <- All_trans_complete %>%
        mutate(Subject = "Non-territorial")
      
      # Perform paired t-tests on Transition_time for each unique transition
      paired_t_tests <- unique_transitions %>%
        map_df(~ {
          data <- filter(All_trans_complete, Transition == .x)
          if (nrow(data) < 2) return(NULL)  # Skip if there are insufficient data
          t_test_result <- tryCatch(t.test(Transition_time ~ Assay, data = data),
                                    error = function(e) NULL)
          if (is.null(t_test_result)) return(NULL)  # Skip if t-test fails
          data.frame(
            Transition = .x,
            statistic = t_test_result$statistic,
            p.value = t_test_result$p.value,
            mean_diff = diff(t_test_result$estimate),  # Mean difference between groups
            conf_low = t_test_result$conf.int[1],      # Lower bound of the confidence interval
            conf_high = t_test_result$conf.int[2],     # Upper bound of the confidence interval
            df = t_test_result$parameter,              # Degrees of freedom
            n = nrow(data)                             # Number of samples in the test
          )
        })
      
      # Remove rows with NA p-values
      paired_t_tests <- paired_t_tests %>%
        filter(!is.na(p.value))
      
      # Apply Benjamini-Hochberg correction
      paired_t_tests$Corrected_p_value <- p.adjust(paired_t_tests$p.value, method = "BH")
      
      # View the results
      print(paired_t_tests)
      
      #write.csv(paired_t_tests, "NTs.decision.latency.paired.t.csv", row.names = TRUE)
      
      
    }
    #Heat-map
    {
      ## average
      #Remove rows with NA values in the Transition_time column
      Non_territorials <- Non_territorials %>% filter(!is.na(Transition_time))
      #Group the dataframe by the Transition column
      Non_territorials_trans_grouped <- Non_territorials %>%
        group_by(Transition,Assay) %>%
        summarise(
          Mean_Transition_time = mean(Transition_time),
          SEM_Transition_time = sd(Transition_time) / sqrt(n())  # Calculate SEM
        )
      #write.csv(Non_territorials_trans_grouped, "Non_Territorials_decision_making_latency.mean.sem.csv", row.names = TRUE)
      
      # Separate the Transition column into From_Behavior and To_Behavior
      Non_territorials_trans_grouped <- Non_territorials_trans_grouped %>%
        separate(Transition, into = c("From_Behavior", "To_Behavior"), sep = "\\|")
      
      # Create the heatmap with facets for each subject, tilted x-axis labels, customized gradient, and title
      p <- ggplot(Non_territorials_trans_grouped, aes(x = From_Behavior, y = To_Behavior, fill = Mean_Transition_time)) +
        geom_tile() +
        scale_fill_gradient(low = "#E6F5FF", high = "#004D99") + # Customizing gradient colors
        labs(x = "From Behavior", y = "To Behavior", fill = "Time (seconds)") +
        ggtitle("Decision Making Latency of Non_Territorial Males Across Assays") +  # Adding title
        theme_minimal() +
        facet_wrap(~ Assay) +
        theme(
          axis.text = element_text(size = 16, family = "Times"),
          axis.title = element_text(size = 16, family = "Times"),
          plot.title = element_text(size = 16, family = "Times"),
          legend.text = element_text(size = 16, family = "Times"),
          legend.title = element_text(size = 16, family = "Times"),
          strip.text = element_text(size = 16, family = "Times"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      # Save the plot as a PNG file
      #ggsave("Non_Territorials.decision_making_heatmap.png", p, width = 10, height = 6, bg = "white")
    }
    #Point-Plot
    {
      # Reorder the levels of the interaction variable alphabetically
      Non_territorials_trans_grouped$interaction_order <- with(Non_territorials_trans_grouped, 
                                                               reorder(interaction(From_Behavior, To_Behavior), 
                                                                       as.character(interaction(From_Behavior, To_Behavior))))
      
      point_plot <- ggplot(Non_territorials_trans_grouped, aes(x =Mean_Transition_time ,y = interaction_order, color = Assay)) +
        geom_point(position = position_dodge(width = 0.5)) +  # Add points with dodge position
        geom_errorbar(aes(xmin = Mean_Transition_time - SEM_Transition_time, xmax = Mean_Transition_time + SEM_Transition_time), 
                      width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
        labs(x = "Average Time (seconds)", y = "Behavioral Transitions", color = "Assay") +
        ggtitle("Average Decision Making Latency of Non-territorial Males Across Assays") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8, hjust = 0,family = "Times")) +
        scale_color_manual(values = c("Dyad" = "black", "Spawn" = "#CC5500"))
      
      
      # Print the point plot
      print(point_plot)
      ggsave("Non_Territorials_decidion_making_point_plot.png", plot = point_plot, width = 8, height = 8, units = "in", dpi = 300)
    }
  }
}
####ANALYSIS OF RELATIVE FREQUENCY OF T AND NT SUBJECTS ACROSS ASSAYS####
{
  AllIncidence <- rbind(AllDyad,AllSpawn)
  #Territorials
  {
    Territorials <- subset(AllIncidence, Subject == "Territorial")
    
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
    #write.csv(Incidence_tank, "Territorials.relative.incidence.csv", row.names = TRUE)
    
    # Calculate mean and SEM for each behavior
    mean_sem_perc <- Incidence_tank %>%
      group_by(Behavior,Assay) %>%
      summarise(
        Mean = mean(relative_incidence, na.rm = TRUE),
        SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
        .groups = "drop"
      )
    #write.csv(mean_sem_perc, "Territorials.mean.SEM.csv", row.names = TRUE)
    
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
      # Filter data for the current behavior
      behavior_data <- complete_incidence %>%
        filter(Behavior == behavior)
      
      # Perform pairwise t-test
      t_test_result <- tryCatch(
        t.test(relative_incidence ~ Assay, data = behavior_data, paired = TRUE),
        error = function(e) NULL
      )
      
      # Check if the t-test was successful
      if (is.null(t_test_result)) {
        return(data.frame(
          Behavior = behavior,
          Statistic = NA,
          P_Value = NA,
          Adjusted_P_Value = NA,
          Degrees_of_Freedom = NA
        ))
      }
      
      # Extract the p-values and other results
      p_value <- t_test_result$p.value
      statistic <- t_test_result$statistic
      df <- t_test_result$parameter
      
      # Return a data frame with results for this behavior
      data.frame(
        Behavior = behavior,
        Statistic = statistic,
        P_Value = p_value,
        Degrees_of_Freedom = df
      )
    })
    
    # Combine the results into a single dataframe
    results_df <- do.call(rbind, results)
    
    # Apply Benjamini-Hochberg correction to all p-values
    results_df$Adjusted_P_Value <- p.adjust(results_df$P_Value, method = "BH")
    
    # Save the results to a CSV file
    #write.csv(results_df, "Territorials.t-test.csv", row.names = FALSE)
    
    # Print the results
    print(results_df)
  }
  #Non-Territorials
  {
    Non_territorials <- subset(AllIncidence, Subject == "Non-territorial")
    
    # Create the Incidence_tank dataframe: how may times each behavior accurred in each tank
    Incidence_tank <- Non_territorials %>%
      group_by(TankID, Behavior,Assay) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Calculate total behaviors for each TankID : how many behaviors occurred in total in each tank 
    total_behaviors_per_tank <- Non_territorials %>%
      group_by(TankID) %>%
      summarise(TotalBehaviors = n(), .groups = "drop")
    
    # Join this total back to the Incidence_tank dataframe
    Incidence_tank <- Incidence_tank %>%
      left_join(total_behaviors_per_tank, by = "TankID")
    
    # Add the relative_incidence column to the Incidence_tank dataframe: #of each behavior in a tank/ total # of behaviors in that tank
    Incidence_tank <- Incidence_tank %>%
      mutate(relative_incidence = Count / TotalBehaviors)
    #write.csv(Incidence_tank, "Non-territorials.relative.incidence.csv", row.names = TRUE)
    
    # Calculate mean and SEM for each behavior
    mean_sem_perc <- Incidence_tank %>%
      group_by(Behavior,Assay) %>%
      summarise(
        Mean = mean(relative_incidence, na.rm = TRUE),
        SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
        .groups = "drop"
      )
    
    #write.csv(mean_sem_perc, "Non-territorials.mean.SEM.csv", row.names = TRUE)
    
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
      # Filter data for the current behavior
      behavior_data <- complete_incidence %>%
        filter(Behavior == behavior)
      
      # Perform pairwise t-test
      t_test_result <- tryCatch(
        t.test(relative_incidence ~ Assay, data = behavior_data, paired = TRUE),
        error = function(e) NULL
      )
      
      # Check if the t-test was successful
      if (is.null(t_test_result)) {
        return(data.frame(
          Behavior = behavior,
          Statistic = NA,
          P_Value = NA,
          Adjusted_P_Value = NA,
          Degrees_of_Freedom = NA
        ))
      }
      
      # Extract the p-values and other results
      p_value <- t_test_result$p.value
      statistic <- t_test_result$statistic
      df <- t_test_result$parameter
      
      # Return a data frame with results for this behavior
      data.frame(
        Behavior = behavior,
        Statistic = statistic,
        P_Value = p_value,
        Degrees_of_Freedom = df
      )
    })
    
    # Combine the results into a single dataframe
    results_df <- do.call(rbind, results)
    
    # Apply Benjamini-Hochberg correction to all p-values
    results_df$Adjusted_P_Value <- p.adjust(results_df$P_Value, method = "BH")
  
    write.csv(results_df, "Non-territorials.t-test.csv", row.names = FALSE)
    
    # Print the results
    print(results_df)
    
  }
}
####PLOT AVERAGE TIME TO INITIATE EACH BEHAVIOR PER SUBJECT IN THE SPAWN ASSAY####
{
  # Filter to keep only the first occurrence of each Behavior in each TankID
  AllSpawn_first <- AllSpawn %>%
    group_by(TankID, Behavior, Subject) %>%
    slice_min(Time, n = 1) %>%
    ungroup()
  
  # Create the ggplot with boxplot
  plot <- ggplot(AllSpawn_first, aes(x = Behavior, y = Time, fill = Subject)) +
    geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot with adjusted fattening
    stat_summary(fun = mean, geom = "crossbar", position = position_dodge(width = 0.75), width = 0.5) +  # Mean marker
    labs(title = "Time taken to Initiate Each Behavior by Subject", x = "Behavior", y = "Time (seconds)") +
    theme(
      axis.text.x = element_text(angle = 55, hjust = 1, family = "Times", size = 16),  # Times font and large size
      text = element_text(family = "Times", size = 20),  # General font settings
      plot.margin = unit(c(1, 1, 2, 1), "cm"),  # Increase bottom margin
      panel.background = element_rect(fill = "white"),  # White background for the panel
      plot.background = element_rect(fill = "white"),  # White background for the plot
    ) +
    scale_y_continuous(limits = c(0, max(AllSpawn_first$Time, na.rm = TRUE))) +  # Adjust y-axis limits
    scale_fill_manual(values = c("Territorial" = "red", "Non-territorial" = "blue"))  # Custom fill colors
  
  # Save the plot
  #ggsave("spawn_behavior_initiation_times_boxplot.png", plot = plot, width = 10, height = 6, dpi = 300)
}
