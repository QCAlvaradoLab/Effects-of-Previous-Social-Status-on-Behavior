library(stats) 
library(dplyr) 
library(ggplot2) 
library(tidyr) 
library(patchwork)
library(purrr)

#Dyad decision making latencies 
{
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
  Dyad_NT$Subject <- "Non_Territorial"
  
  AllDyad <- rbind(Dyad_NT, Dyad_T)
  #write.csv(AllDyad, "AllDyad.csv", row.names = TRUE)
  DyadT <- subset(AllDyad, Subject == "Territorial")
  DyadNT <- subset(AllDyad, Subject == "Non_Territorial")
}
## T
{
  
  # Step 1: Convert the 'Time' column to seconds
  convert_time_to_seconds <- function(time_str) {
    time_parts <- strsplit(time_str, ":", fixed = TRUE)[[1]]
    minutes <- as.numeric(time_parts[1])
    seconds <- as.numeric(time_parts[2])
    total_seconds <- minutes * 60 + seconds
    return(total_seconds)
  }
  
  # Convert 'Time' column to seconds
  DyadT$Time_seconds <- sapply(DyadT$Time, convert_time_to_seconds)
  
  # Get unique TankIDs
  unique_tank_ids <- unique(DyadT$TankID)
 
   # Create separate dataframes for each TankID
  for (tank_id in unique_tank_ids) {
    df_name <- paste0("Tank_", tank_id)
    assign(df_name, DyadT %>% filter(TankID == tank_id))
  }
  
  #D1_T
  #Calculate behavioral transitions and transition times
  D1_T_trans <- Tank_D1_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #D3_T
  #Calculate behavioral transitions and transition times
  D3_T_trans <- Tank_D3_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #D5_T
  #Calculate behavioral transitions and transition times
  D5_T_trans <- Tank_D5_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #D8_T
  #Calculate behavioral transitions and transition times
  D8_T_trans <- Tank_D8_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #combine
  T_trans <- rbind(D1_T_trans,D3_T_trans,D5_T_trans,D8_T_trans)
}
## NT
{
  # Step 1: Convert the 'Time' column to seconds
  convert_time_to_seconds <- function(time_str) {
    time_parts <- strsplit(time_str, ":", fixed = TRUE)[[1]]
    minutes <- as.numeric(time_parts[1])
    seconds <- as.numeric(time_parts[2])
    total_seconds <- minutes * 60 + seconds
    return(total_seconds)
  }
  # Convert 'Time' column to seconds
  Dyad_NT$Time_seconds <- sapply(Dyad_NT$Time, convert_time_to_seconds)
  
  # Step 2: Create separate dataframes for each TankID
  unique_tank_ids <- unique(Dyad_NT$TankID)
  for (tank_id in unique_tank_ids) {
    df_name <- paste0("Tank_", tank_id)
    assign(df_name, filter(Dyad_NT, TankID == tank_id))
  }
  #D1_NT
  #Calculate behavioral transitions and transition times
  D1_NT_trans <- Tank_D1_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #D3_NT
  #Calculate behavioral transitions and transition times
  D3_NT_trans <- Tank_D3_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #D5_NT
  #Calculate behavioral transitions and transition times
  D5_NT_trans <- Tank_D5_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #D8_NT
  #Calculate behavioral transitions and transition times
  D8_NT_trans <- Tank_D8_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  NT_trans <- rbind(D1_NT_trans,D3_NT_trans,D5_NT_trans,D8_NT_trans)
}
## combine 
Dyad_trans <- rbind(T_trans,NT_trans)
}

#Spawn decision making latencies 
{
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

## T
{
  # Step 1: Convert the 'Time' column to seconds
  convert_time_to_seconds <- function(time_str) {
    time_parts <- strsplit(time_str, ":", fixed = TRUE)[[1]]
    minutes <- as.numeric(time_parts[1])
    seconds <- as.numeric(time_parts[2])
    total_seconds <- minutes * 60 + seconds
    return(total_seconds)
  }
  
  # Convert 'Time' column to seconds
  SpawnT$Time_seconds <- sapply(SpawnT$Time, convert_time_to_seconds)
  
  # Get unique TankIDs
  unique_tank_ids <- unique(SpawnT$TankID)
  
  # Create separate dataframes for each TankID
  for (tank_id in unique_tank_ids) {
    df_name <- paste0("Tank_", tank_id)
    assign(df_name, SpawnT %>% filter(TankID == tank_id))
  }
  
  #S1_T
  #Calculate behavioral transitions and transition times
  S1_T_trans <- Tank_S1_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #S3_T
  #Calculate behavioral transitions and transition times
  S3_T_trans <- Tank_S3_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #S5_T
  #Calculate behavioral transitions and transition times
  S5_T_trans <- Tank_S5_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #S8_T
  #Calculate behavioral transitions and transition times
  S8_T_trans <- Tank_S8_T %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #combine
  T_trans <- rbind(S1_T_trans,S3_T_trans,S5_T_trans,S8_T_trans)
}
## NT
{
  # Step 1: Convert the 'Time' column to seconds
  convert_time_to_seconds <- function(time_str) {
    time_parts <- strsplit(time_str, ":", fixed = TRUE)[[1]]
    minutes <- as.numeric(time_parts[1])
    seconds <- as.numeric(time_parts[2])
    total_seconds <- minutes * 60 + seconds
    return(total_seconds)
  }
  
  # Convert 'Time' column to seconds
  SpawnNT$Time_seconds <- sapply(SpawnNT$Time, convert_time_to_seconds)
  
  # Get unique TankIDs
  unique_tank_ids <- unique(SpawnNT$TankID)
  
  # Create separate dataframes for each TankID
  for (tank_id in unique_tank_ids) {
    df_name <- paste0("Tank_", tank_id)
    assign(df_name, SpawnNT %>% filter(TankID == tank_id))
  }
  
  #S1_NT
  #Calculate behavioral transitions and transition times
  S1_NT_trans <- Tank_S1_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #S3_NT
  #Calculate behavioral transitions and transition times
  S3_NT_trans <- Tank_S3_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #S5_NT
  #Calculate behavioral transitions and transition times
  S5_NT_trans <- Tank_S5_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #S8_NT
  #Calculate behavioral transitions and transition times
  S8_NT_trans <- Tank_S8_NT %>%
    arrange(Time_seconds) %>%
    mutate(Transition = paste0(lag(Behavior), "|", Behavior),
           Transition_time = Time_seconds - lag(Time_seconds)) %>%
    filter(!is.na(Transition))  # Remove the first row where there's no previous behavior
  #combine
  NT_trans <- rbind(S1_NT_trans,S3_NT_trans,S5_NT_trans,S8_NT_trans)
}
## combine 
Spawn_trans <- rbind(T_trans,NT_trans)
}

#Combine Dyad and Spawn
All_trans <- rbind(Dyad_trans,Spawn_trans)

#Dyad T vs Spawn T
{
#Paired t test
{
Territorials <- All_trans[All_trans$Subject == "Territorial", ]

# Step 1: Identify all unique transitions
unique_transitions <- unique(Territorials$Transition)

# Step 2: Ensure both subjects have transition times for all unique transitions
All_trans_complete <- Territorials %>%
  complete(Transition, nesting(Assay)) %>%
  mutate(Transition_time = ifelse(is.na(Transition_time), 0, Transition_time))
# Make all values in the Subject column say "Territorial"
All_trans_complete <- All_trans_complete %>%
  mutate(Subject = "Territorial")

# Step 3: Perform paired t-tests on Transition_time for each unique transition
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
      p.value = t_test_result$p.value
    )
  })

# Step 4: Remove rows with NA p-values
paired_t_tests <- paired_t_tests %>%
  filter(!is.na(p.value))

# Step 5: Apply Benjamini-Hochberg correction
paired_t_tests$Corrected_p_value <- p.adjust(paired_t_tests$p.value, method = "BH")

# View the results
print(paired_t_tests)
write.csv(paired_t_tests, "Ts.decision.latency.paired.t.csv", row.names = TRUE)
}
#Heat-map
{
## average
# Step 1: Remove rows with NA values in the Transition_time column
Territorials <- Territorials %>% filter(!is.na(Transition_time))
# Step 2: Group the dataframe by the Transition column
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  theme(axis.text.y = element_text(size = 8, hjust = 0)) +
  scale_color_manual(values = c("Dyad" = "blue", "Spawn" = "red"))

# Print the point plot
print(point_plot)
#ggsave("Territorials_decidion_making_point_plot.png", plot = point_plot, width = 8, height = 6, units = "in", dpi = 300)
}
}  

#Dyad NT vs Spawn NT
{
  #Paired t test
  {
    Non_Territorials <- All_trans[All_trans$Subject == "Non_Territorial", ]
    
    # Step 1: Identify all unique transitions
    unique_transitions <- unique(Non_Territorials$Transition)
    
    # Step 2: Ensure both subjects have transition times for all unique transitions
    All_trans_complete <- Non_Territorials %>%
      complete(Transition, nesting(Assay)) %>%
      mutate(Transition_time = ifelse(is.na(Transition_time), 0, Transition_time))
    # Make all values in the Subject column say "Territorial"
    All_trans_complete <- All_trans_complete %>%
      mutate(Subject = "Non_Territorial")
    
    # Step 3: Perform paired t-tests on Transition_time for each unique transition
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
          p.value = t_test_result$p.value
        )
      })
    
    # Step 4: Remove rows with NA p-values
    paired_t_tests <- paired_t_tests %>%
      filter(!is.na(p.value))
    
    # Step 5: Apply Benjamini-Hochberg correction
    paired_t_tests$Corrected_p_value <- p.adjust(paired_t_tests$p.value, method = "BH")
    
    # View the results
    print(paired_t_tests)
    #write.csv(paired_t_tests, "NTs.decision.latency.paired.t.csv", row.names = TRUE)
  }
  #Heat-map
  {
    ## average
    # Step 1: Remove rows with NA values in the Transition_time column
    Non_Territorials <- Non_Territorials %>% filter(!is.na(Transition_time))
    # Step 2: Group the dataframe by the Transition column
    Non_Territorials_trans_grouped <- Non_Territorials %>%
      group_by(Transition,Assay) %>%
      summarise(
        Mean_Transition_time = mean(Transition_time),
        SEM_Transition_time = sd(Transition_time) / sqrt(n())  # Calculate SEM
      )
    #write.csv(Non_Territorials_trans_grouped, "Non_Territorials_decision_making_latency.mean.sem.csv", row.names = TRUE)
    
    # Separate the Transition column into From_Behavior and To_Behavior
    Non_Territorials_trans_grouped <- Non_Territorials_trans_grouped %>%
      separate(Transition, into = c("From_Behavior", "To_Behavior"), sep = "\\|")
    
    # Create the heatmap with facets for each subject, tilted x-axis labels, customized gradient, and title
    p <- ggplot(Non_Territorials_trans_grouped, aes(x = From_Behavior, y = To_Behavior, fill = Mean_Transition_time)) +
      geom_tile() +
      scale_fill_gradient(low = "#E6F5FF", high = "#004D99") + # Customizing gradient colors
      labs(x = "From Behavior", y = "To Behavior", fill = "Time (seconds)") +
      ggtitle("Decision Making Latency of Non_Territorial Males Across Assays") +  # Adding title
      theme_minimal() +
      facet_wrap(~ Assay) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save the plot as a PNG file
    #ggsave("Non_Territorials.decision_making_heatmap.png", p, width = 10, height = 6, bg = "white")
  }
  #Point-Plot
  {
    # Reorder the levels of the interaction variable alphabetically
    Non_Territorials_trans_grouped$interaction_order <- with(Non_Territorials_trans_grouped, 
                                                             reorder(interaction(From_Behavior, To_Behavior), 
                                                                     as.character(interaction(From_Behavior, To_Behavior))))
    
    point_plot <- ggplot(Non_Territorials_trans_grouped, aes(x =Mean_Transition_time ,y = interaction_order, color = Assay)) +
      geom_point(position = position_dodge(width = 0.5)) +  # Add points with dodge position
      geom_errorbar(aes(xmin = Mean_Transition_time - SEM_Transition_time, xmax = Mean_Transition_time + SEM_Transition_time), 
                    width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
      labs(x = "Average Time (seconds)", y = "Behavioral Transitions", color = "Assay") +
      ggtitle("Average Decision Making Latency of Non_Territorial Males Across Assays") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8, hjust = 0)) +
      scale_color_manual(values = c("Dyad" = "blue", "Spawn" = "red"))
    
    # Print the point plot
    print(point_plot)
    #ggsave("Non_Territorials_decidion_making_point_plot.png", plot = point_plot, width = 8, height = 6, units = "in", dpi = 300)
  }
}