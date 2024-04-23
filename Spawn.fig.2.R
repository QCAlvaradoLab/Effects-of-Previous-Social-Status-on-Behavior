library(stats) 
library(dplyr) 
library(ggplot2) 
library(tidyr) 
library(patchwork)
library(purrr)

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
  #write.csv(Incidence_tank, "relative.incidence.csv", row.names = TRUE)
  
  # Calculate mean and SEM for each behavior
  mean_sem_perc <- Incidence_tank %>%
    group_by(Behavior,Subject) %>%
    summarise(
      Mean = mean(relative_incidence, na.rm = TRUE),
      SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
      .groups = "drop"
    )
  
  #write.csv(mean_sem_perc, "mean.aka.perc.sem.csv", row.names = TRUE)
}
#paired t-test (Dyad T vs Dyad NT)
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
  results <- lapply(unique(complete_incidence$Behavior), function(behavior) {
    t_test_result <- pairwise.t.test(
      complete_incidence$relative_incidence[complete_incidence$Behavior == behavior],
      complete_incidence$Subject[complete_incidence$Behavior == behavior],
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
  #write.csv(results_df, "paired_t_SpawnT_vsSpawnNT.csv", row.names = TRUE)
}
#Individual Behavior Plot
Plot.1 <- ggplot(complete_incidence, aes(x = Behavior, y = relative_incidence, fill = Subject)) +
  geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
  stat_summary(fun.y = mean, geom = "crossbar", 
               position = position_dodge(width = 0.75), width = 0.5) +
  labs(title = "All T/NT Behaviors in Spawn Assay: Relative Incidence", 
       x = "Behavior", y = "Relative Incidence") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        text = element_text(size = 10)) +  # Increase font size for all labels
  scale_y_continuous(limits = c(0, 1))

print(Plot.1)
#ggsave("Spawn.relative.Incidence.png", plot = Plot.1, width = 8, height = 8, units = "in")

# Categorical Analysis
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
      grepl("frontal display", Behavior) ~ "Agressive",
      grepl("flee from female", Behavior) ~ "Aversive",
      grepl("flee from male", Behavior) ~ "Aversive"
    ))
  # Separate data by categories
  aggressive <- subset(AllData, Category == "Agressive")
  aversive <- subset(AllData, Category == "Aversive")
  reproductive <- subset(AllData, Category == "Reproductive")
  
  # Define the significance level
  significance_level <- 0.05
  
  # Perform paired t-tests for each category
  results_aggressive <- pairwise.t.test(aggressive$relative_incidence,
                                        aggressive$Subject,
                                        paired = TRUE)
  results_aversive <- pairwise.t.test(aversive$relative_incidence,
                                      aversive$Subject,
                                      paired = TRUE)
  results_reproductive <- pairwise.t.test(reproductive$relative_incidence,
                                          reproductive$Subject,
                                          paired = TRUE)
  
  # Extract the p-values
  p_values_aggressive <- results_aggressive$p.value
  p_values_aversive <- results_aversive$p.value
  p_values_reproductive <- results_reproductive$p.value
  
  # Apply Benjamini-Hochberg correction
  p_adjusted_aggressive <- p.adjust(p_values_aggressive, method = "BH")
  p_adjusted_aversive <- p.adjust(p_values_aversive, method = "BH")
  p_adjusted_reproductive <- p.adjust(p_values_reproductive, method = "BH")
  
  # Create a dataframe to store the results
  results_df <- data.frame(
    Category = c("Aggressive", "Aversive", "Reproductive"),
    P_Value = c(p_values_aggressive, p_values_aversive, p_values_reproductive),
    Adjusted_P_Value = c(p_adjusted_aggressive, p_adjusted_aversive, p_adjusted_reproductive)
  )
  
  # Print the results dataframe
  print(results_df)
  #write.csv(results_df, "paired_t_SpawnT_vsSpawnNT.CAT.csv", row.names = TRUE)
}
# Categorical Behavior Plot
categorical.1 <- ggplot(AllData, aes(x = Category, y = relative_incidence, fill = Subject))+
  geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
  stat_summary(fun.y = mean, geom = "crossbar", 
               position = position_dodge(width = 0.75), width = 0.5) +
  labs(title = "T/NT Behavioral Categories in Spawn Assay: Relative Incidence", 
       x = "Behavioral Category", y = "Relative Incidence") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        text = element_text(size = 10))+ # Increase font size for all labels
  scale_y_continuous(limits = c(0, 1))

print(categorical.1)
#ggsave("Spawn.relative.Incidence.cat.png", plot = categorical.1, width = 8, height = 8, units = "in")
