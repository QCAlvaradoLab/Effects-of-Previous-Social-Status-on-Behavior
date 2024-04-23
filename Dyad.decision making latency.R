library(stats) 
library(dplyr) 
library(ggplot2) 
library(tidyr) 
library(patchwork)
library(purrr)

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
All_trans <- rbind(T_trans,NT_trans)
#paired t-test
{
  # Step 1: Identify all unique transitions
  unique_transitions <- unique(All_trans$Transition)
  
  # Step 2: Ensure both subjects have transition times for all unique transitions
  All_trans_complete <- All_trans %>%
    complete(Transition, nesting(Subject)) %>%
    mutate(Transition_time = ifelse(is.na(Transition_time), 0, Transition_time))
  
  # Step 3: Perform paired t-tests on Transition_time for each unique transition
  paired_t_tests <- unique_transitions %>%
    map_df(~ {
      data <- filter(All_trans_complete, Transition == .x)
      if (nrow(data) < 2) return(NULL)  # Skip if there are insufficient data
      t_test_result <- tryCatch(t.test(Transition_time ~ Subject, data = data),
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
  
  #write.csv(paired_t_tests, "Dyad.decision.latency.paired.t", row.names = TRUE)
  
}
## average
# Step 1: Remove rows with NA values in the Transition_time column
All_trans <- All_trans %>% filter(!is.na(Transition_time))
# Step 2: Group the dataframe by the Transition column
All_trans_grouped <- All_trans %>%
  group_by(Transition,Subject) %>%
  summarise(
    Mean_Transition_time = mean(Transition_time),
    SEM_Transition_time = sd(Transition_time) / sqrt(n())  # Calculate SEM
  )
#write.csv(All_trans_grouped, "Dyad_decision_making_latency.mean.sem.csv", row.names = TRUE)

## heatmap
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as a PNG file
#ggsave("Dyad.decision_making_heatmap.png", p, width = 10, height = 6, bg = "white")
}
## point plot
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
  theme(axis.text.y = element_text(size = 8, hjust = 0)) +
  scale_color_manual(values = c("Territorial" = "blue", "Non-territorial" = "red"))

# Print the point plot
print(point_plot)
#ggsave("Dyad_decidion_making_point_plot.png", plot = point_plot, width = 8, height = 6, units = "in", dpi = 300)
}