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
#relative incidence 
{
# Create the Incidence_tank dataframe: how may times each behavior accurred in each tank
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
#write.csv(Incidence_tank, "relative.incidence.csv", row.names = TRUE)

# Calculate mean and SEM for each behavior
mean_sem_perc <- Incidence_tank %>%
  group_by(Behavior,Subject) %>%
  summarise(
    Mean = mean(relative_incidence, na.rm = TRUE),
    SEM = sd(relative_incidence, na.rm = TRUE) / sqrt(n()), # Calculate SEM
    .groups = "drop"
  )

#write.csv(mean_sem_perc, "mean.aka.perc.sem", row.names = TRUE)
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
#write.csv(results_df, "paired_t_DyadT_vsDyadNT.csv", row.names = FALSE)
}

#Individual Behavior Plot
Plot.1 <- ggplot(complete_incidence, aes(x = Behavior, y = relative_incidence, fill = Subject)) +
  geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
  stat_summary(fun.y = mean, geom = "crossbar", 
               position = position_dodge(width = 0.75), width = 0.5) +
  labs(title = "All T/NT Behaviors in Dyad Assay: Relative Incidence", 
       x = "Behavior", y = "Relative Incidence") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        text = element_text(size = 10)) +  # Increase font size for all labels
  scale_y_continuous(limits = c(0, 1))+
  geom_text(data = complete_incidence, aes(label = "*"), x = 6, y = 0.9, vjust = -0.5, size = 8) +
  geom_text(data = complete_incidence, aes(label = "*"), x = 10, y = 0.9, vjust = -0.5, size = 8) +
  geom_text(data = complete_incidence, aes(label = "*"), x = 11, y = 0.9, vjust = -0.5, size = 8)


print(Plot.1)
#ggsave("Dyad.relative.Incidence.png", plot = Plot.1, width = 8, height = 8, units = "in")

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
#write.csv(results_df, "paired_t_DyadT_vsDyadNT.CAT.csv", row.names = TRUE)
}

# Categorical Behavior Plot
categorical.1 <- ggplot(AllData, aes(x = Category, y = relative_incidence, fill = Subject))+
  geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
  stat_summary(fun.y = mean, geom = "crossbar", 
               position = position_dodge(width = 0.75), width = 0.5) +
  labs(title = "T/NT Behavioral Categories in Dyad Assay: Relative Incidence", 
       x = "Behavioral Category", y = "Relative Incidence") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        text = element_text(size = 10))+ # Increase font size for all labels
  scale_y_continuous(limits = c(0, 1))+
  geom_text(data = AllData, aes(label = "*"), x = 1, y = 0.9, vjust = -0.5, size = 8)+
  geom_text(data = AllData, aes(label = "*"), x = 2, y = 0.9, vjust = -0.5, size = 8)

print(categorical.1)
#ggsave("Dyad.relative.Incidence.cat.png", plot = categorical.1, width = 8, height = 8, units = "in")
