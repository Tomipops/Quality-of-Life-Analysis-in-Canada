# Load required libraries
library(dplyr)
library(ggplot2)

# Check dataset dimensions and missing values
Discrimination_clean # Checking the dimension of the dataset. It has 8,640 observations and 8 variables.
colSums(is.na(Discrimination_clean)) # There are lots of missing data in the value column, so I will replace it with the mean of that column.
mean_value <- mean(Discrimination_clean$Value, na.rm = TRUE) # Getting the mean of the Value variable (excluding the missing values)
Discrimination_clean$Value[is.na(Discrimination_clean$Value)] <- mean_value # Replacing the missing values with the mean.
colSums(is.na(Discrimination_clean)) # Confirming that the missing values were replaced.
unique(Discrimination_clean$VisibleMinority)

# First, filter out the totals on the VisibleMinority variable and ensure we only have percentages in the 'Statistics' column
Discrimination_clean_filtered <- Discrimination_clean %>%
  filter(VisibleMinority != "Total, by visible minority group" & 
           Statistics == "Percentage of persons") 

# Summarizing the data by Visible Minority
visible_minority_summary <- Discrimination_clean_filtered %>%
  group_by(VisibleMinority) %>%
  summarize(
    percentage_value = mean(Value, na.rm = TRUE), 
    total_percentage = sum(Value, na.rm = TRUE)
  )
print(visible_minority_summary) 

# To show this insight on a dot plot
ggplot(visible_minority_summary, aes(x = reorder(VisibleMinority, -percentage_value), y = percentage_value, color = VisibleMinority)) +
  geom_point(size = 4) +  
  labs(
    title = "Visible Minority Groups in Canada (2020)",
    x = "Visible Minority Group",
    y = "Percentage of Persons"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

# Check unique values in the Indicators column to filter for specific indicators related to discrimination at work
unique(Discrimination_clean$Indicators)

# Filter for relevant indicators (discrimination at work, before and during COVID-19)
Discrimination_clean_filtered_indicators <- Discrimination_clean_filtered %>%
  filter(Indicators %in% c(
    "Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic", 
    "Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic"
  ))


discrimination_work_summary <- Discrimination_clean_filtered_indicators %>%
  group_by(VisibleMinority, Indicators) %>%
  summarize(
    percentage_value = mean(Value, na.rm = TRUE), 
    total_percentage = sum(Value, na.rm = TRUE)
  )

# Print the summary for inspection
ggplot(discrimination_work_summary, aes(x = Indicators, 
                                                 y = percentage_value, 
                                                 fill = Indicators)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Discrimination at the Workplace",
    x = "Period",
    y = "Percentage of Persons"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "grey",  # Before COVID-19
    "orange"   # Since the Beginning of COVID-19
  )) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c(
    "Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic" = "Before COVID",
    "Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic" = "During COVID"
  ))  


# Filter the dataset to include only the relevant age groups
Discrimination_clean_filtered_age_grouped <- Discrimination_clean %>%
  filter(AgeGroup %in% c("15-24 years", "25-64 years", "65 years and over"))

# Summarize the data by AgeGroup and Indicators
discrimination_age_summary <- Discrimination_clean_filtered_age_grouped %>%
  group_by(AgeGroup, Indicators) %>%
  summarize(
    average_discrimination = mean(Value, na.rm = TRUE)
  )

# Create a bar chart for the three age groups
ggplot(discrimination_age_summary, aes(x = AgeGroup, y = average_discrimination, fill = Indicators)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Discrimination by Age Group Before and Since the Start of COVID-19",
    x = "Age Group",
    y = "Percentage of Persons (%)",
    fill = "Period"
  ) +
  scale_fill_manual(values = c(
    "Experience(s) of discrimination, 5 years before COVID-19 pandemic" = "orange",
    "Experience(s) of discrimination since the beginning of COVID-19 pandemic" = "grey"
  ),
  labels = c("Before COVID-19", "Since the Beginning of COVID-19")  # Adding custom labels for the legend
  ) +
  theme_minimal() +
  theme(legend.position = "top")


