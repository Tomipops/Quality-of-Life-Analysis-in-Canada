library(dplyr)  # dplyr is loaded
library(tidyverse)
library(ggplot2)

# Import the CSV file and save it as a tibble
Mental_Health <- read_csv("C:\\Users\\popoo\\OneDrive\\Desktop\\Mental_Health.csv")

# Dataset
print(Mental_Health)

# To see the structure of your data
str(Mental_Health)

# To view column names of the data set
names(Mental_Health)

# To get the total number of missing Data in Each column
colSums(is.na(Mental_Health))

# Remove STATUS, SYMBOL, and TERMINATED columns with many missing values
Mental_Health <- Mental_Health[,-c(2:3,9,11:13,15:18)]
Mental_Health

# Change the date format
Mental_Health$REF_DATE <- as.Date(paste0(Mental_Health$REF_DATE, "-01"), format = "%Y-%m-%d")

# check the first 5 rows to verify conversion
head(Mental_Health$REF_DATE)

# Rename columns
mh_data <- Mental_Health %>% 
  rename(Date = REF_DATE, SocioDemo = `Sociodemographic characteristics`, Value = VALUE)

# Check the updated column names
names(mh_data)

# replace NA's in Value column with 0
mh_data <- mh_data %>%
  mutate(Value = ifelse(is.na(Value), 0, Value))

# check if there are any NA values in Value Column
is.na(mh_data$Value)

# Recode `Indicators` to numeric values for clarity
mh_data <- mh_data %>% mutate(
  Indicators = recode(
    Indicators,
    "Excellent or very good perceived mental health" = "1",
    "Good perceived mental health" = "2",
    "Fair or poor perceived mental health" = "3"
  )
)


# Plot For Mental Health by Age
# Filter and group data by sociodemographic age groups
mental_health_by_age <- mh_data %>%
  filter(
    Indicators %in% c("1", "3") &  # Excellent = "1", Poor = "3"
      SocioDemo %in% c("15 to 24 years", "25 to 34 years", "35 to 44 years",
                       "45 to 54 years", "55 to 64 years", "65 years and over")
  ) %>%
  group_by(SocioDemo, Indicators) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE)) %>% 
  mutate(SocioDemo = recode(SocioDemo,
                            "15 to 24 years" = "15 - 24",
                            "25 to 34 years" = "25 - 34",
                            "35 to 44 years" = "35 - 44",
                            "45 to 54 years" = "45 - 54",
                            "55 to 64 years" = "55 - 64",
                            "65 years and over" = "65+")) %>%
  ungroup() %>%
  pivot_wider(names_from = Indicators, values_from = Total_Value, values_fill = 0) %>%
  rename(Excellent = `1`, Poor = `3`) %>%
  mutate(
    Total = Excellent + Poor,
    Excellent = (Excellent / Total) * 100,
    Poor = (Poor / Total) * 100
  ) %>%
  select(-Total)

# Convert to matrix for barplot
bar_data_age <- as.matrix(mental_health_by_age[, -1])  # Remove SocioDemo column
rownames(bar_data_age) <- mental_health_by_age$SocioDemo

# Create grouped bar chart with horizontal grid lines
barplot_positions <- barplot(
  t(bar_data_age),
  beside = TRUE,
  col = c("darkseagreen", "orange"),
  main = "Perceived Mental Health by Age Group",
  xlab = "Age Group",
  ylab = "Percentage",
  ylim = c(0, 100),  # Set y-axis range for percentages
  legend.text = c("Excellent", "Poor"),
  args.legend = list(x = "topleft", bty = "n"),
  las = 2  # Rotate x-axis labels
)

# Add horizontal grid lines at regular intervals
abline(h = seq(0, 100, by = 10), col = "gray", lty = "dotted", lwd = 0.8)

# Add percentage labels for the bars
text(
  x = as.vector(barplot_positions),
  y = as.vector(t(bar_data_age)),
  labels = paste0(round(as.vector(t(bar_data_age)), 1), "%"),
  pos = 3,
  cex = 0.8,
  col = "black"
)

