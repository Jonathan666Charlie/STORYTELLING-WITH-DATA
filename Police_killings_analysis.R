knitr::opts_chunk$set(echo = TRUE)
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load data
police_data <- read.csv("police_killings.csv", encoding = "ISO-8859-1")
data <- read.csv("police_killings.csv", encoding = "ISO-8859-1")

# Convert columns to numeric where necessary
police_data$age <- as.numeric(police_data$age)
police_data$share_white <- as.numeric(police_data$share_white)
police_data$share_black <- as.numeric(police_data$share_black)
police_data$share_hispanic <- as.numeric(police_data$share_hispanic)
police_data$p_income <- as.numeric(police_data$p_income)
police_data$h_income <- as.numeric(police_data$h_income)



# Handle missing values - Remove rows with missing income data
police_data_clean <- police_data[!is.na(police_data$h_income), ]

# Categorize states into Red, Blue, and Unaffiliated
# Define Red, Blue, and Unaffiliated states (simplified example; adjust as needed)
red_states <- c("AL", "TX", "FL", "MS", "TN", "SC", "LA", "OK", "AR", "KY", "WV", "ND", "SD", "WY", "MT", "ID", "UT", "KS", "NE")
blue_states <- c("CA", "NY", "WA", "MA", "IL", "NJ", "OR", "MD", "CT", "HI", "VT", "DE", "RI")
unaffiliated_states <- setdiff(unique(police_data_clean$state), c(red_states, blue_states))

# Create a new column for state affiliation
police_data_clean$state_affiliation <- ifelse(police_data_clean$state %in% red_states, "Red",
                                              ifelse(police_data_clean$state %in% blue_states, "Blue", "Unaffiliated"))

# Ensure the new column is a factor
police_data_clean$state_affiliation <- factor(police_data_clean$state_affiliation)
library(gtsummary)

# Summarizing police killings dataset with key variables (age, gender, raceethnicity, income)
table2 <- police_data_clean |> 
  tbl_summary(
    include = c(age,gender,  raceethnicity, p_income, h_income, county_income),  # Variables to summarize
    by = state_affiliation,  # Split the table by state
    missing = "no"  # Do not list missing data separately
  ) |> 
  add_n() |>  # Add column with total number of non-missing observations
  add_p() |>  # Test for a difference between groups (state in this case)
  modify_header(label = "**Variable**") |>  # Update column header
  bold_labels()  # Bold labels for clarity

# View the summary table
table2



# Count causes of death by race/ethnicity
cause_race <- data %>%
  group_by(raceethnicity, cause) %>%
  summarise(count = n()) %>%
  ungroup()

# Visualize
ggplot(cause_race, aes(x = cause, y = count, fill = raceethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Disparities in Cause of Death by Race/Ethnicity",
       x = "Cause of Death",
       y = "Count") +
  theme_minimal()

# Aggregate data to county level
county_data <- police_data_clean %>%
  group_by(county_id, state_affiliation) %>%
  summarise(
    killings = n(),
    median_income = mean(h_income, na.rm = TRUE),
    poverty_rate = mean(as.numeric(pov), na.rm = TRUE),
    unemployment_rate = mean(urate, na.rm = TRUE),
    college_graduates = mean(college, na.rm = TRUE)
  )

# Check for missing values in the aggregated data
county_data <- na.omit(county_data)


# Convert 'armed' and 'cause' to factors
police_data_clean$armed <- as.factor(police_data_clean$armed)
police_data_clean$cause <- as.factor(police_data_clean$cause)

# Aggregate data to county level
county_data <- police_data_clean %>%
  group_by(county_id, state_affiliation) %>%
  summarise(
    killings = sum(pop),  # Use 'pop' to represent killings
    median_income = mean(h_income, na.rm = TRUE),
    poverty_rate = mean(as.numeric(pov), na.rm = TRUE),
    unemployment_rate = mean(urate, na.rm = TRUE),
    college_graduates = mean(college, na.rm = TRUE),
    armed_mode = names(sort(table(armed), decreasing = TRUE)[1]),  # Most frequent value
    cause_mode = names(sort(table(cause), decreasing = TRUE)[1])   # Most frequent value
  )

# Handle missing values
county_data <- na.omit(county_data)

# Convert 'armed_mode' and 'cause_mode' to factors
county_data$armed_mode <- as.factor(county_data$armed_mode)
county_data$cause_mode <- as.factor(county_data$cause_mode)

# Fit Poisson regression model
poisson_model <- glm(killings ~ median_income + poverty_rate + unemployment_rate +
                       college_graduates + state_affiliation +
                       armed_mode + cause_mode,
                     data = county_data,
                     family = poisson)

# Display regression summary
summary(poisson_model)



# Visualize the effect of state affiliation on killings
ggplot(county_data, aes(x = state_affiliation, y = killings, fill = state_affiliation)) +
  geom_boxplot() +
  labs(title = "Distribution of Killings (pop) by State Affiliation",
       x = "State Affiliation",
       y = "Number of Killings (pop)") +
  theme_minimal()

# Visualize the effect of 'armed' and 'cause' on killings
ggplot(county_data, aes(x = armed_mode, y = killings, fill = armed_mode)) +
  geom_boxplot() +
  labs(title = "Killings by Weapon Type (armed)",
       x = "Weapon Type",
       y = "Number of Killings") +
  theme_minimal()

ggplot(county_data, aes(x = cause_mode, y = killings, fill = cause_mode)) +
  geom_boxplot() +
  labs(title = "Killings by Cause",
       x = "Cause of Death",
       y = "Number of Killings") +
  theme_minimal()
