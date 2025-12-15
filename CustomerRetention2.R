#SPORT CUSTOMER RETENTION MODEL: 

#Retaining loyal and engaged fans is paramount for the long-term success and sustainability of any sports franchise. 
#But this is not always easy as the data often arrives in a challenging format

#This data set was dumped from an NFL team’s ticketing system. 
#The primary goal of the investigation is to understand customer retention, specifically, the factors that predict whether or not a customer (season ticket holders in this case) will return. 
#The team has experienced some success in recent years with achieving two invitations to the playoffs in 2018 and 2019. 
#The team achieved the following win percentages in each season: 2018 (.688), 2019 (.625), 2020 (.250), 2021 (.235), 2022 (.206), 2023 (.429). 
#The data set excludes 2020 given the pandemic year. 

#packages utilized 
library(dplyr) #data manipulation, including grouping, summarizing, and filtering data
library(ggplot2) #data vis
library(scales) #enhancing the appearance of plots 

Retention_Dataset<-New_Retention_Dataset

#Key commands
#apply - applying functions without a loop 
#%>% - pass the output of one function directly into the next,

#First, think about the variables and their definitions.
  #Which variables do you think might be predictive? 
  #Which column represents the retention/outcome variable? 

#Second, let's consider some preliminary data cleaning and summarizing. 

Retention_Dataset <- readxl::read_excel("New_Retention_Dataset.xlsx")

# Count the number of duplicate rows
num_duplicates <- sum(duplicated(Retention_Dataset))
# Print the result
print(num_duplicates)
# Finding duplicate rows
duplicate_rows <- Retention_Dataset[duplicated(Retention_Dataset), ]
# Display duplicate rows
print(duplicate_rows) 
# Remove only one occurrence of each duplicate row, keeping the first occurrence
Retention_unique <- Retention_Dataset[!duplicated(Retention_Dataset), ]
# Display the data frame with only one instance of each row
View(Retention_unique)

summary(Retention_unique)
#What does the summary help us understand about each variable? 
#Specifically consider row_name, resale variables and team success measures 

# Convert all character columns to factors
Retention_factor <- Retention_unique
Retention_factor[] <- lapply(Retention_unique, function(x) if (is.character(x)) as.factor(x) else x)
# What other variables might be best considered as factors? 

# Check the structure to confirm conversion
str(Retention_factor)

#Summarize the variables with factor levels 
factor_summary <- sapply(Retention_factor[sapply(Retention_factor, is.factor)], summary)

# Display the summary
factor_summary
#What does the summary help us understand about the variables with factor levels? 

#We will reconstruct our dataframe from day 1 and continue in steps toward our model 

summary(Retention_factor) 
#There are NAs present - why should we not remove them now? 
str(Retention_factor)

# Step 1: Remove unnecessary accounts
# Removes accounts with 'acct_id' equal to "0" or marked as comp accounts (FLAG_Comp == 1).
Retention_remove <- Retention_factor[!(Retention_factor$acct_id == "0" | Retention_factor$FLAG_Comp == 1), ]
# Question: Why is it necessary to filter out accounts with 'acct_id' equal to "0"?

# Step 2: Select customer grouping of interest
# Filters to only include customers with Flag_Personal_or_Business_STM == 1 and those that previously purchased
Retention_cust <- Retention_remove[Retention_remove$Flag_Personal_or_Business_STM == 1, ]
# Question: What does setting Flag_Personal_or_Business_STM == 1 help us achieve?

# Step 3: Create a subset with baseline variables
# Selects specific columns of interest for the model.
CRmodeldata <- Retention_cust[c(1, 2, 6, 9:13, 22, 25:26, 31, 33:36)]

# Step 4: Convert columns to numeric
# Ensures selected columns are numeric, important for any calculations.
CRmodeldata$Non_Resale_Num_Games <- as.numeric(as.character(CRmodeldata$Non_Resale_Num_Games))
CRmodeldata$Non_Resale_Scanned_Games <- as.numeric(as.character(CRmodeldata$Non_Resale_Scanned_Games))
CRmodeldata$Resale_Markup <- as.numeric(as.character(CRmodeldata$Resale_Markup))
# Question: Why might it be necessary to convert these variables to numeric?

# Step 5: Identify accounts with multiple records in one year
# Define years of consideration
years <- c(2018, 2019, 2021, 2022, 2023)
# Storing the number of repeats for each year in a list
repeat_acct_by_year <- list()
# Loop through each year to find repeated account IDs within that year (this is what was missing)
for (year in years) {
# Filter the data for the current year
data_filtered_year <- subset(CRmodeldata, season_year == year)
# Count the occurrences of account IDs for the current year
acct_id_counts_year <- table(data_filtered_year$acct_id)
# Find account IDs that repeat (appear more than once) in the current year
repeat_acct_id_year <- acct_id_counts_year[acct_id_counts_year > 1]
# Store the count of repeated account IDs for this year in the list
repeat_acct_by_year[[as.character(year)]] <- length(repeat_acct_id_year)
}
# Print the number of repeated account IDs for each year
print(repeat_acct_by_year)
#Question: what do we know about consumer behavior?

# Step 6: Create unique season-account identifiers
CRmodeldata <- CRmodeldata %>%
  mutate(season_acct_id = paste0(season_year, "_", acct_id))
#Account repeats in one year 
#Define the years of interest
years_of_interest <- c(2018, 2019, 2021, 2022, 2023)
# Filter the dataset for the specified years
data_filtered <- subset(CRmodeldata, season_year %in% years_of_interest)
# Count occurrences of each acc_id in the filtered data
acct_id_counts <- table(data_filtered$acct_id)
# Find repeated acc_id (those with counts > 1)
repeat_acct_id <- acct_id_counts[acct_id_counts > 1]
# Number of repeat acc_id
num_repeat_acct_id <- length(repeat_acct_id)
# Print the result
print(num_repeat_acct_id)

# Step 7: Summarize by most frequent characteristics and totals
# Here we calculate summaries like total seats and most frequent attributes for each account-season.

Model_dataID <- CRmodeldata %>%
  group_by(season_acct_id) %>%
  mutate(total_num_seats = sum(num_seats, na.rm = TRUE)) %>%
  mutate(Most_Frequent_Class = names(sort(table(Class), decreasing = TRUE)[1])) %>%
  mutate(Most_Frequent_Stadium_Classification = names(sort(table(Classification), decreasing = TRUE)[1])) %>%
  mutate(Most_Frequent_Stadium_Level = names(sort(table(Stadium_Level), decreasing = TRUE)[1])) %>%
  mutate(Most_Frequent_Stadium_Side = names(sort(table(Stadium_Side), decreasing = TRUE)[1])) %>%
  mutate(Most_Frequent_Field_View = names(sort(table(Field_View), decreasing = TRUE)[1])) %>%
  mutate(total_num_non_resale = sum(Non_Resale_Num_Games, na.rm = TRUE)) %>%
  mutate(total_num_non_resale_scanned = sum(Non_Resale_Scanned_Games, na.rm = TRUE)) %>%
  mutate(avg_resale_markup = if_else(all(is.na(Resale_Markup)), NA_real_, mean(Resale_Markup, na.rm = TRUE))) %>%
  ungroup()
# Question: Why do we calculate totals and most frequent values instead of using individual records?

# Step 8: Select relevant columns and create a final summary dataset
Model_datafinal <- Model_dataID %>% select(
  c('season_year', 'acct_id', 'season_acct_id', 'total_num_seats', 
    'Most_Frequent_Class', 'Most_Frequent_Stadium_Classification', 
    'Most_Frequent_Stadium_Level', 'Most_Frequent_Stadium_Side', 
    'Most_Frequent_Field_View', 'total_num_non_resale', 
    'total_num_non_resale_scanned', 'avg_resale_markup', 
    'Win_Pct', 'Playoff_Appearance', 'Playoff_Win', 'Hwin_Pct'))

Model_datafinalcollapsed <- Model_datafinal %>%
  group_by(season_acct_id) %>%
  summarise(across(everything(), first), .groups = 'drop') %>%
  mutate(across(starts_with("Most_Frequent"), as.factor))
# Question: Why do we execute the prior code block?

# Step 9: Create the dependent variable 'Retained' for retention analysis
Model_dataF <- Model_datafinalcollapsed
Model_dataF$Retained <- 0  # Initialize 'Retained' column as 0

mark_retained <- function(data, year1, year2) {
  acct_id_both_years <- intersect(
    data %>% filter(season_year == year1) %>% pull(acct_id),
    data %>% filter(season_year == year2) %>% pull(acct_id)
  )
  data %>% mutate(Retained = ifelse(season_year == year1 & acct_id %in% acct_id_both_years, 1, Retained))
}

Model_dataF <- mark_retained(Model_dataF, 2018, 2019)
Model_dataF <- mark_retained(Model_dataF, 2019, 2021)
Model_dataF <- mark_retained(Model_dataF, 2021, 2022)
Model_dataF <- mark_retained(Model_dataF, 2022, 2023)

# Step 10: Remove 2023 data and set 'Retained' as a factor
Model_dataC <- Model_dataF %>%
  filter(season_year != 2023) %>%
  mutate(Retained = as.factor(Retained))
# Question: Why do we remove 2023 data before modeling?

Model_dataC<-na.omit(Model_dataC)

#Step 11 Set the train and test data set 
set.seed(42)  
sample_index <- sample(1:nrow(Model_dataC), 0.7 * nrow(Model_dataC))
train_data <- Model_dataC[sample_index, ]
test_data <- Model_dataC[-sample_index, ]

# Load necessary libraries
if (!require("randomForest")) install.packages("randomForest", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
library(randomForest)
library(caret)
library(dplyr)
library(tidyverse)

#Step 12: Random forests construction 
Retention_rf <- randomForest(train_data$Retained ~ 
                               total_num_seats +
                               Most_Frequent_Class +
                               Most_Frequent_Stadium_Classification + 
                               Most_Frequent_Stadium_Level + 
                               Most_Frequent_Stadium_Side + 
                               Most_Frequent_Field_View +  
                               total_num_non_resale +
                               total_num_non_resale_scanned +
                               avg_resale_markup +
                               Win_Pct +
                               Playoff_Appearance +
                               Playoff_Win +
                               Hwin_Pct, 
                             data = train_data, 
                             ntree = 500, 
                             mtry = sqrt(ncol(train_data) - 1), 
                             importance = TRUE)

# Step 13: Make predictions and evaluate model accuracy
predictions <- predict(Retention_rf, test_data)
confusionMatrix(predictions, test_data$Retained)
# Question: What does the confusion matrix tell us about model accuracy?

# Step 14: Check and plot variable importance
importance_values <- importance(Retention_rf)
print(importance_values)
varImpPlot(Retention_rf)
# Question: Which variables appear most important for predicting retention?






