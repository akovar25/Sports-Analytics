--------------------------------------------------------------------------------
                            #Win Probability
---------------------------------------------------------------------------------
library(tidyverse)
library(nflfastR)
library('caTools')
library(randomForest)
library(pROC)

#Data Preparation and Cleansing
pbp2018 <- load_pbp(2018) #loading the data 
pbp2019 <- load_pbp(2019)

pbpfull = bind_rows(pbp2018,pbp2019)
rm(pbp2018, pbp2019)

pbpfull = pbpfull %>% mutate(winteam = ifelse(result > 0, home_team,
                                              ifelse(result == 0, "tie",
                                                     ifelse(result < 0, away_team, "NA"))))

pbpfull = pbpfull %>% mutate(poswins = ifelse(winteam == posteam,"PosWins","PosLoses")) %>%
  mutate(poswins = fct_relevel(poswins, "PosLoses"))

pbpfull = pbpfull %>% mutate(posspread = ifelse(posteam == home_team, spread_line, -1*spread_line))

cols = c("qtr","down","poswins")
pbpfull = pbpfull %>% mutate_at(cols,as_factor)

pbpfull = pbpfull %>% drop_na(yardline_100) %>%
  drop_na(game_seconds_remaining) %>%
  drop_na(down) %>%
  drop_na(posspread) %>%
  drop_na(score_differential)

pbpfull = pbpfull %>% filter(qtr != 5)

pbpfull = pbpfull %>% filter(result != 0)

#Model 1
mod1 = glm(poswins ~ yardline_100 + game_seconds_remaining + down +
             ydstogo + posspread + score_differential, data = pbpfull, family = "binomial")
options(scipen = 999)
summary(mod1)

#Model 1 Accuracy
sample<-sample.split(pbpfull$poswins, SplitRatio = .8)
train<-subset(pbpfull, sample==TRUE)
test<-subset(pbpfull, sample==FALSE)

modtrain <- glm(poswins ~ yardline_100 + game_seconds_remaining + down +
                  ydstogo + posspread + score_differential, data = train, family = binomial)
glm.probs <- predict(modtrain, newdata = test, type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred,test$poswins)
X <- table(glm.pred,test$poswins)
Accuracy_log1 = (X[1]+X[4])/(X[1]+X[4]+X[2]+X[3]) 
Precision_log1 = (X[1])/(X[1]+X[2]) #Positive Predicition
Recall_log1 = X[1]/(X[1]+X[2]) #Sensitivity / True Positive Rate
Specificity_log1 = (X[4])/(X[4]+X[3]) #True Negative Rate

F1_log1 = 2 * ((Precision_log1 * Recall_log1) / (Precision_log1 + Recall_log1))

#Predictions with Model 1 and Plotting game
predictions_log = predict(mod1, type = "response")

pbpfull = pbpfull %>% mutate(problog = predictions_log) %>%
  mutate(prob_home_log = ifelse(posteam == home_team, problog , 1-problog))
ggplot(pbpfull,aes(x=prob_home_log)) + geom_histogram()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == "2019_01_HOU_NO") %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "red")
gameid = "2019_01_HOU_NO"
homeid = pbpfull %>% filter(game_id == gameid) %>% select(home_team) %>% distinct()
awayid = pbpfull %>% filter(game_id == gameid) %>% select(away_team) %>% distinct()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == gameid) %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "red") +
  annotate("label", x = 3500, y = .95, label = paste0(homeid$home_team)) +
  annotate("label", x = 3500, y = .05, label = paste0(awayid$away_team))

gameid = "2019_01_HOU_NO"
homeid = pbpfull %>% filter(game_id == gameid) %>% select(home_team) %>% distinct()
awayid = pbpfull %>% filter(game_id == gameid) %>% select(away_team) %>% distinct()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == "2019_01_HOU_NO") %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log)) +
  geom_rect(aes(xmin=0, xmax=3600, ymin=0.5, ymax=1), fill = "#D3BC8D", alpha = 1) + geom_rect(aes(xmin=0, xmax=3600, ymin=0, ymax=0.5), fill = "#A71930", alpha = 1) + geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "red") +
  annotate("label", x = 3500, y = .95, label = paste0(homeid$home_team)) +
  annotate("label", x = 3500, y = .05, label = paste0(awayid$away_team))

#Attempting to Improve the Logistic Model by taking into account confounding situations

#Yards * Downs & Time left * Score Differential
mod_glm_interact <- glm(
  poswins ~ yardline_100 + game_seconds_remaining + down +
    ydstogo + posspread + score_differential +
    yardline_100:down + game_seconds_remaining:score_differential,
  data = pbpfull,
  family = "binomial"
)

summary(mod_glm_interact)

sample<-sample.split(pbpfull$poswins, SplitRatio = .8)
train<-subset(pbpfull, sample==TRUE)
test<-subset(pbpfull, sample==FALSE)

mod3train <- glm(
  poswins ~ yardline_100 + game_seconds_remaining + down +
    ydstogo + posspread + score_differential +
    yardline_100:down + game_seconds_remaining:score_differential,
  data = pbpfull,
  family = "binomial"
)
glm.probs <- predict(mod3train, newdata = test, type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred,test$poswins)
X <- table(glm.pred,test$poswins)
Accuracy_log2 = (X[1]+X[4])/(X[1]+X[4]+X[2]+X[3]) 
Precision_log2 = (X[1])/(X[1]+X[2]) #Positive Predicition
Recall_log2 = X[1]/(X[1]+X[2]) #Sensitivity / True Positive Rate
Specificity_log2 = (X[4])/(X[4]+X[3]) #True Negative Rate

F1_log2 = 2 * ((Precision_log2 * Recall_log2) / (Precision_log2 + Recall_log2))

#Model 2
mod2 <- randomForest(
  poswins ~ yardline_100 + game_seconds_remaining + down +
    ydstogo + posspread + score_differential,
  data = pbpfull,
  ntree = 1000,
  mtry = 2,
  nodesize = 10,
  importance = TRUE
)

# Train/test split (80/20)
train_idx <- sample(seq_len(nrow(pbpfull)), size = 0.8 * nrow(pbpfull))
train_data <- pbpfull[train_idx, ]
test_data  <- pbpfull[-train_idx, ]

mod2train <- randomForest(
  poswins ~ yardline_100 + game_seconds_remaining + down +
    ydstogo + posspread + score_differential,
  data = train_data,
  ntree = 1000,
  mtry = 2,
  nodesize = 10,
  importance = TRUE
)

# Random forest predictions (probabilities)
rf_probs <- predict(mod2train, newdata = test_data, type = "prob")[,2]
rf_pred  <- ifelse(rf_probs > 0.5, 1, 0)

table_rf  <- table(Predicted = rf_pred, Actual = test_data$poswins)
table_rf
Accuracy_RF = (table_rf[1]+table_rf[4])/(table_rf[1]+table_rf[4]+table_rf[2]+table_rf[3]) 
Precision_RF = (table_rf[1])/(table_rf[1]+table_rf[2]) #Positive Predicition
Recall_RF = table_rf[1]/(table_rf[1]+table_rf[2]) #Sensitivity / True Positive Rate
Specificity_RF = (table_rf[4])/(table_rf[4]+table_rf[3]) #True Negative Rate

F1_RF = 2 * ((Precision_RF * Recall_RF) / (Precision_RF + Recall_RF))

#Predictions with Model 2 and Plotting game
predictions_log = predict(mod2, type = "prob")[,2]

pbpfull = pbpfull %>% mutate(problog = predictions_log) %>%
  mutate(prob_home_log = ifelse(posteam == home_team, problog , 1-problog))
ggplot(pbpfull,aes(x=prob_home_log)) + geom_histogram()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == "2018_01_CHI_GB") %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "black")
gameid = "2018_01_CHI_GB"
homeid = pbpfull %>% filter(game_id == gameid) %>% select(home_team) %>% distinct()
awayid = pbpfull %>% filter(game_id == gameid) %>% select(away_team) %>% distinct()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == gameid) %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "black") +
  annotate("label", x = 3500, y = .95, label = paste0(homeid$home_team)) +
  annotate("label", x = 3500, y = .05, label = paste0(awayid$away_team))

gameid = "2018_01_CHI_GB"
homeid = pbpfull %>% filter(game_id == gameid) %>% select(home_team) %>% distinct()
awayid = pbpfull %>% filter(game_id == gameid) %>% select(away_team) %>% distinct()

vertical.lines = c(900, 1800, 2700, 3600)
pbpfull %>% filter(game_id == "2018_01_CHI_GB") %>%
  ggplot(aes(x=game_seconds_remaining,y=prob_home_log)) +
  geom_rect(aes(xmin=0, xmax=3600, ymin=0.5, ymax=1), fill = "#FFB733", alpha = 1) + geom_rect(aes(xmin=0, xmax=3600, ymin=0, ymax=0.5), fill = "#c83803", alpha = 1) + geom_line(size = 1) +
  theme_bw() +
  scale_x_reverse(breaks=seq(0,3600,by=450)) +
  ylim(0,1) +
  xlab("Game Time Remaining (seconds)") +
  ylab("Home Team Win Probability") +
  geom_vline(xintercept = vertical.lines, color = "black") +
  annotate("label", x = 3500, y = .95, label = paste0(homeid$home_team)) +
  annotate("label", x = 3500, y = .05, label = paste0(awayid$away_team))

--------------------------------------------------------------------------------
                    #Customer Retention
--------------------------------------------------------------------------------
library(dplyr) #data manipulation, including grouping, summarizing, and filtering data
library(ggplot2) #data vis
library(scales) #enhancing the appearance of plots 
library(randomForest)
library(caret)
library(dplyr)
library(tidyverse)

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

#Identifying the important population to target
ticket_data <- Retention_unique %>%
  filter(FLAG_Comp == 0 & !is.na(Class) & Class != "") %>%
  group_by(season_year, Class) %>%
  summarise(num_tickets = sum(num_seats, na.rm = TRUE), .groups = "drop")

ggplot(ticket_data, aes(x = as.factor(season_year), y = num_tickets, fill = Class)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = num_tickets),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3.5) + 
  labs(title = "Number of Tickets Sold per Year by Seat Class",
       x = "Year", y = "Number of Tickets") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

ticket_data2 <- Retention_unique %>%
  filter(FLAG_Comp == 0 & !is.na(Class) & Class != "") %>%
  group_by(season_year, Class) %>%
  summarise(num_tickets = sum(num_seats, na.rm = TRUE), .groups = "drop")

ggplot(ticket_data, aes(x = season_year, y = num_tickets, color = Class, group = Class)) +
  geom_line(size = 1.2) +                          
  geom_point(size = 3) +                           
  geom_text(aes(label = num_tickets),              
            vjust = -0.5, size = 3.5) +
  labs(title = "Change in Tickets Sold by Seat Class Over Years",
       x = "Year", y = "Number of Tickets") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

library(dplyr)
library(ggplot2)
library(scales)

# Aggregate ticket counts by Stadium Level and Class
level_class_data <- Retention_unique %>%
  filter(FLAG_Comp == 0 & !is.na(Class) & Class != "" &
           !is.na(Stadium_Level) & Stadium_Level %in% c("Upper", "Field")) %>%
  group_by(Stadium_Level, Class) %>%
  summarise(num_tickets = sum(num_seats, na.rm = TRUE), .groups = "drop") %>%
  group_by(Stadium_Level) %>%
  mutate(pct_tickets = num_tickets / sum(num_tickets)) %>%
  ungroup()

# Stacked bar chart showing percentages
ggplot(level_class_data, aes(x = Stadium_Level, y = pct_tickets, fill = Class)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(pct_tickets, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  labs(title = "Percentage Composition of Ticket Classes in Upper vs Field Levels",
       x = "Stadium Level", y = "Percentage of Tickets") +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#TARGET GENERAL ADMISSION#

Retention_factor <- Retention_unique
Retention_factor[] <- lapply(Retention_unique, function(x) if (is.character(x)) as.factor(x) else x)

str(Retention_factor)

#Summarize the variables with factor levels 
factor_summary <- sapply(Retention_factor[sapply(Retention_factor, is.factor)], summary)

# Display the summary
factor_summary

summary(Retention_factor) 
#There are NAs present - why should we not remove them now? 
str(Retention_factor)

Retention_cust <- Retention_remove[Retention_remove$Flag_Personal_or_Business_STM == 1, ]

CRmodeldata <- Retention_cust[c(1, 2, 6, 9:13, 22, 25:26, 31, 33:36)]

CRmodeldata$Non_Resale_Num_Games <- as.numeric(as.character(CRmodeldata$Non_Resale_Num_Games))
CRmodeldata$Non_Resale_Scanned_Games <- as.numeric(as.character(CRmodeldata$Non_Resale_Scanned_Games))
CRmodeldata$Resale_Markup <- as.numeric(as.character(CRmodeldata$Resale_Markup))

years <- c(2018, 2019, 2021, 2022, 2023)

repeat_acct_by_year <- list()
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

CRmodeldata <- CRmodeldata %>%
  mutate(season_acct_id = paste0(season_year, "_", acct_id))

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


Model_dataF <- Model_datafinalcollapsed
Model_dataF$Retained <- 0  


Model_dataF <- mark_retained(Model_dataF, 2018, 2019)
Model_dataF <- mark_retained(Model_dataF, 2019, 2021)
Model_dataF <- mark_retained(Model_dataF, 2021, 2022)
Model_dataF <- mark_retained(Model_dataF, 2022, 2023)

# Filter for General Admission seat class
Model_dataGA <- Model_dataF %>%
  filter(Most_Frequent_Class == "GA") %>%   # adjust string if dataset uses different label
  filter(season_year != 2023) %>%
  mutate(Retained = as.factor(Retained))

# Remove NAs
Model_dataGA <- na.omit(Model_dataGA)

# Train/test split
set.seed(42)
sample_index <- sample(1:nrow(Model_dataGA), 0.7 * nrow(Model_dataGA))
train_data <- Model_dataGA[sample_index, ]
test_data <- Model_dataGA[-sample_index, ]

# Random Forest model
Retention_rf_GA <- randomForest(train_data$Retained ~ 
                                  total_num_seats +
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

# Predictions + evaluation
predictions_GA <- predict(Retention_rf_GA, test_data)
confusionMatrix(predictions_GA, test_data$Retained)

importance_values <- importance(Retention_rf_GA)
print(importance_values)
varImpPlot(Retention_rf_GA)
