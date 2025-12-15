# -------------------------------
# Step 0: Libraries
# -------------------------------
library(tidyverse)
library(randomForest)
library(caret)
library(shiny)
library(shinydashboard)

# -------------------------------
# Step 1: Load data
# -------------------------------
supp   <- read_csv("supplementary_data.csv")
input  <- read_csv("input.csv")
output <- read_csv("output.csv")

# -------------------------------
# Step 2: Player roles and trajectories
# -------------------------------
player_roles <- input %>%
  select(game_id, play_id, nfl_id, player_side, player_role, player_name, player_position) %>%
  distinct()

out_tagged <- output %>%
  left_join(player_roles, by = c("game_id", "play_id", "nfl_id"))

receiver_traj <- out_tagged %>%
  filter(player_side == "Offense", player_role %in% c("Receiver", "Targeted Receiver")) %>%
  select(game_id, play_id, frame_id, nfl_id, x, y) %>%
  rename(rec_nfl_id = nfl_id, rec_x = x, rec_y = y)

defenders_traj <- out_tagged %>%
  filter(player_side == "Defense") %>%
  select(game_id, play_id, frame_id, nfl_id, x, y) %>%
  rename(def_nfl_id = nfl_id, def_x = x, def_y = y)

# -------------------------------
# Step 3: Receiver–Defender primary matchups
# -------------------------------
pairwise <- receiver_traj %>%
  left_join(defenders_traj, by = c("game_id", "play_id", "frame_id")) %>%
  mutate(dist_to_receiver = sqrt((def_x - rec_x)^2 + (def_y - rec_y)^2))

receiver_defender_pairs <- pairwise %>%
  group_by(game_id, play_id, frame_id, rec_nfl_id) %>%
  slice_min(order_by = dist_to_receiver, with_ties = FALSE) %>%
  ungroup()

# -------------------------------
# Step 4: Attach team and coverage info
# -------------------------------
game_teams <- supp %>%
  select(game_id, play_id, possession_team, home_team_abbr, visitor_team_abbr,
         team_coverage_man_zone, team_coverage_type) %>%
  distinct()

player_info <- player_roles %>%
  left_join(game_teams, by = c("game_id", "play_id")) %>%
  mutate(team = case_when(
    player_side == "Offense" & possession_team == home_team_abbr ~ home_team_abbr,
    player_side == "Offense" & possession_team == visitor_team_abbr ~ visitor_team_abbr,
    player_side == "Defense" & possession_team == home_team_abbr ~ visitor_team_abbr,
    player_side == "Defense" & possession_team == visitor_team_abbr ~ home_team_abbr,
    TRUE ~ NA_character_
  )) %>%
  select(game_id, play_id, nfl_id, player_name, player_position, team) %>%
  distinct()

receiver_defender_pairs <- receiver_defender_pairs %>%
  left_join(player_info, by = c("game_id", "play_id", "rec_nfl_id" = "nfl_id")) %>%
  rename(receiver_name = player_name, receiver_position = player_position, receiver_team = team) %>%
  left_join(player_info, by = c("game_id", "play_id", "def_nfl_id" = "nfl_id")) %>%
  rename(defender_name = player_name, defender_position = player_position, defender_team = team) %>%
  left_join(game_teams, by = c("game_id", "play_id"))

# -------------------------------
# Step 5: Coverage success thresholds (defaults)
# -------------------------------
avg_man_separation <- receiver_defender_pairs %>%
  filter(team_coverage_man_zone == "MAN_COVERAGE") %>%
  summarise(avg_sep = mean(dist_to_receiver, na.rm = TRUE)) %>%
  pull(avg_sep)

avg_man_separation2 <- receiver_defender_pairs %>%
  filter(team_coverage_man_zone == "MAN_COVERAGE")

summary(avg_man_separation2)

avg_zone_separation <- receiver_defender_pairs %>%
  filter(team_coverage_man_zone == "ZONE_COVERAGE") %>%
  summarise(avg_sep = mean(dist_to_receiver, na.rm = TRUE)) %>%
  pull(avg_sep)

avg_zone_separation2 <- receiver_defender_pairs %>%
  filter(team_coverage_man_zone == "ZONE_COVERAGE")

summary(avg_zone_separation2)

# -------------------------------
# Step 6: Summarise coverage success at matchup level
# -------------------------------
coverage_summary <- receiver_defender_pairs %>%
  mutate(good_coverage = case_when(
    team_coverage_man_zone == "MAN_COVERAGE" & dist_to_receiver <= avg_man_separation ~ 1,
    team_coverage_man_zone == "ZONE_COVERAGE" & dist_to_receiver <= avg_zone_separation ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(game_id, play_id, rec_nfl_id, def_nfl_id,
           receiver_name, defender_name, receiver_team, defender_team,
           team_coverage_man_zone, team_coverage_type) %>%
  summarise(
    coverage_success_pct = mean(good_coverage, na.rm = TRUE),
    frames_per_play = n(),
    .groups = "drop"
  )

# -------------------------------
# Step 7: Add pass info, yardline, clock, and score differential
# -------------------------------
supp <- supp %>%
  mutate(
    quarter_seconds = as.numeric(substr(game_clock, 1, 2)) * 60 +
      as.numeric(substr(game_clock, 4, 5)),
    time_remaining_game = (4 - quarter) * 900 + quarter_seconds,
    defensive_team = case_when(
      possession_team == home_team_abbr ~ visitor_team_abbr,
      possession_team == visitor_team_abbr ~ home_team_abbr,
      TRUE ~ NA_character_
    ),
    defensive_score_differential = case_when(
      defensive_team == home_team_abbr ~ pre_snap_home_score - pre_snap_visitor_score,
      defensive_team == visitor_team_abbr ~ pre_snap_visitor_score - pre_snap_home_score,
      TRUE ~ NA_real_
    )
  )

targeted_players_plays <- input %>%
  filter(player_role == "Targeted Receiver") %>%
  select(game_id, play_id, nfl_id, player_name, player_position) %>%
  distinct()

possession_team_per_play <- supp %>%
  select(game_id, play_id, possession_team)

targeted_players_plays <- targeted_players_plays %>%
  left_join(possession_team_per_play, by = c("game_id", "play_id"))

total_targets <- targeted_players_plays %>%
  group_by(nfl_id, player_name, player_position, possession_team) %>%
  summarise(
    total_targets = n()
  ) %>%
  arrange(desc(total_targets))

receiver_roles <- total_targets %>%
  group_by(possession_team) %>%
  arrange(desc(total_targets)) %>%
  mutate(
    wr_rank = ifelse(player_position == "WR", row_number(), NA),
    receiver_role = case_when(
      player_position == "TE" ~ "Tight End",
      player_position == "RB" ~ "Running Back",
      player_position == "WR" & wr_rank == 1 ~ "Receiver 1",
      player_position == "WR" & wr_rank == 2 ~ "Receiver 2",
      player_position == "WR" & wr_rank >= 3 ~ "Receiver 3",
      TRUE ~ "Other"
    )
  ) %>%
  ungroup() %>%
  select(nfl_id, player_name, possession_team, receiver_role)

coverage_summary <- coverage_summary %>%
  left_join(receiver_roles, by = c("rec_nfl_id" = "nfl_id", "receiver_team" = "possession_team"))

pass_info <- supp %>%
  select(game_id, play_id, pass_length, down, yards_to_go, pass_result,
         time_remaining_game, defensive_score_differential, offense_formation,
         receiver_alignment, route_of_targeted_receiver) %>%
  distinct()

coverage_summary <- coverage_summary %>%
  left_join(pass_info, by = c("game_id", "play_id"))

yardline <- input %>%
  select(game_id, play_id, absolute_yardline_number) %>%
  distinct()

coverage_summary <- coverage_summary %>%
  left_join(yardline, by = c("game_id", "play_id"))

# -------------------------------
# Step 8: Bin pass length
# -------------------------------
coverage_summary <- coverage_summary %>%
  mutate(pass_length_bin = cut(pass_length,
                               breaks = c(-Inf, 3, 10, Inf),
                               labels = c("<=3", "4-10", "10+"),
                               right = TRUE))

coverage_summary_clean <- coverage_summary %>%
  filter(!is.na(team_coverage_type),
         !is.na(def_nfl_id),
         !is.na(pass_length_bin))

coverage_summary_clean <- coverage_summary_clean %>%
  mutate(
    team_coverage_type        = factor(team_coverage_type),
    receiver_role             = factor(receiver_role, 
                                       levels = c("Receiver 1","Receiver 2","Receiver 3","Tight End","Running Back")),
    receiver_team             = factor(receiver_team),
    defender_team             = factor(defender_team),
    offense_formation         = factor(offense_formation),
    receiver_alignment        = factor(receiver_alignment),
    route_of_targeted_receiver= factor(route_of_targeted_receiver)
  )

# -------------------------------
# Step 9–12: Initial model, evaluation
# -------------------------------
train_idx <- sample(seq_len(nrow(coverage_summary_clean)), size = 0.7 * nrow(coverage_summary_clean))
train_data <- coverage_summary_clean[train_idx, ]
test_data  <- coverage_summary_clean[-train_idx, ]

rf_model <- randomForest(
  pass_length_bin ~ coverage_success_pct + down + yards_to_go +
    team_coverage_type + receiver_role +
    receiver_team + defender_team + absolute_yardline_number +
    time_remaining_game + defensive_score_differential + offense_formation +
    receiver_alignment + route_of_targeted_receiver,
  data = train_data,
  ntree = 250,
  importance = TRUE,
  na.action = na.omit
)

pred_classes <- predict(rf_model, newdata = test_data, type = "class")
confusionMatrix(pred_classes, test_data$pass_length_bin)
varImpPlot(rf_model)
# -------------------------------
# Step 13: Shiny Dashboard with dynamic thresholds and retraining
# -------------------------------
receiver_roles_ui <- c("Receiver 1","Receiver 2","Receiver 3","Tight End","Running Back")

team_abbrs <- sort(unique(c(
  "ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB","HOU","IND","JAX","KC",
  "LV","LAC","LAR","MIA","MIN","NE","NO","NYG","NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS"
)))

offense_formations <- sort(unique(train_data$offense_formation))
receiver_alignments <- sort(unique(train_data$receiver_alignment))
routes <- sort(unique(train_data$route_of_targeted_receiver))

ui <- dashboardPage(
  dashboardHeader(title = "Pass Length Probability Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inputs", tabName = "inputs", icon = icon("sliders")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Inputs tab
      tabItem(tabName = "inputs",
              fluidRow(
                # Thresholds box moved here
                box(title = "Adjust Coverage Thresholds", status = "danger", solidHeader = TRUE, width = 12,
                    tags$p(strong("If you change the average separation thresholds, please be aware there will be a wait for a couple minutes while the model recalibrates to your new separation markers.")),
                    numericInput("man_sep", "Average Man Coverage Separation (yards):", value = avg_man_separation, min = 0, step = 0.1),
                    numericInput("zone_sep", "Average Zone Coverage Separation (yards):", value = avg_zone_separation, min = 0, step = 0.1),
                    actionButton("apply_sep", "Apply Custom Values"),
                    actionButton("reset_sep", "Use Default Separation"),
                    verbatimTextOutput("current_thresholds")
                )
              ),
              fluidRow(
                box(title = "Coverage & Situation", status = "primary", solidHeader = TRUE, width = 6,
                    sliderInput("coverage_success_pct", "Coverage Success (% of the ball in air time the defender is within separation threshold):", min = 0, max = 1, value = 0.75, step = 0.01),
                    selectInput("down", "Down:", choices = 1:4, selected = 1),
                    numericInput("yards_to_go", "Yards to Go:", value = 10, min = 1),
                    sliderInput("absolute_yardline_number", "Distance from End Zone (yards):", min = 0, max = 100, value = 50),
                    sliderInput("time_remaining_game", "Time Remaining (seconds):", min = 0, max = 3600, value = 1800),
                    numericInput("defensive_score_differential", "Defensive Score Differential:", value = 0, step = 1)
                ),
                box(title = "Teams & Players", status = "warning", solidHeader = TRUE, width = 6,
                    selectInput("team_coverage_type", "Coverage Type:", choices = levels(train_data$team_coverage_type)),
                    selectInput("receiver_role", "Receiver Role (Rank is descending in target share):", choices = receiver_roles_ui, selected = "Receiver 1"),
                    selectInput("receiver_team", "Receiver Team:", choices = levels(train_data$receiver_team), selected = "DET"),
                    selectInput("defender_team", "Defender Team:", choices = levels(train_data$defender_team), selected = "ARI"),
                    selectInput("offense_formation", "Offense Formation:", choices = levels(train_data$offense_formation)),
                    selectInput("receiver_alignment", "Receiver Alignment:", choices = levels(train_data$receiver_alignment)),
                    selectInput("route_of_targeted_receiver", "Route of Targeted Receiver:", choices = levels(train_data$route_of_targeted_receiver))
                )
              ),
              # Sneak preview outputs at bottom
              fluidRow(
                valueBoxOutput("mostLikelyBox", width = 12),
                box(title = "Model retraining status", status = "primary", solidHeader = TRUE, width = 12,
                    htmlOutput("retrain_status_inputs"))
              )
      ),
      
      # Results tab
      tabItem(tabName = "results",
              fluidRow(
                box(title = "Predicted Probabilities", status = "success", solidHeader = TRUE, width = 6,
                    tableOutput("prob_table")),
                box(title = "Probability Plot", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("prob_plot"))
              ),
              fluidRow(
                box(title = "Model retraining status", status = "primary", solidHeader = TRUE, width = 12,
                    htmlOutput("retrain_status"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  master_levels <- list(
    team_coverage_type      = levels(coverage_summary_clean$team_coverage_type),
    receiver_role           = levels(coverage_summary_clean$receiver_role),
    receiver_team           = levels(coverage_summary_clean$receiver_team),
    defender_team           = levels(coverage_summary_clean$defender_team),
    offense_formation       = levels(coverage_summary_clean$offense_formation),
    receiver_alignment      = levels(coverage_summary_clean$receiver_alignment),
    route_of_targeted_receiver = levels(coverage_summary_clean$route_of_targeted_receiver)
  )
  
  # Reactive thresholds
  thresholds <- reactiveValues(
    man = avg_man_separation,
    zone = avg_zone_separation
  )
  
  # Apply sidebar values
  observeEvent(input$apply_sep, {
    thresholds$man <- input$man_sep
    thresholds$zone <- input$zone_sep
  })
  
  # Reset to defaults
  observeEvent(input$reset_sep, {
    thresholds$man <- avg_man_separation
    thresholds$zone <- avg_zone_separation
    updateNumericInput(session, "man_sep", value = avg_man_separation)
    updateNumericInput(session, "zone_sep", value = avg_zone_separation)
  })
  
  # Display current thresholds
  output$current_thresholds <- renderText({
    paste0("Man: ", round(thresholds$man, 2), " | Zone: ", round(thresholds$zone, 2))
  })
  
  # Dynamic coverage summary with thresholds + receiver roles join
  coverage_summary_dynamic <- reactive({
    df <- receiver_defender_pairs %>%
      mutate(good_coverage = case_when(
        team_coverage_man_zone == "MAN_COVERAGE" & dist_to_receiver <= thresholds$man ~ 1,
        team_coverage_man_zone == "ZONE_COVERAGE" & dist_to_receiver <= thresholds$zone ~ 1,
        TRUE ~ 0
      )) %>%
      group_by(game_id, play_id, rec_nfl_id, def_nfl_id,
               receiver_name, defender_name, receiver_team, defender_team,
               team_coverage_man_zone, team_coverage_type) %>%
      summarise(
        coverage_success_pct = mean(good_coverage, na.rm = TRUE),
        frames_per_play = n(),
        .groups = "drop"
      ) %>%
      left_join(pass_info, by = c("game_id", "play_id")) %>%
      left_join(yardline, by = c("game_id", "play_id")) %>%
      mutate(pass_length_bin = cut(pass_length,
                                   breaks = c(-Inf, 3, 10, Inf),
                                   labels = c("<=3", "4-10", "10+"),
                                   right = TRUE)) %>%
      filter(!is.na(team_coverage_type),
             !is.na(def_nfl_id),
             !is.na(pass_length_bin))
    
    # Rejoin receiver roles by nfl_id + team
    df <- df %>%
      left_join(receiver_roles, 
                by = c("rec_nfl_id" = "nfl_id", "receiver_team" = "possession_team")) %>%
      mutate(
        team_coverage_type       = factor(team_coverage_type, levels = master_levels$team_coverage_type),
        receiver_role            = factor(receiver_role, levels = master_levels$receiver_role),
        receiver_team            = factor(receiver_team, levels = master_levels$receiver_team),
        defender_team            = factor(defender_team, levels = master_levels$defender_team),
        offense_formation        = factor(offense_formation, levels = master_levels$offense_formation),
        receiver_alignment       = factor(receiver_alignment, levels = master_levels$receiver_alignment),
        route_of_targeted_receiver = factor(route_of_targeted_receiver, levels = master_levels$route_of_targeted_receiver)
      )
    
    df
  })
  
  # Retraining trigger
  retrain_trigger <- reactiveVal(Sys.time())
  
  observeEvent(list(thresholds$man, thresholds$zone), {
    if (thresholds$man == avg_man_separation && thresholds$zone == avg_zone_separation) {
      output$retrain_status <- renderUI({
        HTML("<span style='color:#2980b9;font-weight:bold;'>Using default model (no retrain).</span>")
      })
      output$retrain_status_inputs <- renderUI({
        HTML("<span style='color:#2980b9;font-weight:bold;'>Using default model (no retrain).</span>")
      })
    } else {
      output$retrain_status <- renderUI({
        HTML(paste0("<span style='color:#d35400;font-weight:bold;'>Retraining model...</span> ",
                    "(Man: ", round(thresholds$man, 2), ", Zone: ", round(thresholds$zone, 2), ")"))
      })
      output$retrain_status_inputs <- renderUI({
        HTML(paste0("<span style='color:#d35400;font-weight:bold;'>Retraining model...</span> ",
                    "(Man: ", round(thresholds$man, 2), ", Zone: ", round(thresholds$zone, 2), ")"))
      })
      retrain_trigger(Sys.time())
    }
  })
  
  # Retrain RF model with updated formula
  rf_model_dynamic <- eventReactive(retrain_trigger(), {
    if (thresholds$man == avg_man_separation && thresholds$zone == avg_zone_separation) {
      return(rf_model) # default model
    }
    df <- coverage_summary_dynamic()
    if (nrow(df) < 10) return(rf_model)
    set.seed(123)
    model <- randomForest(
      pass_length_bin ~ coverage_success_pct + down + yards_to_go +
        team_coverage_type + receiver_role +
        receiver_team + defender_team + absolute_yardline_number +
        time_remaining_game + defensive_score_differential + offense_formation +
        receiver_alignment + route_of_targeted_receiver,
      data = df,
      ntree = 250,
      mtry = 3,
      importance = TRUE,
      na.action = na.omit
    )
    output$retrain_status <- renderUI({
      HTML(paste0("<span style='color:#27ae60;font-weight:bold;'>Retraining complete.</span> ",
                  "Updated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  " | Man: ", round(thresholds$man, 2),
                  ", Zone: ", round(thresholds$zone, 2)))
    })
    output$retrain_status_inputs <- renderUI({
      HTML(paste0("<span style='color:#27ae60;font-weight:bold;'>Retraining complete.</span> ",
                  "Updated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  " | Man: ", round(thresholds$man, 2),
                  ", Zone: ", round(thresholds$zone, 2)))
    })
    model
  }, ignoreInit = FALSE)
  
  # Reactive newdata frame from UI
  newdata <- reactive({
    data.frame(
      coverage_success_pct = input$coverage_success_pct,
      down = as.integer(input$down),
      yards_to_go = input$yards_to_go,
      team_coverage_type = factor(input$team_coverage_type, levels = master_levels$team_coverage_type),
      receiver_role = factor(input$receiver_role, levels = master_levels$receiver_role),
      receiver_team = factor(input$receiver_team, levels = master_levels$receiver_team),
      defender_team = factor(input$defender_team, levels = master_levels$defender_team),
      absolute_yardline_number = input$absolute_yardline_number,
      time_remaining_game = input$time_remaining_game,
      defensive_score_differential = input$defensive_score_differential,
      offense_formation = factor(input$offense_formation, levels = master_levels$offense_formation),
      receiver_alignment = factor(input$receiver_alignment, levels = master_levels$receiver_alignment),
      route_of_targeted_receiver = factor(input$route_of_targeted_receiver, levels = master_levels$route_of_targeted_receiver)
    )
  })
  
  # Prediction adjusted by yardline rules
  adjusted_probs <- reactive({
    model <- rf_model_dynamic()
    df <- as.data.frame(predict(model, newdata(), type = "prob"))
    probs <- as.matrix(df)
    yardline <- input$absolute_yardline_number
    
    if (yardline <= 3) {
      probs[,] <- 0
      probs[, "<=3"] <- 1
    } else if (yardline <= 10) {
      probs[, "10+"] <- 0
      total <- sum(probs)
      if (total > 0) probs <- probs / total
    }
    probs
  })
  
  # Probability table
  output$prob_table <- renderTable({
    probs <- adjusted_probs()
    pct <- round(probs * 100, 1)
    apply(pct, 2, function(x) paste0(x, "%"))
  }, rownames = TRUE)
  
  # Probability plot
  output$prob_plot <- renderPlot({
    probs <- adjusted_probs()
    pct <- round(probs * 100, 1)
    bp <- barplot(
      probs * 100,
      beside = TRUE,
      col = c("skyblue","orange","darkgreen"),
      names.arg = colnames(probs),
      main = "Predicted Pass Length Probabilities",
      ylab = "Probability (%)",
      ylim = c(0, 100)
    )
    text(x = bp, y = probs * 100, labels = paste0(pct, "%"), pos = 3, cex = 0.8)
  })
  
  # Sync yards_to_go with yardline when inside 10 yards
  observeEvent(input$absolute_yardline_number, {
    if (input$absolute_yardline_number <= 10) {
      updateNumericInput(session, "yards_to_go", value = input$absolute_yardline_number)
    }
  })
  
  # Most likely bucket
  output$mostLikelyBox <- renderValueBox({
    m <- adjusted_probs()
    probs <- m[1, ]
    max_bucket <- names(which.max(probs))
    max_prob <- round(max(probs) * 100, 1)
    valueBox(
      value = paste0("Most Likely Pass Length: ", max_bucket, " (", max_prob, "%)"),
      subtitle = "Pass Length Prediction",
      icon = icon("football-ball"),
      color = "purple"
    )
  })
}

shinyApp(ui = ui, server = server)
