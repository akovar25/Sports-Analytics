library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)
library(formattable)

ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("Packers Availability"),
                
                sidebarLayout(
                  position = "left",
                  sidebarPanel(
                    selectInput(
                      "week",
                      h4("Week of Interest"),
                      choices = list("Week 1: Chicago Bears" = 1, "Week 2: Atlanta Falcons" = 2, "Week 3: New Orleans Saints" = 3, "Week 4: Detroit Lions" = 4, 
                                     "Week 5: Las Vegas Raiders" = 5, "Week 6: Bye" = 6, "Week 7: Denver Broncos" = 7, "Week 8: Minnesota Vikings" = 8,
                                     "Week 9: Los Angeles Rams" = 9, "Week 10: Pittsburgh Steelers" = 10, "week 11: Los Angeles Chargers" = 11, 
                                     "Week 12: Detroit Lions" = 12, "Week 13: Kansas City Chiefs" = 13, "Week 14: New York Giants" = 14, "Week 15: Tampa Bay Buccaneers" = 15,
                                     "Week 16: Carolina Panters" = 16, "Week 17: Minnesota Vikings" = 17, "Week 18: Chicago Bears" = 18, "Wild Card Playoff: Dallas Cowboys" = 19, "Divisional Round Playoff: San Francisco 49ers" = 20),
                      selected = 1)
                  ),
                  
                  mainPanel(
                    h2("Team Availabilities"),
                    conditionalPanel(
                      condition = "input.week != 6",
                      fluidRow(
                        column(
                          width = 6,
                          wellPanel(
                            h4("Packers Availability"),
                            formattableOutput("Packers_stat")
                          )
                        ),
                        column(
                          width = 6,
                          wellPanel(
                            h4("Opponent Availability"),
                            formattableOutput("Opp_stat")
                          )
                        ),
                        column(
                          width = 6,
                          wellPanel(
                            h4("Packers Players of Interest"),
                            formattableOutput("Packers_pos_int")
                          )
                        ),
                        column(
                          width = 6,
                          wellPanel(
                            h4("Opponent Players of Interest"),
                            formattableOutput("Opp_pos_int")
                          )
                        ),
                        column(
                          width = 12,
                          wellPanel(
                            fluidRow(
                              column(
                                width = 6,
                                wellPanel(
                                  h4("Packers Injured Players"),
                                  formattableOutput("Pack_inj_players")
                                )
                              ),
                              column(
                                width = 6,
                                wellPanel(
                                  h4("Opponent Injured Players"),
                                  formattableOutput("Opp_inj_players")
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.week == 6",
                      h3("No Opponent for the Bye Week!")
                    )
                  )
                )
)

server <- function(input, output, session) {
  
  factors <- read_csv("Factors.csv")
  packavail <- read_csv("Packers and Opps Weekly Report.csv")
  teamintin <- reactive({
    if (input$week %in% c(1, 18)) {
      return("CHI")
    } else if (input$week %in% c(2)) {
      return("ATL")
    } else if (input$week %in% c(3)) {
      return("NO")
    } else if (input$week %in% c(4, 12)) {
      return("DET")
    } else if (input$week %in% c(5)) {
      return("LV")
    } else if (input$week %in% c(6)) {
      return("BYE")
    } else if (input$week %in% c(7)) {
      return("DEN")
    } else if (input$week %in% c(8, 17)) {
      return("MIN")
    } else if (input$week %in% c(9)) {
      return("LA")
    } else if (input$week %in% c(10)) {
      return("PIT")
    } else if (input$week %in% c(11)) {
      return("LAC")
    } else if (input$week %in% c(13)) {
      return("KC")
    } else if (input$week %in% c(14)) {
      return("NYG")
    } else if (input$week %in% c(15)) {
      return("TB")
    } else if (input$week %in% c(19)) {
      return("DAL")
    } else if (input$week %in% c(20)) {
      return("SF")
    } else {
      return("CAR")
    }
  })
  
  OppColor <- reactive({
    if (input$week %in% c(1, 18)) {
      return("#C83803")
    } else if (input$week %in% c(2)) {
      return("#A71930")
    } else if (input$week %in% c(3)) {
      return("#D3BC8D")
    } else if (input$week %in% c(4, 12)) {
      return("#006db0")
    } else if (input$week %in% c(5)) {
      return("#A5ACAF")
    } else if (input$week %in% c(6)) {
      return("#FFB612")
    } else if (input$week %in% c(7)) {
      return("#FB4F14")
    } else if (input$week %in% c(8, 17)) {
      return("#4F2683")
    } else if (input$week %in% c(9)) {
      return("#003594")
    } else if (input$week %in% c(10)) {
      return("#FFB612")
    } else if (input$week %in% c(11)) {
      return("#0080C6")
    } else if (input$week %in% c(13)) {
      return("#E31837")
    } else if (input$week %in% c(14)) {
      return("#0B2265")
    } else if (input$week %in% c(15)) {
      return("#D50A0A")
    } else if (input$week %in% c(19)) {
      return("#869397")
    } else if (input$week %in% c(20)) {
      return("#B3995D")
    } else {
      return("#0085CA")
    }
  })
  
  output$Packers_stat <- renderFormattable({
    PackersPre <- packavail %>%
      filter(team == "GB") %>%
      filter(week == "pre-season") %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench") %>%
      group_by(position) %>%
      summarize(sum(grade))
    
    Packers1 <- packavail %>%
      filter(team == "GB") %>%
      filter(week == input$week) %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench")
    
    Packers1 <- merge(Packers1, factors, by = "position")
    
    Packers1 <- subset(Packers1, grade != 0)
    
    Packers1 <- Packers1 %>%
      mutate(weekly_grade = if_else(status == "Questionable", grade * factor, grade)) %>%
      select(position, player, team, weekly_grade) %>%
      group_by(position) %>%
      summarize(mean(weekly_grade), sum(weekly_grade))
    
    PackersDif <- merge(Packers1, PackersPre, by = "position")
    
    PackersDif <- PackersDif %>%
      mutate(Availability_Percentage = (`sum(weekly_grade)`/`sum(grade)`)) %>%
      mutate(`Availability Percentage` = percent(Availability_Percentage)) %>%
      mutate(`Average Grade Per Position` = round(`mean(weekly_grade)`, 2)) %>%
      rename("Position" = position) %>%
      select(Position, `Availability Percentage`,`Average Grade Per Position`)
    
    formattable(PackersDif, list(
      `Availability Percentage` = color_bar("#203731")
    ))
    
  })
  
  output$Opp_stat <- renderFormattable({
    
    OppPre <- packavail %>%
      filter(team == teamintin()) %>%
      filter(week == "pre-season") %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench") %>%
      group_by(position) %>%
      summarize(sum(grade))
    
    
    Opp1 <- packavail %>%
      filter(team == teamintin()) %>%
      filter(week == input$week) %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench")
    
    Opp1 <- merge(Opp1, factors, by = "position")
    
    Opp1 <- subset(Opp1, grade != 0)
    
    Opp1 <- Opp1 %>%
      mutate(weekly_grade = if_else(status == "Questionable", grade * factor, grade)) %>%
      select(position, player, team, weekly_grade) %>%
      group_by(position) %>%
      summarize(mean(weekly_grade), sum(weekly_grade))
    
    OppDif <- merge(Opp1, OppPre, by = "position")
    
    OppDif <- OppDif %>%
      mutate(Availability_Percentage = (`sum(weekly_grade)`/`sum(grade)`)) %>%
      mutate(`Availability Percentage` = percent(Availability_Percentage)) %>%
      mutate(`Average Grade Per Position` = round(`mean(weekly_grade)`, 2)) %>%
      rename("Position" = position) %>%
      select(Position, `Availability Percentage`, `Average Grade Per Position`)
    
    formattable(OppDif, list(
      `Availability Percentage` = color_bar(OppColor())
    ))
    
  })
  
  output$Pack_inj_players <- renderFormattable({
    injplayers <- packavail %>%
      filter(team == "GB" & week == input$week & status != "Active") %>%
      select(player, team, position, status, `Reason of Absence`) %>%
      rename("Player" = player, "Team" = team, "Position" = position, "Status" = status)
    
    formattable(injplayers)
  })
  
  output$Opp_inj_players <- renderFormattable({
    injplayers <- packavail %>%
      filter(team == teamintin() & week == input$week & status != "Active") %>%
      select(player, team, position, status, `Reason of Absence`) %>%
      rename("Player" = player, "Team" = team, "Position" = position, "Status" = status)
    
    formattable(injplayers)
  })
  
  output$Packers_pos_int <- renderFormattable({
    PackersPre <- packavail %>%
      filter(team == "GB") %>%
      filter(week == "pre-season") %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench") %>%
      group_by(position) %>%
      summarize(sum(grade))
    
    Packers1 <- packavail %>%
      filter(team == "GB") %>%
      filter(week == input$week) %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench")
    
    Packers1 <- merge(Packers1, factors, by = "position")
    
    Packers1 <- subset(Packers1, grade != 0)
    
    Packers1 <- Packers1 %>%
      mutate(weekly_grade = if_else(status == "Questionable", grade * factor, grade)) %>%
      select(position, player, team, weekly_grade) %>%
      group_by(position) %>%
      summarize(mean(weekly_grade), sum(weekly_grade))
    
    PackersDif <- merge(Packers1, PackersPre, by = "position")
    
    PackersDif <- PackersDif %>%
      mutate(Availability_Percentage = (`sum(weekly_grade)`/`sum(grade)`)) %>%
      mutate(`Availability Percentage` = percent(Availability_Percentage)) %>%
      mutate(`Average Grade Per Position` = round(`mean(weekly_grade)`, 2)) %>%
      rename("Position" = position) %>%
      select(Position, `Availability Percentage`,`Average Grade Per Position`)
    
    PackersDifference <- PackersDif %>%
      filter(`Availability Percentage` != "100.00%") %>%
      select(Position)
    
    PackersDifference <- as.list(PackersDifference$Position)
    
    PackPre <- packavail %>%
      filter(team == "GB" & week == "pre-season") %>%
      filter(position %in% PackersDifference) %>%
      select(player, team, position, week, `weekly depth`)
    
    PackWeekInput <- packavail %>%
      filter(team == "GB" & week == input$week) %>%
      filter(position %in% PackersDifference) %>%
      select(player, week, `weekly depth`)
    
    PackWeekPosDif <- left_join(PackPre, PackWeekInput, by = "player")
    
    PackWeekPosDif <- PackWeekPosDif %>%
      mutate(`Pre-season Depth` = `weekly depth.x`,
             `Weekly Depth` = `weekly depth.y`) %>%
      select(player, team, position, `Pre-season Depth`, `Weekly Depth`)
    
    PackWeekPosDif <- PackWeekPosDif %>%
      filter(`Pre-season Depth` != `Weekly Depth`) %>%
      rename("Position" = position, "Team" = team, "Player" = player)
    
    formattable(PackWeekPosDif)
    
  })
  
  
  output$Opp_pos_int <- renderFormattable({
    
    OppPre <- packavail %>%
      filter(team == teamintin()) %>%
      filter(week == "pre-season") %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench") %>%
      group_by(position) %>%
      summarize(sum(grade))
    
    
    Opp1 <- packavail %>%
      filter(team == teamintin()) %>%
      filter(week == input$week) %>%
      filter(`weekly depth` == "Starter" | `weekly depth` == "Bench")
    
    Opp1 <- merge(Opp1, factors, by = "position")
    
    Opp1 <- subset(Opp1, grade != 0)
    
    Opp1 <- Opp1 %>%
      mutate(weekly_grade = if_else(status == "Questionable", grade * factor, grade)) %>%
      select(position, player, team, weekly_grade) %>%
      group_by(position) %>%
      summarize(mean(weekly_grade), sum(weekly_grade))
    
    OppDif <- merge(Opp1, OppPre, by = "position")
    
    OppDif <- OppDif %>%
      mutate(Availability_Percentage = (`sum(weekly_grade)`/`sum(grade)`)) %>%
      mutate(`Availability Percentage` = percent(Availability_Percentage)) %>%
      mutate(`Average Grade Per Position` = round(`mean(weekly_grade)`, 2)) %>%
      rename("Position" = position) %>%
      select(Position, `Availability Percentage`, `Average Grade Per Position`)
    
    OppDifference <- OppDif %>%
      filter(`Availability Percentage` != "100.00%") %>%
      select(Position)
    
    OppDifference <- as.list(OppDifference$Position)
    
    OppsPre <- packavail %>%
      filter(team == teamintin() & week == "pre-season") %>%
      filter(position %in% OppDifference) %>%
      select(player, team, position, week, `weekly depth`)
    
    OppWeekInput <- packavail %>%
      filter(team == teamintin() & week == input$week) %>%
      filter(position %in% OppDifference) %>%
      select(player, week, `weekly depth`)
    
    OppWeekPosDif <- left_join(OppsPre, OppWeekInput, by = "player")
    
    OppWeekPosDif <- OppWeekPosDif %>%
      mutate(`Pre-season Depth` = `weekly depth.x`,
             `Weekly Depth` = `weekly depth.y`) %>%
      select(player, team, position, `Pre-season Depth`, `Weekly Depth`)
    
    OppWeekPosDif <- OppWeekPosDif %>%
      filter(`Pre-season Depth` != `Weekly Depth`) %>%
      rename("Position" = position, "Team" = team, "Player" = player)
    
    
    formattable(OppWeekPosDif)
    
  })
  
}

shinyApp(ui, server)
