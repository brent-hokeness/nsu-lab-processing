# Learning R Shiny for Athlete Monitoring

# Install libraries if needed
# install.packages(c('shiny','dplyr','googlesheets4','googledrive','flexdashboard','ggplot2'))

# Load libraries
library(shiny)
library(flexdashboard)    # for gauges
library(dplyr)
library(googlesheets4)
library(googledrive)
library(rsconnect)
library(ggplot2)

# Read in new assessment data from Google Sheets
data_url <- "https://docs.google.com/spreadsheets/d/1swTbrsnVsumXeWBb3q42WxBJmr4u3Vib2x8lLwjLyws/edit?gid=0#gid=0"
new_assessment <- read_sheet(data_url, sheet = "Sheet1")
colnames(new_assessment) <- toupper(colnames(new_assessment))

# Convert DATE column (format: m/d/Y) to Date class
new_assessment$DATE <- as.Date(new_assessment$DATE, format = "%m/%d/%Y")

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("NSU Baseball Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("playerName", "Select Player Name:",
                  choices = unique(new_assessment$PARTICIPANT)),
      
      fluidRow(
        column(6, h4("Predicted Bat Speed")),
        column(6, gaugeOutput("predictedVeloGauge", height = "120px"))
      ),
      textOutput("predictedVeloDate"),
      textOutput("predictedVelo"),
      br(),
      
      fluidRow(
        column(6, h4("Bat Speed")),
        column(6, gaugeOutput("batSpeedGauge", height = "120px"))
      ),
      textOutput("batSpeedDate"),
      textOutput("batSpeed"),
      br(),
      
      div(style = "background-color: darkred; padding: 10px; color: white; border-radius: 3px;",
          h3("Player Details")
      ),
      textOutput("position"),
      textOutput("grade"),
      textOutput("height"),
      textOutput("weight"),
      br()
    ),
    mainPanel(
      div(style = "background-color: darkred; padding: 10px; color: white; border-radius: 3px;",
          h3("Assessment Scores")
      ),
      br(),
      fluidRow(
        column(2,
               gaugeOutput("normalizedPeakGrfPhase1Gauge", height = "120px"),
               div(style = "text-align: center; font-weight: bold", textOutput("normalizedPeakGrfPhase1Label")),
               div(style = "text-align: center; font-size: 80%", textOutput("normalizedPeakGrfPhase1Date"))
        ),
        column(2,
               gaugeOutput("flying10Gauge", height = "120px"),
               div(style = "text-align: center; font-weight: bold", textOutput("flying10Label")),
               div(style = "text-align: center; font-size: 80%", textOutput("flying10Date"))
        ),
        column(2,
               gaugeOutput("frontSquatGauge", height = "120px"),
               div(style = "text-align: center; font-weight: bold", textOutput("frontSquatLabel")),
               div(style = "text-align: center; font-size: 80%", textOutput("frontSquatDate"))
        ),
        column(2,
               gaugeOutput("deadliftGauge", height = "120px"),
               div(style = "text-align: center; font-weight: bold", textOutput("deadliftLabel")),
               div(style = "text-align: center; font-size: 80%", textOutput("deadliftDate"))
        ),
        column(2,
               gaugeOutput("benchPressGauge", height = "120px"),
               div(style = "text-align: center; font-weight: bold", textOutput("benchPressLabel")),
               div(style = "text-align: center; font-size: 80%", textOutput("benchPressDate"))
        ),
        column(2,
               gaugeOutput("powerCleanGauge", height = "120px"),
               div(style = "text-align: center; font-weight: bold", textOutput("powerCleanLabel")),
               div(style = "text-align: center; font-size: 80%", textOutput("powerCleanDate"))
        )
      ),
      br(),
      
      div(style = "background-color: goldenrod; padding: 10px; color: white; border-radius: 3px;",
          h4("Percentile Scores")
      ),
      plotOutput("categoriesBarChart"),
      br(),
      
      div(style = "background-color: goldenrod; padding: 10px; color: white; border-radius: 3px;",
          h4("CMJ Section")
      ),
      selectInput("selectedCMJMetric", "Select CMJ Metric:", choices = NULL),
      plotOutput("cmjBarGraph", height = "300px"),
      br(),
      
      div(style = "background-color: goldenrod; padding: 10px; color: white; border-radius: 3px;",
          h4("Performance Metrics")
      ),
      selectInput("selectedPerformanceMetric", "Select Performance Metric:", choices = NULL),
      plotOutput("performanceBarGraph", height = "300px"),
      br(),
      
      div(style = "background-color: goldenrod; padding: 10px; color: white; border-radius: 3px;",
          h4("Baseball Performance Metrics")
      ),
      selectInput("selectedBaseballMetric", "Select Baseball Performance Metric:", choices = NULL),
      plotOutput("rmBarGraph", height = "300px")
    )
  )
)

server <- function(input, output, session) {
  # Function to get most recent non-NA value for a given metric
  get_most_recent_value <- function(df, metric_name) {
    df <- df %>% 
      filter(!is.na(.data[[metric_name]])) %>%
      arrange(desc(DATE))
    
    if (nrow(df) > 0) {
      return(df[1, ])
    } else {
      return(NULL)
    }
  }
  
  # Reactive for player data
  player_data <- reactive({
    req(input$playerName)
    filter(new_assessment, PARTICIPANT == input$playerName)
  })
  
  # Reactive for latest player info (for general details like position, grade, etc.)
  latest_player_info <- reactive({
    req(player_data())
    player_data() %>% arrange(desc(DATE)) %>% head(1)
  })
  
  # Reactive for most recent data for each metric
  most_recent_data <- reactive({
    req(player_data())
    df <- player_data()
    
    list(
      # CMJ metrics
      normalized_peak_grf_phase1 = get_most_recent_value(df, "NORMALIZED PEAK GRF PHASE 1"),
      normalized_peak_grf_phase1_percentile = get_most_recent_value(df, "NORMALIZED PEAK GRF PHASE 1_PERCENTILE"),
      
      # Performance metrics
      flying10 = get_most_recent_value(df, "FLYING 10"),
      front_squat = get_most_recent_value(df, "FRONT SQUAT"),
      front_squat_percentile = get_most_recent_value(df, "FRONT SQUAT_PERCENTILE"),
      deadlift = get_most_recent_value(df, "DEADLIFT"),
      deadlift_percentile = get_most_recent_value(df, "DEADLIFT_PERCENTILE"),
      bench_press = get_most_recent_value(df, "BENCH PRESS"),
      bench_press_percentile = get_most_recent_value(df, "BENCH PRESS_PERCENTILE"),
      power_clean = get_most_recent_value(df, "POWER CLEAN"),
      power_clean_percentile = get_most_recent_value(df, "POWER CLEAN_PERCENTILE"),
      
      # Baseball metrics
      bat_speed = get_most_recent_value(df, "BAT SPEED"),
      predicted_bat_speed = get_most_recent_value(df, "PREDICTED BAT SPEED"),
      rotational_acceleration = get_most_recent_value(df, "ROTATIONAL ACCELERATION"),
      pitch_velo = get_most_recent_value(df, "PITCH VELO")
    )
  })
  
  # Reactive for longitudinal history
  player_data_all_events <- reactive({
    req(input$playerName)
    filter(new_assessment, PARTICIPANT == input$playerName)
  })
  
  # Format date as mm/dd/yyyy
  format_date <- function(date) {
    if(is.null(date) || is.na(date)) return("No data")
    format(as.Date(date), "%m/%d/%Y")
  }
  
  # Populate dropdowns
  observe({
    cmj_cols <- c("AVG PEAK FORCE PHASE 1 (N)", "PERCENT DIFFERENCE PHASE 1 (%)",
                  "COM VELOCITY AT VEM5", "GRF AT VEM5",
                  "NORMALIZED PEAK GRF PHASE 1", "NORMALIZED PEAK GRF PHASE 2",
                  "NORMALIZED GRF AT VEM5", "RAW PEAK GRF PHASE 1", "RAW PEAK GRF PHASE 2")
    updateSelectInput(session, "selectedCMJMetric", choices = cmj_cols, selected = cmj_cols[1])
  })
  observe({
    perf_cols <- c("FLYING 10","FRONT SQUAT","BENCH PRESS","DEADLIFT","POWER CLEAN")
    updateSelectInput(session, "selectedPerformanceMetric", choices = perf_cols, selected = perf_cols[1])
  })
  observe({
    ball_cols <- c("BAT SPEED","ROTATIONAL ACCELERATION","PITCH VELO")
    updateSelectInput(session, "selectedBaseballMetric", choices = ball_cols, selected = ball_cols[1])
  })
  
  # Text outputs - player info
  output$position <- renderText({ paste("Position:", latest_player_info()$POSITION) })
  output$grade    <- renderText({ paste("Grade:",    latest_player_info()$GRADE) })
  output$height   <- renderText({ paste("Height:",   latest_player_info()$HEIGHT) })
  output$weight   <- renderText({ paste("Weight:",   latest_player_info()$WEIGHT) })
  
  # Text outputs - metric values and dates
  output$predictedVelo <- renderText({ 
    data <- most_recent_data()$predicted_bat_speed
    if(!is.null(data)) {
      sprintf("Predicted Bat Speed: %.2f mph", data$`PREDICTED BAT SPEED`) 
    } else {
      "Predicted Bat Speed: No data"
    }
  })
  output$predictedVeloDate <- renderText({ 
    data <- most_recent_data()$predicted_bat_speed
    if(!is.null(data)) {
      paste("Date:", format_date(data$DATE))
    } else {
      ""
    }
  })
  
  output$batSpeed <- renderText({ 
    data <- most_recent_data()$bat_speed
    if(!is.null(data)) {
      sprintf("Bat Speed: %.2f mph", data$`BAT SPEED`) 
    } else {
      "Bat Speed: No data"
    }
  })
  output$batSpeedDate <- renderText({ 
    data <- most_recent_data()$bat_speed
    if(!is.null(data)) {
      paste("Date:", format_date(data$DATE))
    } else {
      ""
    }
  })
  
  # Date outputs for gauges
  output$normalizedPeakGrfPhase1Date <- renderText({
    data <- most_recent_data()$normalized_peak_grf_phase1
    if(!is.null(data)) format_date(data$DATE) else "No data"
  })
  output$flying10Date <- renderText({
    data <- most_recent_data()$flying10
    if(!is.null(data)) format_date(data$DATE) else "No data"
  })
  output$frontSquatDate <- renderText({
    data <- most_recent_data()$front_squat
    if(!is.null(data)) format_date(data$DATE) else "No data"
  })
  output$deadliftDate <- renderText({
    data <- most_recent_data()$deadlift
    if(!is.null(data)) format_date(data$DATE) else "No data"
  })
  output$benchPressDate <- renderText({
    data <- most_recent_data()$bench_press
    if(!is.null(data)) format_date(data$DATE) else "No data"
  })
  output$powerCleanDate <- renderText({
    data <- most_recent_data()$power_clean
    if(!is.null(data)) format_date(data$DATE) else "No data"
  })
  
  # Gauges
  output$normalizedPeakGrfPhase1Gauge <- renderGauge({
    data <- most_recent_data()$normalized_peak_grf_phase1
    if(!is.null(data)) {
      value <- data$`NORMALIZED PEAK GRF PHASE 1`
      gauge(value, min=0, max=3, symbol="N",
            gaugeSectors(success=c(1.5,3), warning=c(1,1.49), danger=c(0,0.99)))
    } else {
      gauge(0, min=0, max=3, symbol="N",
            gaugeSectors(success=c(1.5,3), warning=c(1,1.49), danger=c(0,0.99)))
    }
  })
  
  output$flying10Gauge <- renderGauge({
    data <- most_recent_data()$flying10
    if(!is.null(data)) {
      val <- data$`FLYING 10`
      gauge(sprintf("%.3f",val), min=0.95, max=1.25, symbol='s',
            gaugeSectors(success=c(0.95,1.02), warning=c(1.03,1.06), danger=c(1.07,1.25)))
    } else {
      gauge(0, min=0.95, max=1.25, symbol='s',
            gaugeSectors(success=c(0.95,1.02), warning=c(1.03,1.06), danger=c(1.07,1.25)))
    }
  })
  
  output$frontSquatGauge <- renderGauge({ 
    data <- most_recent_data()$front_squat
    if(!is.null(data)) {
      value <- data$`FRONT SQUAT`
      gauge(value, min=0, max=350,
            gaugeSectors(success=c(275,350), warning=c(225,274), danger=c(0,224))) 
    } else {
      gauge(0, min=0, max=350,
            gaugeSectors(success=c(275,350), warning=c(225,274), danger=c(0,224)))
    }
  })
  
  output$deadliftGauge <- renderGauge({ 
    data <- most_recent_data()$deadlift
    if(!is.null(data)) {
      value <- data$`DEADLIFT`
      gauge(value, min=0, max=500,
            gaugeSectors(success=c(400,500), warning=c(350,399), danger=c(0,349)))
    } else {
      gauge(0, min=0, max=500,
            gaugeSectors(success=c(400,500), warning=c(350,399), danger=c(0,349)))
    }
  })
  
  output$benchPressGauge <- renderGauge({ 
    data <- most_recent_data()$bench_press
    if(!is.null(data)) {
      value <- data$`BENCH PRESS`
      gauge(value, min=0, max=300,
            gaugeSectors(success=c(215,300), warning=c(185,214), danger=c(0,184)))
    } else {
      gauge(0, min=0, max=300,
            gaugeSectors(success=c(215,300), warning=c(185,214), danger=c(0,184)))
    }
  })
  
  output$powerCleanGauge <- renderGauge({ 
    data <- most_recent_data()$power_clean
    if(!is.null(data)) {
      value <- data$`POWER CLEAN`
      gauge(value, min=0, max=275,
            gaugeSectors(success=c(225,275), warning=c(185,224), danger=c(0,184)))
    } else {
      gauge(0, min=0, max=275,
            gaugeSectors(success=c(225,275), warning=c(185,224), danger=c(0,184)))
    }
  })
  
  output$predictedVeloGauge <- renderGauge({ 
    data <- most_recent_data()$predicted_bat_speed
    if(!is.null(data)) {
      value <- data$`PREDICTED BAT SPEED`
      gauge(value, min=50, max=90, symbol='MPH',
            gaugeSectors(success=c(70,90), warning=c(65,69.99), danger=c(50,64.99)))
    } else {
      gauge(50, min=50, max=90, symbol='MPH',
            gaugeSectors(success=c(70,90), warning=c(65,69.99), danger=c(50,64.99)))
    }
  })
  
  output$batSpeedGauge <- renderGauge({ 
    data <- most_recent_data()$bat_speed
    if(!is.null(data)) {
      value <- data$`BAT SPEED`
      gauge(value, min=50, max=90, symbol='MPH',
            gaugeSectors(success=c(70,90), warning=c(65,69.99), danger=c(50,64.99)))
    } else {
      gauge(50, min=50, max=90, symbol='MPH',
            gaugeSectors(success=c(70,90), warning=c(65,69.99), danger=c(50,64.99)))
    }
  })
  
  # Gauge labels
  output$normalizedPeakGrfPhase1Label <- renderText({"Peak CMJ Force (Norm)"})
  output$flying10Label <- renderText({"Flying 10"})
  output$frontSquatLabel <- renderText({"Front Squat"})
  output$deadliftLabel <- renderText({"Deadlift"})
  output$benchPressLabel <- renderText({"Bench Press"})
  output$powerCleanLabel <- renderText({"Power Clean"})
  output$predictedVeloLabel <- renderText({"Predicted Bat Speed"})
  output$batSpeedLabel <- renderText({"Bat Speed"})
  
  # Categories bar chart
  output$categoriesBarChart <- renderPlot({
    metrics_data <- list(
      "CMJ Force" = most_recent_data()$normalized_peak_grf_phase1_percentile,
      "Front Squat" = most_recent_data()$front_squat_percentile,
      "Deadlift" = most_recent_data()$deadlift_percentile,
      "Bench Press" = most_recent_data()$bench_press_percentile,
      "Power Clean" = most_recent_data()$power_clean_percentile
    )
    
    # Create dataframe for plotting
    df <- data.frame(
      Category = character(),
      Score = numeric(),
      Date = as.Date(character()),
      stringsAsFactors = FALSE
    )
    
    # Add data for each metric if available
    for (name in names(metrics_data)) {
      data <- metrics_data[[name]]
      if (!is.null(data)) {
        # Set the correct percentile column for each metric
        if (name == "CMJ Force") {
          percentile_col <- "NORMALIZED PEAK GRF PHASE 1_PERCENTILE"
        } else if (name == "Front Squat") {
          percentile_col <- "FRONT SQUAT_PERCENTILE"
        } else if (name == "Deadlift") {
          percentile_col <- "DEADLIFT_PERCENTILE"
        } else if (name == "Bench Press") {
          percentile_col <- "BENCH PRESS_PERCENTILE"
        } else if (name == "Power Clean") {
          percentile_col <- "POWER CLEAN_PERCENTILE"
        }
        
        # Check if the percentile column exists and is not NA
        if(percentile_col %in% colnames(data) && !is.na(data[[percentile_col]])) {
          df <- rbind(df, data.frame(
            Category = name,
            Score = data[[percentile_col]],
            Date = data$DATE,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # If we have data to plot
    if (nrow(df) > 0) {
      # Set desired order
      desired_order <- c("CMJ Force", "Front Squat", "Deadlift", "Bench Press", "Power Clean")
      df$Category <- factor(df$Category, levels = desired_order)
      
      # Create plot
      p <- ggplot(df, aes(x=Category, y=Score, fill=Category)) +
        geom_col() +
        geom_text(aes(label=round(Score,2)), vjust=-0.5) +
        geom_text(aes(label=format(Date, "%m/%d/%Y"), y=0.05), 
                  color="white", size=3, angle=90, hjust=0) +
        scale_fill_manual(values=rep("firebrick4", length(unique(df$Category)))) +
        ylim(0,1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, hjust=1),
              legend.position = "none")
      
      return(p)
    } else {
      # Return empty plot if no data
      ggplot() + 
        theme_minimal() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available") +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
    }
  })
  
  # Longitudinal CMJ over DATE
  output$cmjBarGraph <- renderPlot({
    req(player_data_all_events(), input$selectedCMJMetric)
    df <- player_data_all_events() %>% arrange(DATE)
    
    # Check if metric exists in data
    if(input$selectedCMJMetric %in% colnames(df)) {
      # Filter out NA values for selected metric
      df <- df[!is.na(df[[input$selectedCMJMetric]]), ]
      
      if(nrow(df) > 0) {
        ggplot(df, aes(x=DATE, y=.data[[input$selectedCMJMetric]])) +
          geom_col(fill="firebrick4") +
          geom_text(aes(label=round(.data[[input$selectedCMJMetric]],2)), vjust=-0.5) +
          scale_x_date(date_labels="%m/%d/%Y") +
          labs(x="Date", y=input$selectedCMJMetric) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle=45, hjust=1))
      } else {
        ggplot() + 
          theme_minimal() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available for this metric") +
          theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()
          )
      }
    } else {
      ggplot() + 
        theme_minimal() + 
        annotate("text", x = 0.5, y = 0.5, label = "Metric not found in data") +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
    }
  })
  
  # Longitudinal Performance over DATE
  output$performanceBarGraph <- renderPlot({
    req(player_data_all_events(), input$selectedPerformanceMetric)
    df <- player_data_all_events() %>% arrange(DATE)
    
    # Check if metric exists in data
    if(input$selectedPerformanceMetric %in% colnames(df)) {
      # Filter out NA values for selected metric
      df <- df[!is.na(df[[input$selectedPerformanceMetric]]), ]
      
      if(nrow(df) > 0) {
        ggplot(df, aes(x=DATE, y=.data[[input$selectedPerformanceMetric]])) +
          geom_col(fill="firebrick4") +
          geom_text(aes(label=round(.data[[input$selectedPerformanceMetric]],2)), vjust=-0.5) +
          scale_x_date(date_labels="%m/%d/%Y") +
          labs(x="Date", y=input$selectedPerformanceMetric) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle=45, hjust=1))
      } else {
        ggplot() + 
          theme_minimal() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available for this metric") +
          theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()
          )
      }
    } else {
      ggplot() + 
        theme_minimal() + 
        annotate("text", x = 0.5, y = 0.5, label = "Metric not found in data") +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
    }
  })
  
  # Longitudinal Baseball Metrics over DATE
  output$rmBarGraph <- renderPlot({
    req(player_data_all_events(), input$selectedBaseballMetric)
    df <- player_data_all_events() %>% arrange(DATE)
    
    # Check if metric exists in data
    if(input$selectedBaseballMetric %in% colnames(df)) {
      # Filter out NA values for selected metric
      df <- df[!is.na(df[[input$selectedBaseballMetric]]), ]
      
      if(nrow(df) > 0) {
        ggplot(df, aes(x=DATE, y=.data[[input$selectedBaseballMetric]])) +
          geom_col(fill="firebrick4") +
          geom_text(aes(label=round(.data[[input$selectedBaseballMetric]],2)), vjust=-0.5) +
          scale_x_date(date_labels="%m/%d/%Y") +
          labs(x="Date", y=input$selectedBaseballMetric) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle=45, hjust=1))
      } else {
        ggplot() + 
          theme_minimal() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available for this metric") +
          theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()
          )
      }
    } else {
      ggplot() + 
        theme_minimal() + 
        annotate("text", x = 0.5, y = 0.5, label = "Metric not found in data") +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
    }
  })
}

# Run the application
shinyApp(ui, server)
