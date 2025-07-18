# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(viridis)

# Check if data file exists
if (!file.exists("data/LTCS.csv")) {
  stop("Data file 'data/LTCS.csv' not found. Please ensure the file is in the correct location.")
}

# Read the data with error handling
tryCatch({
  data <- read_csv("data/LTCS.csv")
  
  # Check for required columns
  required_cols <- c("Borough", "Neighbourhood", "CohortPop", "AtFib", "Cardiovascular_disease", "CKD", "Diabetes", "Hypertension")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  cat("Data loaded successfully. Rows:", nrow(data), "Columns:", ncol(data), "\n")
}, error = function(e) {
  stop("Error reading data file: ", e$message)
})

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "LTC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Borough Overview", tabName = "borough", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    ),
    br(),
    selectInput("condition", "Select Health Condition:",
                choices = c("Atrial Fibrillation" = "AtFib",
                            "Cardiovascular Disease" = "Cardiovascular_disease",
                            "Chronic Kidney Disease" = "CKD",
                            "Diabetes" = "Diabetes",
                            "Hypertension" = "Hypertension"),
                selected = "Hypertension"),
    br(),
    radioButtons("view_type", "View Type:",
                 choices = c("Rate" = "rate",
                             "Proportional Rate" = "proportional"),
                 selected = "rate"),
    br(),
    actionButton("reset", "Reset to Borough View", class = "btn-warning")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "borough",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = textOutput("chart_title"),
                    plotlyOutput("main_plot", height = "500px")
                )
              ),
              fluidRow(
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Instructions",
                    p("• Select a Long Term Health Condition from the dropdown menu"),
                    p("• Choose between 'Rate' (per 1,000 population) or 'Proportional Rate' (% difference from average)"),
                    p("• Click on any borough bar to see neighborhood-level data"),
                    p("• Use the 'Reset to Borough View' button to return to borough overview"),
                    p("• The red dashed line shows the overall average (or 0% for proportional view)")
                )
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Raw Data",
                    DT::dataTableOutput("data_table")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to track current view
  values <- reactiveValues(
    current_view = "borough",
    selected_borough = NULL
  )
  
  # Reset to borough view
  observeEvent(input$reset, {
    values$current_view <- "borough"
    values$selected_borough <- NULL
  })
  
  # Prepare data based on current view
  plot_data <- reactive({
    condition <- input$condition
    
    # Calculate overall average across all neighborhoods
    overall_avg <- data %>%
      summarise(
        total_cases = sum(!!sym(condition), na.rm = TRUE),
        total_pop = sum(CohortPop, na.rm = TRUE)
      ) %>%
      mutate(avg_rate = (total_cases / total_pop) * 1000) %>%
      pull(avg_rate)
    
    if (values$current_view == "borough") {
      # Aggregate by borough
      borough_data <- data %>%
        group_by(Borough) %>%
        summarise(
          condition_count = sum(!!sym(condition), na.rm = TRUE),
          population = sum(CohortPop, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(
          rate_per_1000 = (condition_count / population) * 1000,
          proportional_rate = ((rate_per_1000 - overall_avg) / overall_avg) * 100,
          label = Borough
        ) %>%
        # Handle any potential NA or infinite values
        mutate(
          rate_per_1000 = ifelse(is.na(rate_per_1000) | is.infinite(rate_per_1000), 0, rate_per_1000),
          proportional_rate = ifelse(is.na(proportional_rate) | is.infinite(proportional_rate), 0, proportional_rate),
          condition_count = ifelse(is.na(condition_count), 0, condition_count),
          population = ifelse(is.na(population), 0, population)
        )
      return(borough_data)
    } else {
      # Show neighborhoods for selected borough
      neighborhood_data <- data %>%
        filter(Borough == values$selected_borough) %>%
        mutate(
          condition_count = as.numeric(!!sym(condition)),
          population = as.numeric(CohortPop),
          rate_per_1000 = (condition_count / population) * 1000,
          proportional_rate = ((rate_per_1000 - overall_avg) / overall_avg) * 100,
          label = Neighbourhood
        ) %>%
        # Handle any potential NA or infinite values
        mutate(
          rate_per_1000 = ifelse(is.na(rate_per_1000) | is.infinite(rate_per_1000), 0, rate_per_1000),
          proportional_rate = ifelse(is.na(proportional_rate) | is.infinite(proportional_rate), 0, proportional_rate),
          condition_count = ifelse(is.na(condition_count), 0, condition_count),
          population = ifelse(is.na(population), 0, population)
        )
      return(neighborhood_data)
    }
  })
  
  # Dynamic chart title
  output$chart_title <- renderText({
    condition_name <- case_when(
      input$condition == "AtFib" ~ "Atrial Fibrillation",
      input$condition == "Cardiovascular_disease" ~ "Cardiovascular Disease",
      input$condition == "CKD" ~ "Chronic Kidney Disease",
      input$condition == "Diabetes" ~ "Diabetes",
      input$condition == "Hypertension" ~ "Hypertension"
    )
    
    view_suffix <- if(input$view_type == "rate") {
      "(per 1,000 population)"
    } else {
      "(% difference from overall average)"
    }
    
    if (values$current_view == "borough") {
      paste(condition_name, "by Borough", view_suffix)
    } else {
      paste(condition_name, "in", values$selected_borough, "Neighborhoods", view_suffix)
    }
  })
  
  # Main plot
  output$main_plot <- renderPlotly({
    plot_df <- plot_data()
    condition <- input$condition
    
    # Ensure we have data
    if (nrow(plot_df) == 0) {
      return(plotly_empty())
    }
    
    # Calculate overall average across all neighborhoods
    overall_avg <- data %>%
      summarise(
        total_cases = sum(!!sym(condition), na.rm = TRUE),
        total_pop = sum(CohortPop, na.rm = TRUE)
      ) %>%
      mutate(avg_rate = (total_cases / total_pop) * 1000) %>%
      pull(avg_rate)
    
    # Generate viridis colors based on data length
    n_bars <- nrow(plot_df)
    colors <- viridis(n_bars, option = if(values$current_view == "borough") "D" else "C")
    
    # Choose y variable and reference line based on view type
    if (input$view_type == "rate") {
      y_var <- plot_df$rate_per_1000
      y_title <- "Rate per 1,000 population"
      reference_line <- overall_avg
      reference_label <- paste("Overall Average:", round(overall_avg, 1))
    } else {
      y_var <- plot_df$proportional_rate
      y_title <- "% Difference from Overall Average"
      reference_line <- 0
      reference_label <- "Overall Average (0%)"
    }
    
    # Determine y variable based on view type
    if (input$view_type == "rate") {
      y_var <- ~rate_per_1000
      y_title <- "Rate per 1,000 population"
      reference_line <- overall_avg
      reference_label <- paste("Overall Average:", round(overall_avg, 1))
    } else {
      y_var <- ~proportional_rate
      y_title <- "% Difference from Overall Average"
      reference_line <- 0
      reference_label <- "Overall Average (0%)"
    }
    
    # Create base plot with proper object references
    p <- plot_df %>%
      plot_ly(
        x = ~reorder(label, if(input$view_type == "rate") rate_per_1000 else proportional_rate),
        y = y_var,
        type = "bar",
        marker = list(color = colors),
        text = ~paste(
          "<b>", label, "</b><br>",
          if (input$view_type == "rate") {
            paste("Rate:", round(rate_per_1000, 1), "per 1,000<br>")
          } else {
            paste("Difference:", round(proportional_rate, 1), "%<br>")
          },
          "Cases:", prettyNum(condition_count, big.mark = ","), "<br>",
          "Population:", prettyNum(population, big.mark = ",")
        ),
        hovertemplate = "%{text}<extra></extra>",
        textposition = "none"
      ) %>%
      layout(
        xaxis = list(
          title = if(values$current_view == "borough") "Borough" else "Neighbourhood",
          tickangle = -45
        ),
        yaxis = list(title = y_title),
        margin = list(b = 120, l = 60, r = 40, t = 40),
        showlegend = FALSE,
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = reference_line,
            y1 = reference_line,
            line = list(color = "red", dash = "dash", width = 2)
          )
        ),
        annotations = list(
          list(
            x = 0.02,
            y = reference_line,
            xref = "paper",
            yref = "y",
            text = reference_label,
            showarrow = FALSE,
            bgcolor = "rgba(255,255,255,0.8)",
            bordercolor = "red",
            borderwidth = 1,
            font = list(color = "red", size = 10)
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add click event only for borough view
    if (values$current_view == "borough") {
      p <- p %>%
        event_register("plotly_click")
    }
    
    p
  })
  
  # Handle plot clicks
  observeEvent(event_data("plotly_click"), {
    if (values$current_view == "borough") {
      click_data <- event_data("plotly_click")
      if (!is.null(click_data)) {
        clicked_borough <- plot_data()$label[click_data$pointNumber + 1]
        values$selected_borough <- clicked_borough
        values$current_view <- "neighborhood"
      }
    }
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    # Calculate rates
    display_data <- data %>%
      mutate(
        AtFib_rate = round((AtFib / CohortPop) * 1000, 2),
        CVD_rate = round((Cardiovascular_disease / CohortPop) * 1000, 2),
        CKD_rate = round((CKD / CohortPop) * 1000, 2),
        Diabetes_rate = round((Diabetes / CohortPop) * 1000, 2),
        Hypertension_rate = round((Hypertension / CohortPop) * 1000, 2)
      ) %>%
      select(
        Neighbourhood, Borough, CohortPop,
        AtFib, AtFib_rate,
        Cardiovascular_disease, CVD_rate,
        CKD, CKD_rate,
        Diabetes, Diabetes_rate,
        Hypertension, Hypertension_rate
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 2:ncol(display_data)-1)
        )
      ),
      colnames = c(
        "Neighbourhood", "Borough", "Population",
        "AtFib Cases", "AtFib Rate/1000",
        "CVD Cases", "CVD Rate/1000",
        "CKD Cases", "CKD Rate/1000",
        "Diabetes Cases", "Diabetes Rate/1000",
        "Hypertension Cases", "Hypertension Rate/1000"
      )
    ) %>%
      DT::formatRound(columns = c("AtFib_rate", "CVD_rate", "CKD_rate", "Diabetes_rate", "Hypertension_rate"), digits = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)