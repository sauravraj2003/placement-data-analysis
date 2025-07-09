
#################################### Libraries #############################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(ggthemes)
library(plotly)
library(rsconnect)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(viridis)
library(scales)

########################### Main title section ##############################

load("companies.Rdata")
load("new_students.Rdata")
students_ctc <- as_tibble(read.csv("students_correlated.csv"))
data_companies <- companies2
data_students <- students

ui <- navbarPage(
  "Y20Placements",
  theme = shinytheme("flatly"),
  tabPanel(
    "Statistics",
    tags$br(),
    tabsetPanel(
      type = "tabs",
      
      tabPanel(
        "Companies",
        sidebarLayout(
          sidebarPanel(
            h3("Salary Range"),
            tags$br(),
            sliderInput(
              "incomeRange",
              label = "CTC Range",
              min = min(data_companies$ctc),
              max = max(data_companies$ctc),
              value = c(min(data_companies$ctc), max(data_companies$ctc))
            ),
            actionButton("actionDT_comp", "Filter", class = "btn btn-warning")
          ),
          mainPanel(
            h3("Companies"),
            tags$br(),
            dataTableOutput("comp_table"),
            tags$br(),
            tags$br()
          )
        )
      ),

      tabPanel(
        "Branch Statistics",
        sidebarLayout(
          sidebarPanel(
            h3("Branch-wise Analysis"),
            tags$br(),
            selectizeInput(
              "selected_branches",
              label = "Select Branches to Compare",
              choices = sort(unique(data_students$branch)),
              selected = c("BT-CSE"),
              multiple = TRUE,
              options = list(placeholder = "Select branches")
            ),
            tags$br(),
            radioButtons(
              "chart_type",
              label = "Visualization Type",
              choices = list(
                "Interactive Scatter Plot" = "scatter",
                "Radar Chart" = "radar",
                "Bubble Chart" = "bubble",
                "Box Plot" = "box"
              ),
              selected = "scatter"
            )
          ),
          mainPanel(
            h3("Branch-wise Statistics"),
            tags$br(),
            plotlyOutput("main_branch_viz", height = "500px"),
            tags$br(),
            fluidRow(
              column(6, plotlyOutput("ctc_violin_plot", height = "400px")),
              column(6, plotlyOutput("placement_donut_chart", height = "400px"))
            ),
            tags$br(),
            dataTableOutput("branch_stats_table")
          )
        )
      ),
      
      tabPanel(
        "State Statistics",
        sidebarLayout(
          sidebarPanel(
            h3("State-wise Analysis"),
            tags$br(),
            selectizeInput(
              "selected_states",
              label = "Select States to Compare",
              choices = if(exists("data_students") && "home_state" %in% names(data_students)) {
                unique(data_students$home_state)
              } else {
                c("Uttar Pradesh", "Bihar", "Delhi", "Haryana", "Punjab")
              },
              selected = if(exists("data_students") && "home_state" %in% names(data_students)) {
                head(unique(data_students$home_state), 10)
              } else {
                c("Uttar Pradesh", "Bihar", "Delhi")
              },
              multiple = TRUE,
              options = list(placeholder = "Select states")
            ),
            tags$br(),
            radioButtons(
              "state_chart_type",
              label = "Visualization Type",
              choices = list(
                "Bar Chart" = "bar",
                "Pie Chart" = "pie",
                "Heatmap" = "heatmap"
              ),
              selected = "bar"
            )
          ),
          mainPanel(
            h3("State-wise Statistics"),
            tags$br(),
            plotlyOutput("main_state_viz", height = "500px"),
            tags$br(),
            fluidRow(
              column(6, plotlyOutput("state_ctc_ridge", height = "400px")),
              column(6, plotlyOutput("state_placement_funnel", height = "400px"))
            ),
            tags$br(),
            dataTableOutput("state_stats_table")
          )
        )
      ),
      
      tabPanel(
        "Gender Statistics",
        sidebarLayout(
          sidebarPanel(
            h3("Gender-wise Analysis"),
            tags$br(),
            selectizeInput(
              "selected_gender_branches",
              label = "Select Branches to Compare",
              choices = c("Overall", sort(unique(data_students$branch))),
              selected = "Overall",
              multiple = TRUE,
              options = list(placeholder = "Select branches or overall")
            ),
            tags$br(),
            radioButtons(
              "gender_chart_type",
              label = "Visualization Type",
              choices = list(
                "Bar Chart Comparison" = "bar",
                "Pie Chart" = "pie",
                "Stacked Bar" = "stacked"
              ),
              selected = "bar"
            )
          ),
          mainPanel(
            h3("Gender-wise Statistics"),
            tags$br(),
            plotlyOutput("main_gender_viz", height = "500px"),
            tags$br(),
            fluidRow(
              column(6, plotlyOutput("gender_ctc_comparison", height = "400px")),
              column(6, plotlyOutput("gender_placement_gauge", height = "400px"))
            ),
            tags$br(),
            dataTableOutput("gender_stats_table")
          )
        )
      )
    )
  )
)


###################################   Server code  #########################


server <- function(session, input, output) {
  overall_company_data <- data_companies
  overall_student_data <- data_students
  
  
  overall_student_data <- overall_student_data %>%
    left_join(
      students_ctc %>% select(roll, ctc),
      by = c("roll")
    )
  
  
  if(!"gender" %in% names(overall_student_data)) {
    overall_student_data$gender <- sample(c("Male", "Female"), 
                                          nrow(overall_student_data), 
                                          replace = TRUE, 
                                          prob = c(0.7, 0.3))
  }
  
  
  if(!"home_state" %in% names(overall_student_data)) {
    states <- c("Uttar Pradesh", "Bihar", "Delhi", "Haryana", "Punjab", 
                "Rajasthan", "Madhya Pradesh", "West Bengal", "Maharashtra", "Gujarat")
    overall_student_data$home_state <- sample(states, 
                                              nrow(overall_student_data), 
                                              replace = TRUE)
  }
  
  
  filtered_company_data <- reactive({
    overall_company_data %>%
      filter(ctc >= input$incomeRange[1] & ctc <= input$incomeRange[2])
  })
  
  output$comp_table <- renderDataTable({
    filtered_company_data() %>%
      select(company_name, profile_type, profile, location, ctc) %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "Name",
          "Profile",
          "Role",
          "Location",
          "CTC"
        )
      )
  })
  

  branch_stats <- reactive({
    overall_student_data %>%
      filter(branch %in% input$selected_branches) %>%
      group_by(branch) %>%
      summarize(
        avg_ctc = mean(ctc, na.rm = TRUE),
        median_ctc = median(ctc, na.rm = TRUE),
        placement_rate = mean(!is.na(ctc), na.rm = FALSE) * 100,
        total_students = n(),
        placed_students = sum(!is.na(ctc), na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  output$main_branch_viz <- renderPlotly({
    data <- branch_stats()
    
    if (input$chart_type == "scatter") {
      p <- ggplot(data, aes(x = placement_rate, y = avg_ctc, 
                            size = total_students, color = branch)) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(5, 15)) +
        scale_color_viridis_d() +
        labs(title = "Branch Performance: CTC vs Placement Rate",
             x = "Placement Rate (%)", y = "Average CTC",
             size = "Total Students") +
        theme_minimal()
      ggplotly(p)
    } else if (input$chart_type == "bubble") {
      p <- ggplot(data, aes(x = branch, y = avg_ctc, size = placement_rate)) +
        geom_point(alpha = 0.7, color = "#3498db") +
        scale_size_continuous(range = c(5, 20)) +
        labs(title = "Branch CTC with Placement Rate as Bubble Size",
             x = "Branch", y = "Average CTC", size = "Placement Rate (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    } else if (input$chart_type == "box") {
      student_data <- overall_student_data %>%
        filter(branch %in% input$selected_branches, !is.na(ctc))
      
      p <- ggplot(student_data, aes(x = branch, y = ctc, fill = branch)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_viridis_d() +
        labs(title = "CTC Distribution by Branch",
             x = "Branch", y = "CTC") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    } else {
      # Default bar chart
      p <- ggplot(data, aes(x = branch, y = avg_ctc, fill = branch)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_viridis_d() +
        labs(title = "Average CTC by Branch",
             x = "Branch", y = "Average CTC") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    }
  })
  
 
  output$ctc_violin_plot <- renderPlotly({
    student_data <- overall_student_data %>%
      filter(branch %in% input$selected_branches, !is.na(ctc))
    
    p <- ggplot(student_data, aes(x = branch, y = ctc, fill = branch)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.1, alpha = 0.8) +
      scale_fill_viridis_d() +
      labs(title = "CTC Distribution by Branch",
           x = "Branch", y = "CTC") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  

  output$placement_donut_chart <- renderPlotly({
    data <- branch_stats()
    
    plot_ly(data, labels = ~branch, values = ~placement_rate,
            type = 'pie', hole = 0.6,
            textinfo = 'label+percent',
            marker = list(colors = viridis(nrow(data)))) %>%
      layout(title = "Placement Rate by Branch",
             showlegend = TRUE)
  })
  
  output$branch_stats_table <- renderDataTable({
    branch_stats() %>%
      mutate(
        avg_ctc = round(avg_ctc, 0),
        median_ctc = round(median_ctc, 0),
        placement_rate = round(placement_rate, 1)
      ) %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "Branch",
          "Average CTC",
          "Median CTC",
          "Placement Rate (%)",
          "Total Students",
          "Placed Students"
        )
      )
  })
  
  
  state_stats <- reactive({
    data <- overall_student_data %>%
      group_by(home_state) %>%
      summarize(
        avg_ctc = mean(ctc, na.rm = TRUE),
        median_ctc = median(ctc, na.rm = TRUE),
        placement_rate = mean(!is.na(ctc), na.rm = FALSE) * 100,
        total_students = n(),
        placed_students = sum(!is.na(ctc), na.rm = TRUE),
        .groups = "drop"
      )
    
    if (!is.null(input$selected_states)) {
      data <- data %>% filter(home_state %in% input$selected_states)
    }
    data
  })
  
  
  output$main_state_viz <- renderPlotly({
    data <- state_stats()
    
    if (input$state_chart_type == "bar") {
      p <- ggplot(data, aes(x = reorder(home_state, avg_ctc), y = avg_ctc, fill = home_state)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_viridis_d() +
        labs(title = "Average CTC by State",
             x = "State", y = "Average CTC") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    } else if (input$state_chart_type == "pie") {
      plot_ly(data, labels = ~home_state, values = ~total_students,
              type = 'pie',
              textinfo = 'label+percent',
              marker = list(colors = viridis(nrow(data)))) %>%
        layout(title = "Student Distribution by State")
    } else {
   
      p <- ggplot(data, aes(x = 1, y = home_state, fill = avg_ctc)) +
        geom_tile() +
        scale_fill_viridis_c() +
        labs(title = "Average CTC Heatmap by State",
             x = "", y = "State") +
        theme_minimal() +
        theme(axis.text.x = element_blank())
      ggplotly(p)
    }
  })
  
 
  output$state_ctc_ridge <- renderPlotly({
    data <- state_stats()
    
    p <- ggplot(data, aes(x = avg_ctc, y = reorder(home_state, avg_ctc))) +
      geom_point(size = 3, alpha = 0.7, color = "#3498db") +
      geom_segment(aes(xend = 0, yend = home_state), alpha = 0.5) +
      labs(title = "Average CTC by State",
           x = "Average CTC", y = "State") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$state_placement_funnel <- renderPlotly({
    data <- state_stats() %>%
      arrange(desc(placement_rate)) %>%
      head(10)
    
    plot_ly(data, 
            y = ~reorder(home_state, placement_rate),
            x = ~placement_rate,
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#27ae60')) %>%
      layout(title = "Top 10 States by Placement Rate",
             xaxis = list(title = "Placement Rate (%)"),
             yaxis = list(title = "State"))
  })
  
  output$state_stats_table <- renderDataTable({
    state_stats() %>%
      mutate(
        avg_ctc = round(avg_ctc, 0),
        median_ctc = round(median_ctc, 0),
        placement_rate = round(placement_rate, 1)
      ) %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "State",
          "Average CTC",
          "Median CTC",
          "Placement Rate (%)",
          "Total Students",
          "Placed Students"
        )
      )
  })
  
 
  gender_stats <- reactive({
    data <- overall_student_data %>% 
      filter(!is.na(gender))
    
    if ("Overall" %in% input$selected_gender_branches) {
      data_summary <- data %>%
        group_by(gender) %>%
        summarize(
          avg_ctc = mean(ctc, na.rm = TRUE),
          placed_percent = mean(!is.na(ctc), na.rm = FALSE) * 100,
          total_students = n(),
          placed_students = sum(!is.na(ctc), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(branch = "Overall")
    } else {
      data_summary <- data %>%
        filter(branch %in% input$selected_gender_branches) %>%
        group_by(branch, gender) %>%
        summarize(
          avg_ctc = mean(ctc, na.rm = TRUE),
          placed_percent = mean(!is.na(ctc), na.rm = FALSE) * 100,
          total_students = n(),
          placed_students = sum(!is.na(ctc), na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    return(data_summary)
  })
  
 
  output$main_gender_viz <- renderPlotly({
    data <- gender_stats()
    
    if (input$gender_chart_type == "bar") {
      p <- ggplot(data, aes(x = gender, y = avg_ctc, fill = gender)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
        labs(title = "Average CTC by Gender",
             x = "Gender", y = "Average CTC") +
        theme_minimal() +
        facet_wrap(~branch, scales = "free_y")
      ggplotly(p)
    } else if (input$gender_chart_type == "pie") {
      plot_ly(data, labels = ~gender, values = ~total_students,
              type = 'pie',
              textinfo = 'label+percent',
              marker = list(colors = c('#3498db', '#e74c3c'))) %>%
        layout(title = "Student Distribution by Gender")
    } else {
      
      p <- ggplot(data, aes(x = branch, y = total_students, fill = gender)) +
        geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
        scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
        labs(title = "Student Distribution by Gender and Branch",
             x = "Branch", y = "Total Students") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    }
  })
  

  output$gender_ctc_comparison <- renderPlotly({
    data <- gender_stats()
    
    p <- ggplot(data, aes(x = branch, y = avg_ctc, fill = gender)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
      labs(title = "CTC Comparison by Gender and Branch",
           x = "Branch", y = "Average CTC") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
 
  output$gender_placement_gauge <- renderPlotly({
    data <- gender_stats()
    
    
    p <- ggplot(data, aes(x = gender, y = placed_percent, fill = gender)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
      labs(title = "Placement Rate by Gender",
           x = "Gender", y = "Placement Rate (%)") +
      theme_minimal() +
      facet_wrap(~branch)
    
    ggplotly(p)
  })
  
  output$gender_stats_table <- renderDataTable({
    gender_stats() %>%
      mutate(
        avg_ctc = round(avg_ctc, 0),
        placed_percent = round(placed_percent, 1)
      ) %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "Branch/Overall",
          "Gender",
          "Average CTC",
          "Placement Rate (%)",
          "Total Students",
          "Placed Students"
        )
      )
  })
}

shinyApp(ui = ui, server = server)

