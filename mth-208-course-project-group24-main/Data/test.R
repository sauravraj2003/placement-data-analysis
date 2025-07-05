##########################################
####   Main Libraries                 ####
##########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(kableExtra)
library(ggthemes)
library(plotly)

library(rsconnect)
library(shinythemes)
library(shinyWidgets)

# ------------------
# Main title section
# ------------------
setwd("/home/atulya/Desktop/MTH208/mth-208-course-project-group24/ShinyApp/g24")  
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
      
      # Companies Tab
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
      
      # Students Tab
      tabPanel(
        "Students",
        sidebarLayout(
          sidebarPanel(
            h3("Filter Students"),
            tags$br(),
            selectInput(
              "branch",
              label = "Select Branch",
              choices = c("All", sort(unique(data_students$branch))),
              selected = "All"
            ),
            selectInput(
              "gender",
              label = "Select Gender",
              choices = c("All", unique(data_students$gender)),
              selected = "All"
            ),
            selectInput(
              "home_state",
              label = "Select Home State",
              choices = c("All", sort(unique(data_students$home_state))),
              selected = "All"
            ),
            tags$br(),
            
            # New Checkbox for "Placed" Filter
            checkboxInput(
              "placed_filter",
              label = "Show Only Placed Students",
              value = FALSE
            ),
            tags$br(),
            actionButton("actionDT_stud", "Filter", class = "btn btn-warning")
          ),
          mainPanel(
            h3("Students"),
            tags$br(),
            dataTableOutput("stud_table"),
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
              choices = unique(data_students$branch),
              selected = c("BT-CSE"),
              multiple = TRUE,
              options = list(placeholder = 'Select branches')
            ),
            tags$br()
          ),
          mainPanel(
            h3("Branch-wise Statistics"),
            tags$br(),
            plotlyOutput("ctc_bar_plot"),
            tags$br(),
            plotlyOutput("placement_rate_plot"),
            tags$br(),
            dataTableOutput("branch_stats_table")
          )
        )
      )

    )
  )
)


##########################################
####   Attaching datasets             ####
##########################################

setwd("/home/atulya/Desktop/MTH208/mth-208-course-project-group24/ShinyApp/g24")  
load("companies.Rdata")
load("new_students.Rdata")
students_ctc <- as_tibble(read.csv("students_correlated.csv"))
data_companies <- companies2
data_students <- students

## Setting datatables view
opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(
    targets = c(1, 6), searchable = FALSE
  ))
)


##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {

  overall_company_data <- data_companies 
  overall_student_data <- data_students 
  
  # Joining CTC data with overall students data
  overall_student_data <- overall_student_data %>%
    left_join(
      students_ctc %>% select(roll, ctc),
      by = c("roll")
    )


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


  filtered_student_data <- reactive({
    data <- overall_student_data
    
    # Filter by branch if selected
    if (!is.null(input$branch) && input$branch != "All") {
      data <- data %>% filter(branch == input$branch)
    }
    
    # Filter by gender if selected and not "All"
    if (!is.null(input$gender) && input$gender != "All") {
      data <- data %>% filter(gender == input$gender)
    }
    
    # Filter by home state if selected
    if (!is.null(input$home_state) && input$home_state != "All") {
      data <- data %>% filter(home_state == input$home_state)
    }
    
    # Filter by placement status if the checkbox is checked
    if (input$placed_filter) {
      data <- data %>% filter(!is.na(company_name))  # Only show placed students
    }
    
    return(data)
  })

  output$stud_table <- renderDataTable({
    filtered_student_data() %>%  
      select(roll, name, company_name, profile, gender, branch, home_state, blood_group, ctc) %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "RollNo.",
          "Name",
          "Company",
          "Role",
          "Gender",
          "Branch",
          "Home State",
          "Blood Group",
          "CTC"
        )
      )
  })

  branch_stats <- reactive({
    data <- overall_student_data %>%
      group_by(branch) %>%
      summarize(
        avg_ctc = mean(ctc, na.rm = TRUE),
        median_ctc = median(ctc, na.rm = TRUE),
        placement_rate = mean(!is.na(company_name), na.rm = FALSE) * 100,
        total_students = n()
      )
    # Filter the data by selected branches
    if (!is.null(input$selected_branches)) {
      data <- data %>% filter(branch %in% input$selected_branches)
    }
    data
  })

output$ctc_bar_plot <- renderPlotly({
  plot <- ggplot(branch_stats(), aes(x = reorder(branch, avg_ctc), y = avg_ctc)) +
    geom_bar(stat = "identity", fill = "#2c3e50", width = 0.5) +  # Reduce width for more spacing
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9, hjust = 1),  # Adjust size for readability
      plot.margin = margin(t = 10, r = 10, b = 10, l = 40)  # Extra space for long names
    ) +
    labs(title = "Average CTC by Branch", x = "Branch", y = "Average CTC")
  
  ggplotly(plot)
})

output$placement_rate_plot <- renderPlotly({
  plot <- ggplot(branch_stats(), aes(x = reorder(branch, placement_rate), y = placement_rate)) +
    geom_bar(stat = "identity", fill = "#e67e22", width = 0.5) +  # Reduce width for spacing
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9, hjust = 1),  # Adjust size for readability
      plot.margin = margin(t = 10, r = 10, b = 10, l = 40)  # Extra space for long names
    ) +
    labs(title = "Placement Rate by Branch", x = "Branch", y = "Placement Rate (%)")
  
  ggplotly(plot)
})

  output$branch_stats_table <- renderDataTable({
    branch_stats() %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "Branch",
          "Average CTC",
          "Median CTC",
          "Placement Rate (%)",
          "Total Students"
        )
      )
  })


}

shinyApp(ui = ui, server = server)