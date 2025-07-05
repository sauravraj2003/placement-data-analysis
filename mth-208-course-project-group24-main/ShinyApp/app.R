##########################################
####   Main Libraries                 ####
##########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
# library(kableExtra)
library(ggthemes)
library(plotly)

library(rsconnect)
library(shinythemes)
library(shinyWidgets)

library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)





# ------------------
# Main title section
# ------------------
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
              choices = sort(unique(data_students$branch)),
              selected = c("BT-CSE"),
              multiple = TRUE,
              options = list(placeholder = "Select branches")
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
              choices = unique(data_students$home_state),
              selected = unique(data_students$home_state),
              multiple = TRUE,
              options = list(placeholder = "Select states")
            ),
            tags$br()
          ),
          mainPanel(
            h3("State-wise Statistics"),
            tags$br(),
            plotlyOutput("ctc_state_plot"),
            tags$br(),
            plotlyOutput("placement_rate_state_plot"),
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
            tags$br()
          ),
          mainPanel(
            h4("Average CTC by Gender"),
            plotOutput("avg_ctc_gender_plot"),
            tags$br(),
            h4("Placement Rate by Gender"),
            plotOutput("placement_rate_gender_plot"),
            h3("Gender-wise Statistics"),
            tags$br(),
            dataTableOutput("gender_stats_table")
          )
        )
      )
    )
  )
)


##########################################
####   Attaching datasets             ####
##########################################

load("companies.Rdata")
load("new_students.Rdata")
students_ctc <- as_tibble(read.csv("students_correlated.csv"))
data_companies <- companies2
data_students <- students
data_companies <- data_companies[data_companies$company_name != "Da Vinci Derivatives B.V.", ]
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
      data <- data %>% filter(!is.na(company_name)) # Only show placed students
    }

    return(data)
  })

  output$stud_table <- renderDataTable({
    filtered_student_data() %>%
      select(roll, name, company_name, profile, gender, branch, home_state, blood_group) %>%
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
          "Blood Group"
        )
      )
  })



  branch_stats <- reactive({
    overall_student_data %>%
      group_by(branch) %>%
      summarize(
        avg_ctc = mean(ctc, na.rm = TRUE),
        median_ctc = median(ctc, na.rm = TRUE),
        placement_rate = mean(!is.na(ctc), na.rm = FALSE) * 100,
        total_students = n(),
        .groups = "drop"
      ) %>%
      filter(branch %in% input$selected_branches)
  })

  output$ctc_bar_plot <- renderPlotly({
    plot <- ggplot(branch_stats(), aes(x = reorder(branch, avg_ctc), y = avg_ctc)) +
      geom_bar(stat = "identity", fill = "#2c3e50", width = 0.5) + # Reduce width for more spacing
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 9, hjust = 1), # Adjust size for readability
        plot.margin = margin(t = 10, r = 10, b = 10, l = 40) # Extra space for long names
      ) +
      labs(title = "Average CTC by Branch", x = "Branch", y = "Average CTC")

    ggplotly(plot)
  })

  output$placement_rate_plot <- renderPlotly({
    plot <- ggplot(branch_stats(), aes(x = reorder(branch, placement_rate), y = placement_rate)) +
      geom_bar(stat = "identity", fill = "#e67e22", width = 0.5) + # Reduce width for spacing
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 9, hjust = 1), # Adjust size for readability
        plot.margin = margin(t = 10, r = 10, b = 10, l = 40) # Extra space for long names
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


  state_stats <- reactive({
    data <- overall_student_data %>%
      group_by(home_state) %>%
      summarize(
        avg_ctc = mean(ctc, na.rm = TRUE),
        median_ctc = median(ctc, na.rm = TRUE),
        placement_rate = mean(!is.na(company_name), na.rm = FALSE) * 100,
        total_students = n()
      )
    # Filter the data by selected states
    if (!is.null(input$selected_states)) {
      data <- data %>% filter(home_state %in% input$selected_states)
    }
    data
  })

  output$ctc_state_plot <- renderPlotly({
    plot <- ggplot(state_stats(), aes(x = reorder(home_state, avg_ctc), y = avg_ctc)) +
      geom_bar(stat = "identity", fill = "#3498db", width = 0.5) + # Adjust width for spacing
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 9, hjust = 1), # Adjust size for readability
        plot.margin = margin(t = 10, r = 10, b = 10, l = 40) # Extra space for long names
      ) +
      labs(title = "Average CTC by State", x = "State", y = "Average CTC")

    ggplotly(plot)
  })

  output$placement_rate_state_plot <- renderPlotly({
    plot <- ggplot(state_stats(), aes(x = reorder(home_state, placement_rate), y = placement_rate)) +
      geom_bar(stat = "identity", fill = "#27ae60", width = 0.5) + # Adjust width for spacing
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 9, hjust = 1), # Adjust size for readability
        plot.margin = margin(t = 10, r = 10, b = 10, l = 40) # Extra space for long names
      ) +
      labs(title = "Placement Rate by State", x = "State", y = "Placement Rate (%)")

    ggplotly(plot)
  })

  output$state_stats_table <- renderDataTable({
    state_stats() %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "State",
          "Average CTC",
          "Median CTC",
          "Placement Rate (%)",
          "Total Students"
        )
      )
  })


  gender_stats <- reactive({
    data <- overall_student_data %>% filter(!is.na(gender))

    if ("Overall" %in% input$selected_gender_branches) {
      data_summary <- data %>%
        group_by(gender) %>%
        summarize(
          avg_ctc = mean(ctc, na.rm = TRUE),
          placed_percent = mean(!is.na(company_name)) * 100,
          total_students = n(),
          .groups = "drop"
        ) %>%
        mutate(branch = "Overall")
    } else {
      data_summary <- data %>%
        group_by(branch, gender) %>%
        summarize(
          avg_ctc = mean(ctc, na.rm = TRUE),
          placed_percent = mean(!is.na(ctc), na.rm = FALSE) * 100,
          total_students = n(),
          .groups = "drop"
        ) %>%
        filter(branch %in% input$selected_gender_branches)
    }

    return(data_summary)
  })

  # Render the gender statistics table
  output$gender_stats_table <- renderDataTable({
    gender_stats() %>%
      datatable(
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c(
          "Branch/Overall",
          "Gender",
          "Average CTC (Rupees)",
          "Placement Rate (%)",
          "Total Count"
        )
      )
  })

  output$placement_rate_gender_plot <- renderPlot({
    gender_data <- gender_stats()

    ggplot(gender_data, aes(x = gender, y = placed_percent, fill = gender)) +
      geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
      labs(
        title = "Placement Rate by Gender and Branch",
        x = "Gender",
        y = "Placement Rate (%)"
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("Male" = "#1E90FF", "Female" = "#FF69B4")) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      geom_text(aes(label = paste0(round(placed_percent, 1), "%")), vjust = -0.5, size = 4.5, color = "darkblue") +
      facet_wrap(~branch) # Facet by branch to create separate graphs for each branch
  })

  output$avg_ctc_gender_plot <- renderPlot({
    gender_data <- gender_stats()

    ggplot(gender_data, aes(x = gender, y = avg_ctc, fill = gender)) +
      geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
      labs(
        title = "Average CTC by Gender and Branch",
        x = "Gender",
        y = "Average CTC (Rupees)"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("Male" = "#1E90FF", "Female" = "#FF69B4")) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      geom_text(aes(label = scales::comma(round(avg_ctc, 1))), vjust = -0.5, size = 4.5, color = "darkblue") +
      facet_wrap(~branch)
  })
}

shinyApp(ui = ui, server = server)
