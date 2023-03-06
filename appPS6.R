library(shiny)
library(dplyr)
library(ggplot2)

# Load the absenteeism dataset
absenteeism <- read.csv("absenteeism.csv")
names(absenteeism) <- make.names(names(absenteeism))


# Define the UI
ui <- navbarPage(
  "Absenteeism at Work",
  tabsetPanel(
    tabPanel("About",
             h3("Absenteeism at work"),
             p("The database was created with records of absenteeism at work from July 2007 to July 2010 at a courier company in Brazil."),
             p("Creators original owner and donors: Andrea Martiniano (1), Ricardo Pinto Ferreira (2), and Renato Jose Sassi (3)."),
             p("E-mail address:"),
             p("andrea.martiniano'@'gmail.com (1) - PhD student;"),
             p("log.kasparov'@'gmail.com (2) - PhD student;"),
             p("sassi'@'uni9.pro.br (3) - Prof. Doctor."),
             p("Universidade Nove de Julho - Postgraduate Program in Informatics and Knowledge Management."),
             p("Address: Rua Vergueiro, 235/249 Liberdade, Sao Paulo, SP, Brazil. Zip code: 01504-001."),
             p("Website: http://www.uninove.br/curso/informatica-e-gestao-do-conhecimento/"),
             p("Relevant Papers:"),
             p("Martiniano, A., Ferreira, R. P., Sassi, R. J., & Affonso, C. (2012). Application of a neuro fuzzy network in prediction of absenteeism at work. In Information Systems and Technologies (CISTI), 7th Iberian Conference on (pp. 1-4). IEEE."),
             p("Citation Requests / Acknowledgements:"),
             p("Martiniano, A., Ferreira, R. P., Sassi, R. J., & Affonso, C. (2012). Application of a neuro fuzzy network in prediction of absenteeism at work. In Information Systems and Technologies (CISTI), 7th Iberian Conference on (pp. 1-4). IEEE.")
    ),
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 h3("Select Variable"),
                 selectInput(inputId = "variable", label = NULL,
                             choices = c("ID", "Reason_for_absence", "Month_of_absence", "Day_of_the_week", "Seasons", "Transportation_expense", "Distance_from_Residence_to_Work", "Service_time", "Age", "`Work_load_Average.day`", "Hit_target", "Disciplinary_failure", "Education", "Son", "Social.drinker", "Social.smoker", "Pet", "Weight", "Height", "Body_mass_index")),
                 sliderInput(inputId = "font_size", label = "Font Size of X-axis",
                             min = 1, max = 20, value = 12)
               ),
               mainPanel(
                 plotOutput("plot", height = "700px", width = "1000px"),
                 verbatimTextOutput("plot_info")
               )
             )
    ),
    tabPanel("Table",
             h3("Average Absenteeism Time by Age Group"),
             sliderInput(inputId = "age_range", label = "Select age range:",
                         min = 27, max = 58, value = c(27, 58), step = 1),
             tableOutput("table"),
             verbatimTextOutput("age_range_output")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # About tab - display general information
  output$info <- renderTable({
    summary(absenteeism)
  })
  
  # Plots tab - plot variable vs absenteeism time
  output$plot <- renderPlot({
    plot_data <- absenteeism %>% 
      select(!!sym(input$variable), Absenteeism_time_in_hours)
    
    ggplot(plot_data, aes(x = !!sym(input$variable), y = Absenteeism_time_in_hours)) +
      geom_point() +
      ggtitle(paste0("Total Absences by ", input$variable)) +
      xlab("Variable") +
      ylab("Total Absences (in hours)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = input$font_size),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$plot_info <- renderPrint({
    plot_data <- absenteeism %>% 
      select(!!sym(input$variable), Absenteeism_time_in_hours)
    
    paste0(input$variable, ": Amount of data = ", nrow(plot_data), 
           ", x range = [", min(plot_data[[1]]), ", ", max(plot_data[[1]]), 
           "], y range = [", min(plot_data$Absenteeism_time_in_hours), ", ", 
           max(plot_data$Absenteeism_time_in_hours), "]")
  })
  
  # Table tab - display average absenteeism time by age group
  output$table <- renderTable({
    absenteeism %>%
      filter(Age >= input$age_range[1] & Age <= input$age_range[2]) %>%
      group_by(Age) %>%
      summarise(avg_absenteeism_time = mean(Absenteeism_time_in_hours))
  })
  
  output$age_range_output <- renderPrint({
    age_range <- input$age_range
    avg_time <- absenteeism %>%
      filter(Age >= age_range[1] & Age <= age_range[2]) %>%
      summarise(avg_absenteeism_time = mean(Absenteeism_time_in_hours)) %>%
      pull(avg_absenteeism_time)
    
    paste("Age Range:", age_range[1], "-", age_range[2], ", Average time taken off for this range:", avg_time)
  })
  
}

# Run the app
shinyApp(ui, server)
