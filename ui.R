#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Input Practice Progress!"),
  tabsetPanel(
  tabPanel("Log", fluid = TRUE,
           {
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "type", label = "Type", 
                  choices = c("piece","exercise"),
                  selected = "piece", 
                  multiple = FALSE, selectize = TRUE),
      uiOutput("name_sel_box"),
      uiOutput("exercise_sel_box"),
      uiOutput("new_piece"),
      uiOutput("new_piece_composer"),
      uiOutput("new_piece_year"),
      uiOutput("exercise_note1"),
      uiOutput("exercise_note2"),
      uiOutput("musical_mode"),
      uiOutput("minor_type"),
      uiOutput("exercise_articulation"),
      uiOutput("percentage_mastered"),
      uiOutput("piece_speed_bpm"),
      actionButton(inputId = "new_log_entry", label = "Add to log")
      ),
    # Show a plot of the generated distribution
    mainPanel(
      h3("Pieces:"),
      DT::dataTableOutput("log_table_display_piece"),
      h3("Exercises:"),
      DT::dataTableOutput("log_table_display_exercise")
    )
  )}),
  tabPanel("Plot", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               shiny::radioButtons(inputId = "plot_date_window",label = "View for:",
                                   choiceNames = c("last week","last month","last year","all time"),
                                   choiceValues = c(7,31,365,Inf))
             ),
             mainPanel(
               plotOutput("progress_plot")
             )
           )
           
  )
)
)
)

