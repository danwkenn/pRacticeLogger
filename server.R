#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("functions.R")

library(shiny)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  message('Reading in log.')
  log_table <- reactiveVal(value = read_in_log(),label = "log_table")
  
  observeEvent(input$new_log_entry,
               {
                message("New log button clicked. Adding row to log_table.")
                other_details <- other_details_formatter(
                   type = input$type,
                   exercise_type = input$exercise_type,
                   details_list = list(
                     musical_mode = input$musical_mode,
                     minor_type = input$minor_type,
                     exercise_articulation = input$exercise_articulation,
                     piece_speed_bpm = input$piece_speed_bpm
                   )
                 )
                log_table(add_row(
                  log_table(),
                  type = input$type,
                  name_sel_box = input$name,
                  new_name = input$new_name,
                  new_composer = input$new_piece_composer,
                  new_year = input$new_piece_year,
                  tonic = paste0(input$exercise_note,input$exercise_note2),
                  exercise_type = input$exercise_type,
                  percentage_mastered = input$percentage_mastered,
                  other_details = other_details
                  ))
                save_log(log_table = log_table())
               }
  )
  
  selected_piece <- reactive({
    selections <- paste0(
           log_table()$piece_name, " (",
           log_table()$composer,", ",
           log_table()$composition_year,")")
    matching_selections <- which(selections == input$name)
    if(length(matching_selections) > 0){
      max(matching_selections)
    }else{
      integer()
    }
  })
  
  selected_exercise <- reactive({
    matches <- which(
        log_table()$exercise_type == input$exercise_type &
        log_table()$tonic == paste0(input$exercise_note,input$exercise_note2)
      )
    if(length(matches) > 0){
      max(matches)
    }else{
      integer()
    }
  })
  
  # Current piece names:
  piece_names_to_display <- reactive({
    temp_table <- log_table() %>% filter(type == "piece")
    sort(unique(paste0(temp_table$piece_name, " (",
                       temp_table$composer,", ",
                       temp_table$composition_year,")")))
  })
  
  # Piece name selection:
  name_selection <- reactive({
    message("Name selection changed.")
    if(input$type == "piece"){
      if(nrow(log_table()) > 0){
        unique(c("[New piece]",piece_names_to_display()))
      }else{
        "[New piece]"
      }
    }
  })
  
  # Exercise selection
  exercise_selection <- reactive({
    message("Exercise selection updated.")
    if(input$type == "exercise"){
      c("arpeggio","scale")
    }
  })
  
  output$name_sel_box = renderUI({
  if(input$type == "piece"){
    message("Rendering piece name selection box.")
    selectInput('name', paste0('Select ',input$type,":"), name_selection())
    }else{
    NULL
    }
  })
  
  output$exercise_sel_box = renderUI({
    if(input$type == "exercise"){
      message("Rendering exercise selection box.")
      selectInput('exercise_type', paste0('Select exercise type:'), exercise_selection())
    }else{
      NULL
    }
  })
  
  output$new_piece <- renderUI({
    if(input$type == "piece"){
      if(input$name == "[New piece]"){
        textInput(inputId = "new_name",value = "",label = "Name:")
      }
    }
  })
  
  output$exercise_note1 <- renderUI({
    if(input$type == "exercise"){
      selectInput("exercise_note",label = "Note:",selected = "C",choices = LETTERS[1:7])
    }
  })
  
  output$exercise_note2 <- renderUI({
    if(input$type == "exercise"){
      radioButtons("exercise_note2",label = NULL,
                   selected = "", inline = TRUE,
                   choiceValues = c("","-Flat","-Sharp"),
                   choiceNames = c("Natural","♭","♯"))
    }
  })

output$exercise_articulation <- renderUI({
  if(input$type == "exercise"){
    radioButtons("exercise_articulation",label = "Articulation:",
                 selected = "", inline = TRUE,
                 choiceValues = c("legato","staccato","mixed"),
                 choiceNames = c("legato","staccato","mixed"))
  }
})
    
  output$new_piece_composer <- renderUI({
    if(input$type == "piece"){
      if(input$name == "[New piece]"){
        textInput(inputId = "new_piece_composer",value = "",label = "Composer/Artist:")
      }
    }
  })
  
  output$new_piece_year <- renderUI({
    if(input$type == "piece"){
      if(input$name == "[New piece]"){
        textInput(inputId = "new_piece_year",value = "",label = "Year written:")
      }
    }
  })
  
  output$percentage_mastered <- renderUI({
    if(input$type == "piece"){
      # if(input$name != "[New piece]"){
      # 
      # }
      numericInput(inputId = "percentage_mastered",
                   value = log_table()$percentage_mastered[selected_piece()],
                   min = 0, max = 0,
                   label = "Percentage mastered:")
    }else{
      numericInput(inputId = "percentage_mastered",
                   value = log_table()$percentage_mastered[selected_exercise()],
                   min = 0, max = 0,
                   label = "Percentage mastered:")
    }
  })
  
  output$musical_mode <- renderUI({
    if(input$type == "exercise"){
      radioButtons(
        inputId = "musical_mode",label = "Mode:",selected = "major",
        choiceValues = c("major","minor"),choiceNames = c("Major","Minor"),
        inline = TRUE)
    }
  })
  
  output$minor_type <- renderUI({
    if(input$type == "exercise"){
      if(input$exercise_type == "scale"){
        if(input$musical_mode == "minor"){
      radioButtons(
        inputId = "minor_type",label = NULL,selected = "natural",
        choiceValues = c("natural","harmonic","melodic"),
        choiceNames = c("Natural","Harmonic","Melodic"),
        inline = TRUE)
        }
      }
    }
  })
  
  output$piece_speed_bpm <- renderUI({
    if(input$type == "piece"){
      numericInput(inputId = "piece_speed_bpm",
                   value = NA,
                   min = 0, max = Inf,
                   label = "Speed (BPM):")
    }
  })
  
  output$log_table_display_piece <- DT::renderDataTable({
    log_table() %>% 
      filter(type == "piece") %>% arrange(desc(id)) %>% 
      select(
        Date = date,Piece = piece_name,Composer = composer,
        Year = composition_year,`% Mastered` = percentage_mastered)})
  output$log_table_display_exercise <- DT::renderDataTable({
    log_table() %>% 
      filter(type == "exercise") %>% arrange(desc(id)) %>% 
      select(
        Date = date,Type = exercise_type,
        Tonic = tonic,`% Mastered` = percentage_mastered
      )})
  
  output$progress_plot <- renderPlot(
    {
    message("Re-rendering plot.")
    ggplot(data = log_table() %>% filter(type == "piece") %>%
             filter(as.numeric(Sys.Date() - date) < as.numeric(input$plot_date_window)) %>%
             mutate(piece_name_display = paste0(piece_name," (",composer,", ",composition_year,")")),
           aes(y = percentage_mastered,x = date,
               group = piece_name_display,colour = piece_name_display)) + geom_point() + geom_line() +
      labs(title = "Piece mastery",x = "Date",y = "Percentage mastered",
           colour = "Piece") + 
      theme_minimal()
    }
  )
})
