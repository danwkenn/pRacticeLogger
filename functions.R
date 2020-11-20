library(tidyverse)
library(lubridate)

read_in_log <- function(){
  if(!file.exists("log.csv")){
    log_table <- tibble(
      id = numeric(),
      date = col_date(),
      type = factor(,levels = c("piece","exercise")),
      piece_name = character(),
      composer = character(),
      composition_year = integer(),
      exercise_type = character(),
      tonic = character(),
      percentage_mastered = numeric(),
      other_details = character()
      #notes = character(),
      #speed = numeric(),
      #reference_note = character()
    )
  }else{
   log_table <- read_csv(file = "log.csv",col_types = list(
      id = col_integer(),
      date = col_date(),
      type = col_factor(levels = c("piece","exercise")),
      piece_name = col_character(),
      composer = col_character(),
      composition_year = col_integer(),
      exercise_type = col_character(),
      tonic = col_character(),
      percentage_mastered = col_double(),
      other_details = col_character()
      #notes = col_character(),
      #speed = col_double(),
      #reference_note = col_character()
      )
      )
  }
  return(log_table)
}

save_log <- function(log_table){
  write_csv(x = log_table, path = "log.csv")
}

add_row <- function(
  log_table,
  type,
  name_sel_box,
  new_name,
  new_composer,
  new_year,
  tonic,
  exercise_type,
  percentage_mastered,
  other_details
){
  if(type == "piece"){
  if(name_sel_box == "[New piece]"){
    message("new piece, using new name box.")
    name <- new_name
    composer <- new_composer
    year <- new_year
  }else{
    
    # Find correct row:
    name_comp <- paste0(log_table$piece_name, " (",log_table$composer,", ",log_table$composition_year,")")
    row <- which(name_comp == name_sel_box)[1]
    name <- log_table$piece_name[row]
    composer <- log_table$composer[row]
    year <- log_table$composition_year[row]
  }
  new_row <- tibble(
    id = nrow(log_table)+1,
    date = Sys.Date(),
    type = factor(type,levels = c("piece","exercise")),
    piece_name = name,
    composer = composer,
    composition_year = year,
    exercise_type = as.character(NA),
    tonic = NA,
    percentage_mastered = percentage_mastered,
    other_details = other_details
  )
  log_table <- rbind(
    log_table,
    new_row
  )
  }else{
    new_row <- tibble(
      id = nrow(log_table)+1,
      date = Sys.Date(),
      type = factor(type,levels = c("piece","exercise")),
      piece_name = NA,
      composer = NA,
      composition_year = NA,
      exercise_type = exercise_type,
      tonic = tonic,
      percentage_mastered = percentage_mastered,
      other_details = other_details
    )
    
    log_table <- rbind(
      log_table,
      new_row
    )
  }
  log_table
}

other_details_formatter <- function(
  type,
  exercise_type,
  details_list
){
  message("Formatting log details.")
  if(type == "piece"){
    message("Parsing details for type...")
    other_details = c()
    if(!is.na(details_list$piece_speed_bpm)){
      other_details <- c(
        other_details,
        paste0(
          "{{piece_speed_bpm}={",details_list$piece_speed_bpm,"}}")
      )
    }
    other_details <- paste0(other_details,collapse = ",")
    return(other_details)
  }else{
    other_details = c()
      other_details <- c(
        other_details,
        paste0(
          "{{mode}=","{",details_list$musical_mode,"}}")
      )
    if(details_list[["musical_mode"]] == "minor" & exercise_type == "scale"){
      other_details <- c(
        other_details,
        paste0(
          "{{minor_type}={",details_list[["minor_type"]],"}}"
        )
      )
    }
      
      other_details <- c(
        other_details,
        paste0(
          "{{exercise_articulation}={",details_list[["exercise_articulation"]],"}}"
        )
      )
    
    other_details <- paste0(other_details,collapse = ",")
    return(other_details)
  }
}