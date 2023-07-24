# NYE WEEK DUMMIES
add_events <- function(data, config) {
  
  events <- config$data_config$events
  
  for (e in events) {
    var_w1 <- paste(e[1], "w1", sep = "_")
    var_w2 <- paste(e[1], "w2", sep = "_")
    var_w3 <- paste(e[1], "w3", sep = "_")
    var_w4 <- paste(e[1], "w4", sep = "_")
    
    data <- data %>%
      mutate(
        !!var_w1 := ifelse(date > as.Date(e[2]) &
                           date <= (as.Date(e[2]) + 7), 1, 0),
        !!var_w2 := ifelse(date > (as.Date(e[2]) + 7) &
                           date <= (as.Date(e[2]) + 14), 1, 0),
        !!var_w3 := ifelse(date > (as.Date(e[2]) + 14) &
                           date <= (as.Date(e[2]) + 21), 1, 0),
        !!var_w4 := ifelse(date > (as.Date(e[2]) + 21) &
                           date <= (as.Date(e[2]) + 28), 1, 0)
      )
    
    # add variables for Cologne only
    if (e[1] == "Col") {
      data <- data %>% 
        mutate(nye = ifelse(date > as.Date(e[2]), 1, 0),
               days_Col = as.numeric(date - as.Date(e[2])),
               Col_negw1 = ifelse(date > (as.Date(e[2]) - 7) &
                                    date <= e[2], 1, 0))
    }
  }
  
  return(data)
}
