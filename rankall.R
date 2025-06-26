rankall <- function(outcome, num = "best") {
  # Read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define valid outcomes and corresponding column numbers
  outcome_columns <- c("heart attack" = 11,
                       "heart failure" = 17,
                       "pneumonia" = 23)
  
  # Check for valid outcome input
  if (!outcome %in% names(outcome_columns)) {
    stop("invalid outcome")
  }
  
  # Get the correct column index for the outcome
  outcome_col <- outcome_columns[[outcome]]
  
  # Convert the outcome column to numeric (suppress NA warnings)
  data[, outcome_col] <- suppressWarnings(as.numeric(data[, outcome_col]))
  
  # Get all unique state abbreviations, sorted alphabetically
  states <- sort(unique(data$State))
  
  # Create an empty list to hold results
  result <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  
  # Loop through each state
  for (state in states) {
    # Subset data for that state
    state_data <- data[data$State == state, c(2, outcome_col)]
    names(state_data) <- c("Hospital", "Rate")
    
    # Remove rows with missing Rate
    state_data <- state_data[!is.na(state_data$Rate), ]
    
    # Order by Rate, then Hospital name
    state_data <- state_data[order(state_data$Rate, state_data$Hospital), ]
    
    # Determine which row to return
    if (num == "best") {
      hospital_name <- state_data$Hospital[1]
    } else if (num == "worst") {
      hospital_name <- state_data$Hospital[nrow(state_data)]
    } else if (is.numeric(num)) {
      if (num > nrow(state_data)) {
        hospital_name <- NA
      } else {
        hospital_name <- state_data$Hospital[num]
      }
    } else {
      stop("invalid num")
    }
    
    # Add result to the data frame
    result <- rbind(result, data.frame(hospital = hospital_name, state = state, stringsAsFactors = FALSE))
  }
  
  # Return the full result
  return(result)
}
