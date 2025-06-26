rankhospital <- function(state, outcome, num = "best") {
  # Read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define valid outcomes and their column indices
  outcome_columns <- c("heart attack" = 11,
                       "heart failure" = 17,
                       "pneumonia" = 23)
  
  # Validate state
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  # Validate outcome
  if (!outcome %in% names(outcome_columns)) {
    stop("invalid outcome")
  }
  
  # Get the column number for the requested outcome
  outcome_col <- outcome_columns[[outcome]]
  
  # Filter data for the requested state
  state_data <- data[data$State == state, ]
  
  # Select hospital name and outcome columns
  hospital_data <- state_data[, c(2, outcome_col)]
  names(hospital_data) <- c("Hospital", "Rate")
  
  # Convert Rate column to numeric
  hospital_data$Rate <- as.numeric(hospital_data$Rate)
  
  # Remove rows with missing outcome data
  hospital_data <- hospital_data[!is.na(hospital_data$Rate), ]
  
  # Sort by Rate ascending, then Hospital name alphabetically
  hospital_data <- hospital_data[order(hospital_data$Rate, hospital_data$Hospital), ]
  
  # Handle "best", "worst", or numeric rank
  if (num == "best") {
    return(hospital_data$Hospital[1])
  } else if (num == "worst") {
    return(hospital_data$Hospital[nrow(hospital_data)])
  } else if (is.numeric(num)) {
    if (num > nrow(hospital_data)) {
      return(NA)
    } else {
      return(hospital_data$Hospital[num])
    }
  } else {
    stop("invalid num")
  }
}
