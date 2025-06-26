best <- function(state, outcome) {
  # Read data from the CSV file with all columns as character strings
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define which columns correspond to which outcomes
  outcome_columns <- c("heart attack" = 11,
                       "heart failure" = 17,
                       "pneumonia" = 23)
  
  # Check if the state argument is valid (exists in the data)
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  # Check if the outcome argument is valid
  if (!outcome %in% names(outcome_columns)) {
    stop("invalid outcome")
  }
  
  # Get the numeric column index for the requested outcome
  outcome_col <- outcome_columns[[outcome]]
  
  # Filter data for the selected state
  state_data <- data[data$State == state, ]
  
  # Select hospital name and outcome columns
  hospital_data <- state_data[, c(2, outcome_col)]
  names(hospital_data) <- c("Hospital", "Rate")
  
  # Convert the Rate column to numeric, NAs introduced if values are non-numeric
  hospital_data$Rate <- as.numeric(hospital_data$Rate)
  
  # Remove rows where Rate is NA (missing data)
  hospital_data <- hospital_data[!is.na(hospital_data$Rate), ]
  
  # Order by Rate ascending, and then Hospital name alphabetically to break ties
  hospital_data <- hospital_data[order(hospital_data$Rate, hospital_data$Hospital), ]
  
  # Return the hospital name with the lowest 30-day mortality rate
  return(hospital_data$Hospital[1])
}
