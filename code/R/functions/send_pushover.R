library(pushoverr)
library(webshot)

send_pushover <- function(script="code/R/other/pushover_trial.R"){
  
  # Get timestamp and computer name
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  computer_name <- Sys.info()["nodename"]
  
  # Send notification using Pushover API
  pushover(
    message = paste0("Your code started running (", script, ") at ", timestamp)
  )
  
  result <- tryCatch({
    # Source the script
    source(script)
  }, error = function(e) {
    error_message <<- paste0("Error: ", e$message)
  })
  
  # Check if the script ran successfully
  if (exists("error_message")) {
    message <- paste0("An error occurred while running your script (", script, "): ", error_message)
  } else {
    message <- paste0("Work done successfully (", script, ")")
  }
 
  # Send notification using Pushover API
  pushover(
    message = message
  )
}

