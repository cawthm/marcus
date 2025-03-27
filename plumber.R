# plumber.R

#* @apiTitle Twilio Webhook Receiver
library(plumber)
library(data.table)

source("twilio_verbs.R")

#* Log all incoming data and parse JSON automatically
#* @param req The request object from Plumber
#* @post /receive-twilio-webhook
function(req) {
    data <- req$body
    if (is.null(data)) {
        req$status <- 400
        return(list(error = "No data provided"))
    }

    # Log the raw incoming data as JSON
    log_entry <- jsonlite::toJSON(data, pretty = TRUE, auto_unbox = TRUE)
    writeLines(paste(Sys.time(), "Received data:", log_entry), con = "webhook_raw_logs.txt")

    # Parse JSON data into a data table
    log_entry2 <- jsonlite::fromJSON(log_entry) %>%
        data.table::as.data.table()
    setnames(log_entry2, tolower(names(log_entry2)))
    log_entry2[, time := Sys.time()]
    log_entry2 <- log_entry2[, .(time, from, body, smsmessagesid)]
    log_entry2[, from := as.character(from)]

    # Parse the message body
    parsed_result <- parser(log_entry2$body)
    log_entry2[, verb := parsed_result[[1]]]  # First element is always the verb
    
    # Handle the count/data differently based on verb type
    if (log_entry2$verb == "ADD_QUOTE") {
        log_entry2[, count := list(parsed_result[[2]])]  # For ADD_QUOTE, second element is a list with quote and author
    } else {
        log_entry2[, count := parsed_result[[2]]]  # For other verbs, second element is numeric or NA
    }

    # Append the parsed data to the CSV file
    fwrite(log_entry2, "received_text_db.csv", append = TRUE)

    # Process the incoming data
    tryCatch({
        dispatch_function(log_entry2)
    }, error = function(e) {
        writeLines(paste(Sys.time(), "Error:", e$message), con = "webhook_raw_logs.txt")
        req$status <- 500
        return(list(error = e$message))
    })

    return(list(status = "success"))
} 