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

    # Parse JSON data into a data table
    log_entry2 <- jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE)) %>%
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
        # For ADD_QUOTE, store the quote and author as a string in the count column
        quote_data <- parsed_result[[2]]
        if (!is.null(quote_data)) {
            log_entry2[, count := paste0('"', quote_data$quote, '" --', quote_data$author)]
        } else {
            log_entry2[, count := NA_character_]
        }
    } else {
        log_entry2[, count := parsed_result[[2]]]  # For other verbs, second element is numeric or NA
    }

    # Process the incoming data
    tryCatch({
        dispatch_function(log_entry2)
    }, error = function(e) {
        req$status <- 500
        return(list(error = e$message))
    })

    return(list(status = "success"))
} 