# Parse JSON data into a data table
log_entry2 <- jsonlite::fromJSON(log_entry) %>%
    data.table::as.data.table()
setnames(log_entry2, tolower(names(log_entry2)))
log_entry2[, time := Sys.time()]
log_entry2 <- log_entry2[, .(time, from, body, smsmessagesid)]
log_entry2 <- log_entry2[, from := as.character(from)]

# Parse the message body
parsed_result <- parser(log_entry2$body)
log_entry2[, verb := parsed_result$verb]
log_entry2[, count := parsed_result$count]

# Append the parsed data to the CSV file
fwrite(log_entry2, "received_text_db.csv", append = TRUE)

# Process the incoming data 