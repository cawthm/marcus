## sends this file
#system("scp -r ~/Dropbox/mc/Personal/twilio_no_drinking_app/twilio_verbs.R cawthm@indra-investors:/home/cawthm/indra-investors/plumr/twilio_verbs.R")

## sends the stoic quotes file
#system("scp -r ~/Dropbox/mc/Personal/twilio_no_drinking_app/stoic_quotes.csv cawthm@indra-investors:/home/cawthm/indra-investors/plumr/stoic_quotes.csv")

library(readr)
library(httr2)
library(data.table)

twilio_tokens <- readr::read_rds("twilio_tokens.rds")

auth_token <- twilio_tokens$TWILIO_API_TOKEN
account_sid <- twilio_tokens$TWILIO_API_SID
account_phone <- twilio_tokens$TWILIO_PHONE

STATS_verb <- function(df) {
    ##load data
    player_db <- data.table::fread("player_db.csv", colClasses = list(character = "phone"))

    # Calculate days remaining (100-day bet)
    start_date <- as.Date(player_db$start_date[[1]])
    days_until_start <- as.integer(difftime(start_date, Sys.Date(), units = "days"))
    days_remaining <- if (days_until_start > 0) {
        100  # Bet hasn't started yet
    } else {
        max(0, 100 + days_until_start)  # days_until_start is negative here
    }

    msg <- paste("STATS","\n","\n",
          "Days remaining: ", days_remaining, "\n", "\n",
          "Drinks","\n", format_dataframe_to_string(player_db[,c("initials", "drinks")]),"\n","\n",
          "Health","\n", format_dataframe_to_string(player_db[,c("initials", "health")]),"\n","\n",
          "Net","\n", format_dataframe_to_string(player_db[,c("initials", "net")]))
   send_text(df$from, msg)
}

#STATS_verb(log_entry2)

MENU_verb <- function(df) {
    msg <- paste0("Valid replies:","\n","\n",
                  "DRINK n or DRINKS n","\n","(to log drink(s), where n is the number of drinks)","\n","\n",
                  "HEALTH n","\n","(to log health activity, where n is the number of units)","\n","\n",
                  "STATS","\n","(to see everyone's current standings)","\n","\n",
                  "QUOTE","\n","(to see today's quote)","\n","\n",
                  'ADD_QUOTE "Your quote here" Author Name',"\n","(to add a new quote to the database)")

    send_text(df$from, msg)
}

#MENU_verb(log_entry2)

DRINK_verb <- function(df) {
    df <- as.data.table(df)
    
    # Make sure count is numeric and phone is character
    df[, `:=`(
        count = as.numeric(count),
        from = as.character(from)
    )]

    # Debug: Print working directory
    print("Current working directory:")
    print(getwd())

    # Load and prepare player database
    print("Reading player_db.csv...")
    player_db <- data.table::fread("player_db.csv")
    print("Initial player_db contents:")
    print(player_db)
    
    # Convert phone numbers to character and ensure + prefix
    player_db[, phone := as.character(phone)]
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
    
    print("Phone numbers after conversion:")
    print(data.frame(
        db_phone = player_db$phone,
        incoming_phone = df$from,
        match = player_db$phone == df$from
    ))
    
    ## increment drinks count and update net
    print(paste("Updating drinks for phone:", df$from))
    player_db[phone == df$from, `:=`(
       drinks = drinks + df$count,
       net = health - (drinks + df$count)  # net = health - drinks
    )]

    print("Player_db after update:")
    print(player_db)

    # Save the updated database
    print("Writing to player_db.csv...")
    tryCatch({
        fwrite(player_db, "player_db.csv")
        print("Write completed. Checking file...")
        # Verify the write by reading back
        verification <- fread("player_db.csv")
        print("File contents after write:")
        print(verification)
    }, error = function(e) {
        print("Error writing to file:")
        print(e$message)
    })
    
    # Send confirmation
    msg <- paste0(player_db[phone == df$from,]$initials, " logged ", df$count, " drink(s).")
    send_text(df$from, msg)
    
    # Let other players know
    msg2 <- paste0(player_db[phone == df$from,]$initials, " just logged ", df$count, " drink(s).")
    purrr::map(player_db[phone != df$from,]$phone, send_text, msg2)
}

#DRINK_verb(log_entry2)

HEALTH_verb <- function(df) {
    df <- as.data.table(df)
    
    # Make sure count is numeric and phone is character
    df[, `:=`(
        count = as.numeric(count),
        from = as.character(from)
    )]

    # Debug: Print working directory
    print("Current working directory:")
    print(getwd())

    # Load and prepare player database
    print("Reading player_db.csv...")
    player_db <- data.table::fread("player_db.csv")
    print("Initial player_db contents:")
    print(player_db)
    
    # Convert phone numbers to character and ensure + prefix
    player_db[, phone := as.character(phone)]
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
    
    print("Phone numbers after conversion:")
    print(data.frame(
        db_phone = player_db$phone,
        incoming_phone = df$from,
        match = player_db$phone == df$from
    ))
    
    ## increment health count and update net
    print(paste("Updating health for phone:", df$from))
    player_db[phone == df$from, `:=`(
       health = health + df$count,
       net = (health + df$count) - drinks  # net = health - drinks
    )]

    print("Player_db after update:")
    print(player_db)

    # Save the updated database
    print("Writing to player_db.csv...")
    tryCatch({
        fwrite(player_db, "player_db.csv")
        print("Write completed. Checking file...")
        # Verify the write by reading back
        verification <- fread("player_db.csv")
        print("File contents after write:")
        print(verification)
    }, error = function(e) {
        print("Error writing to file:")
        print(e$message)
    })
    
    # Send confirmation
    msg <- paste0(player_db[phone == df$from,]$initials, " logged ", df$count, " health unit(s).")
    send_text(df$from, msg)
    
    # Let other players know
    msg2 <- paste0(player_db[phone == df$from,]$initials, " just logged ", df$count, " health unit(s).")
    purrr::map(player_db[phone != df$from,]$phone, send_text, msg2)
}

QUOTE_verb <- function(df) {
    player_db <- data.table::fread("player_db.csv", colClasses = list(character = "phone"))
    quotes <- fread("stoic_quotes.csv")
    
    # Get today's quote
    current_quote <- quotes[player_db$sampled_row[[1]],]
    send_text(df$from, format_quote(current_quote))
}

HAIKU_verb <- function(df) {
    player_db <- data.table::fread("player_db.csv", colClasses = list(character = "phone"))
    quotes <- fread("stoic_quotes.csv")


    ## send Haiku
    #return("YOOO")
    send_text(df$from ,quotes[player_db$sampled_row[[1]],]$Haiku)


}

HOGS_verb <- function(df) {
    player_db <- data.table::fread("player_db.csv", colClasses = list(character = "phone"))
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]

    # First, identify who will get a foregone drink
    player_db[, will_get_foregone := drink_balance >= 6]
    
    # Check for mulligan awards BEFORE incrementing foregone_drinks
    # This way we only award when crossing the threshold
    old_foregone <- player_db$foregone_drinks
    new_foregone <- old_foregone + player_db$will_get_foregone
    player_db[, new_mulligans := floor(new_foregone/3) - floor(old_foregone/3)]
    
    # Update mulligans where awarded
    player_db[new_mulligans > 0, mulligan_balance := mulligan_balance + new_mulligans]
    
    # Now handle drink balance and foregone drinks
    player_db[will_get_foregone == TRUE, foregone_drinks := foregone_drinks + 1]
    player_db[, drink_balance := pmin(drink_balance + 1, 6)]
    
    # Cleanup
    player_db[, `:=`(
        will_get_foregone = NULL,
        new_mulligans = NULL
    )]

    # Save the updated player_db
    fwrite(player_db, "player_db.csv")

    # Send message to all players
    msg <- "WOO PIG- DRINK UP"
    purrr::map(player_db$phone, send_text, msg)
}

BUY_verb <- function(df) {
    df <- as.data.table(df)
    df[, count := as.integer(count)]
    
    player_db <- data.table::fread("player_db.csv", colClasses = list(character = "phone"))
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
    
    # Increment drinks_consumed only for sender
    player_db[phone == df$from, `:=`(
       drinks_consumed = drinks_consumed + df$count
    )]
    
    fwrite(player_db, "player_db.csv")
    
    # Calculate amount owed
    amount_owed <- df$count * 10
    
    # Let other players know they're owed money
    msg_others <- paste0(player_db[phone == df$from,]$initials, " just bought ", df$count, 
                   " drink(s). He owes you $", amount_owed, ".")
    purrr::map(player_db[phone != df$from,]$phone, send_text, msg_others)
    
    # Send confirmation to buyer (different message)
    msg_buyer <- paste0("You bought ", df$count, " drink(s), logged to your drinks consumed.")
    send_text(df$from, msg_buyer)
}

ADD_QUOTE_verb <- function(df) {
    # Load the current quotes database
    quotes <- readr::read_rds("stoic_quotes.rds")
    
    # Check if we have valid quote data
    if (is.na(df$count) || !is.list(df$count) || 
        is.null(df$count$quote) || is.null(df$count$author)) {
        msg <- "Invalid format. Use: ADD_QUOTE \"Your quote here\" Author Name"
        send_text(df$from, msg)
        return(NULL)
    }
    
    # Create new quote entry
    new_quote <- data.table(
        quote = df$count$quote,
        author = df$count$author,
        work = NA_character_,  # Set these as NA as per requirements
        Haiku = NA_character_,
        prop = 0.5  # Default probability same as others
    )
    
    # Append to existing quotes
    quotes <- rbind(quotes, new_quote)
    
    # Save back to RDS
    readr::write_rds(quotes, "stoic_quotes.rds")
    
    # Confirm to the user
    msg <- paste0("Added new quote:\n\n",
                 '"', df$count$quote, '"\n\n',
                 " --", capitalize_title(df$count$author))
    send_text(df$from, msg)
    
    # Let other players know
    msg2 <- paste0(player_db[phone == df$from,]$initials, 
                  " added a new quote to the database!")
    player_db <- data.table::fread("player_db.csv", 
                                  colClasses = list(character = "phone"))
    purrr::map(player_db[phone != df$from,]$phone, send_text, msg2)
}

######### Utility / helper functions below
# 1. send_text()
# 2. format_quote()
# 3. capitalize_title()
# 4. function_map()
# 5. dispatch_function()
# 6. parser()

send_text <- function(to_number, message_body) {
    # Base URL for Twilio API
    base_url <- sprintf("https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json", account_sid)
    ## get tokens
    #auth_token <- Sys.getenv("TWILIO_API_TOKEN")
    #account_sid <- Sys.getenv("TWILIO_API_SID")
    # Create the request object
    req <- request(base_url) |>
        httr2::req_auth_basic(account_sid, auth_token) |>
        httr2::req_body_form(
            To = to_number,
            From = account_phone,  # Replace this with your Twilio phone number
            Body = message_body
        )

    # Perform the HTTP POST request
    response <- httr2::req_perform(req)

    # Check if the request was successful
    if (response$status_code == 201) {  # 201 Created indicates success
        cat("Message sent successfully!\n")
    } else {
        cat("Failed to send message:\n")
        print(httr2::response_content(response, as = "text"))
    }

    return(response)
}

#send_text('+15015555802', message_body = "hello darkness my old friend")
######################################################

######################################################

format_quote <- function(quote_row) {
    paste0('"', quote_row$quote[[1]], '"',
           "\n","\n",
           " --", capitalize_title(gsub("_", " ", quote_row$author[[1]])))
}
#format_quote(1)
#send_quote('+19175551234', format_quote(1))


#######################################################

capitalize_title <- function(title) {
    # Words to keep in lowercase
    lowercase_words <- c("a", "the")

    # Split the title into words
    words <- strsplit(tolower(title), " ")[[1]]

    # Capitalize the first word and words not in the lowercase list
    words <- mapply(function(word, index) {
        if (index == 1 || !(word %in% lowercase_words)) {
            # Capitalize the first letter and concatenate with the rest of the word
            return(paste0(toupper(substring(word, 1, 1)), substring(word, 2)))
        } else {
            # Return the word unchanged
            return(word)
        }
    }, word = words, index = seq_along(words), SIMPLIFY = FALSE)

    # Combine the words back into a single string
    return(paste(words, collapse = " "))
}



#######################################################


function_map <- list(
    "DRINK" = DRINK_verb,
    "DRINKS" = DRINK_verb,
    "HEALTH" = HEALTH_verb,
    "MENU" = MENU_verb,
    "STATS" = STATS_verb,
    "QUOTE" = QUOTE_verb,
    "ADD_QUOTE" = ADD_QUOTE_verb
)

#######################################################
## Dispatch function

dispatch_function <- function(df, ...) {
    if(!is.null( function_map[[df$verb]])) {
        return(function_map[[df$verb]](df))
    } else {
        stop("Unknown command")
    }

}
#######################################################

parser <- function(string) {
    # Special handling for ADD_QUOTE which has a different format
    if (grepl("^ADD_QUOTE\\s+", string, ignore.case = TRUE)) {
        # Extract the quote (text between quotes) and author
        quote_match <- regexpr('"([^"]*)"\\s*(.+)?', string)
        if (quote_match > 0) {
            quote_text <- regmatches(string, quote_match)[[1]]
            # Extract the quote and author parts
            quote <- gsub('^"(.*)".*$', "\\1", quote_text)
            author <- trimws(gsub('^".*"\\s*(.*)$', "\\1", quote_text))
            return(list("ADD_QUOTE", list(quote = quote, author = author)))
        }
        return(list("ADD_QUOTE", NA))  # Return NA if quote format is invalid
    }

    # Regular verb pattern for other commands
    pattern <- "(?i)\\b(STATS|MENU|DRINKS?|HEALTH|QUOTE)\\b(?:\\s+(\\d+(?:\\.\\d+)?)?)?"

    # Apply the regex pattern
    matches <- regmatches(string, gregexpr(pattern, string, perl = TRUE))

    # Initialize result as list with two elements
    if (length(matches[[1]]) == 0) {
        # Return list with NAs if no matches are found
        return(list(NA, NA))
    } else {
        # Process the first match
        parts <- strsplit(matches[[1]][1], "\\s+")[[1]]
        
        # Initialize result
        result <- list(
            # First element is the verb (normalize DRINKS to DRINK if needed)
            ifelse(tolower(parts[1]) %in% c("drink", "drinks"), 
                  "DRINK", 
                  toupper(parts[1])),
            # Second element is the count (NA if not present)
            if (length(parts) == 2) as.numeric(parts[2]) else NA
        )
        
        return(result)
    }
}

format_dataframe_to_string <- function(df, delimiter = " | ") {
    # Check if the input is a data frame
    if (!is.data.frame(df)) {
        stop("Input must be a data frame")
    }

    # Convert each row to a string
    rows <- apply(df, 1, function(row) paste(row, collapse = delimiter))

    # Combine the rows into a single string with newlines
    result <- paste(rows, collapse = "\n")

    return(result)
}

# # Example usage
# df <- data.frame(
#     A = c(1, 2),
#     B = c(3, 4),
#     C = c(5, 6)
# )
#
# formatted_string <- format_dataframe_to_string(df)
# cat(formatted_string)



#parser("nonesense")



