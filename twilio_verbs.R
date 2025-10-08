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
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net", "drink_balance", "big_goal")
    ))

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
          "Drink Balance","\n", format_dataframe_to_string(player_db[,c("initials", "drink_balance")]),"\n","\n",
          "Net","\n", format_dataframe_to_string(player_db[,c("initials", "net")]),"\n","\n",
          "Big Goal","\n", format_dataframe_to_string(player_db[,c("initials", "big_goal")]),"\n","\n",
          "Mulligan Balance","\n", format_dataframe_to_string(player_db[,c("initials", "mulligan_balance")]))
   send_text(df$from, msg)
}

#STATS_verb(log_entry2)

MENU_verb <- function(df) {
    msg <- paste0("Valid replies:","\n","\n",
                  "DRINK n or DRINKS n","\n","(to log drink(s), where n is the number of drinks)","\n","\n",
                  "HEALTH n","\n","(to log health activity, where n is the number of units)","\n","\n",
                  "GOALED","\n","(to log achieving a big goal, +100 health points)","\n","\n",
                  "MULLIGAN","\n","(to use a mulligan)","\n","\n",
                  "STATS","\n","(to see everyone's current standings)","\n","\n",
                  "QUOTE","\n","(to see today's quote)","\n","\n",
                  'ADD_QUOTE "Your quote here" Author Name',"\n","(to add a new quote to the database)")

    send_text(df$from, msg)
}

#MENU_verb(log_entry2)

DRINK_verb <- function(df) {
    df <- as.data.table(df)
    
    # Validate input is not NA
    if (is.na(df$count) || tolower(df$count) == "na") {
        send_text(df$from, "Invalid entry. Please enter a valid number.")
        return()
    }
    
    # Make sure count is numeric and phone is character
    df[, `:=`(
        count = as.numeric(count),
        from = as.character(from)
    )]
    
    # Round down to nearest 0.5
    df[, count := floor(count * 2) / 2]

    # Load and prepare player database
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net", "drink_balance", "big_goal")
    ))
    player_db[, phone := as.character(phone)]
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
    
    ## increment drinks count, decrement drink balance, and update net
    player_db[phone == df$from, `:=`(
       drinks = drinks + df$count,
       drink_balance = drink_balance - df$count,
       net = health - (drinks + df$count)  # net = health - drinks
    )]

    # Save the updated database
    fwrite(player_db, "player_db.csv")
    
    # Prepare messages for all recipients
    to_numbers <- list(df$from)  # Start with sender
    messages <- list(paste0(player_db[phone == df$from,]$initials, " logged ", df$count, " drink(s)."))
    
    # Add other players
    other_phones <- player_db[phone != df$from,]$phone
    if (length(other_phones) > 0) {
        to_numbers <- c(to_numbers, as.list(other_phones))
        messages <- c(messages, 
                     replicate(length(other_phones),
                             paste0(player_db[phone == df$from,]$initials, " just logged ", df$count, " drink(s)."),
                             simplify = FALSE))
    }
    
    # Send all messages in one batch
    send_text(to_numbers, messages)
}

#DRINK_verb(log_entry2)

HEALTH_verb <- function(df) {
    df <- as.data.table(df)
    
    # Validate input is not NA
    if (is.na(df$count) || tolower(df$count) == "na") {
        send_text(df$from, "Invalid entry. Please enter a valid number.")
        return()
    }
    
    # Make sure count is numeric and phone is character
    df[, `:=`(
        count = as.numeric(count),
        from = as.character(from)
    )]

    # Load and prepare player database
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net", "drink_balance", "big_goal")
    ))
    player_db[, phone := as.character(phone)]
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
    
    ## Calculate drink balance increase from every 10 health points
    old_health <- player_db[phone == df$from, health]
    new_health <- old_health + df$count
    old_drink_tokens <- floor(old_health / 10)
    new_drink_tokens <- floor(new_health / 10)
    drink_balance_increase <- new_drink_tokens - old_drink_tokens

    ## increment health count, drink balance, and update net
    player_db[phone == df$from, `:=`(
       health = health + df$count,
       drink_balance = drink_balance + drink_balance_increase,
       net = (health + df$count) - drinks  # net = health - drinks
    )]

    # Save the updated database
    fwrite(player_db, "player_db.csv")
    
    # Prepare messages for all recipients
    to_numbers <- list(df$from)  # Start with sender
    messages <- list(paste0(player_db[phone == df$from,]$initials, " logged ", df$count, " health unit(s)."))
    
    # Add other players
    other_phones <- player_db[phone != df$from,]$phone
    if (length(other_phones) > 0) {
        to_numbers <- c(to_numbers, as.list(other_phones))
        messages <- c(messages, 
                     replicate(length(other_phones),
                             paste0(player_db[phone == df$from,]$initials, " just logged ", df$count, " health unit(s)."),
                             simplify = FALSE))
    }
    
    # Send all messages in one batch
    send_text(to_numbers, messages)
}

QUOTE_verb <- function(df) {
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net")
    ))
    quotes <- readr::read_rds("stoic_quotes.rds")
    
    # Get today's quote
    current_quote <- quotes[player_db$sampled_row[[1]],]
    send_text(df$from, format_quote(current_quote))
}

HAIKU_verb <- function(df) {
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net")
    ))
    quotes <- readr::read_rds("stoic_quotes.rds")
    send_text(df$from, quotes[player_db$sampled_row[[1]],]$Haiku)
}

HOGS_verb <- function(df) {
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net")
    ))
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

    # Send message to all players in one batch
    msg <- "WOO PIG- DRINK UP"
    send_text(player_db$phone, replicate(nrow(player_db), msg, simplify = FALSE))
}

BUY_verb <- function(df) {
    df <- as.data.table(df)
    df[, count := as.integer(count)]
    
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net")
    ))
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
    
    # Increment drinks_consumed only for sender
    player_db[phone == df$from, `:=`(
       drinks_consumed = drinks_consumed + df$count
    )]
    
    fwrite(player_db, "player_db.csv")
    
    # Calculate amount owed
    amount_owed <- df$count * 10
    
    # Prepare messages for all recipients
    to_numbers <- list(df$from)  # Start with sender
    messages <- list(paste0("You bought ", df$count, " drink(s), logged to your drinks consumed."))
    
    # Add other players
    other_phones <- player_db[phone != df$from,]$phone
    if (length(other_phones) > 0) {
        to_numbers <- c(to_numbers, as.list(other_phones))
        messages <- c(messages, 
                     replicate(length(other_phones),
                             paste0(player_db[phone == df$from,]$initials, " just bought ", df$count, 
                                   " drink(s). He owes you $", amount_owed, "."),
                             simplify = FALSE))
    }
    
    # Send all messages in one batch
    send_text(to_numbers, messages)
}

GOALED_verb <- function(df) {
    # Load and prepare player database
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net", "drink_balance", "big_goal")
    ))
    player_db[, phone := as.character(phone)]
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]

    ## Award 100 health points and increment big_goal
    player_db[phone == df$from, `:=`(
       health = health + 100,
       big_goal = big_goal + 1,
       net = (health + 100) - drinks  # net = health - drinks
    )]

    # Save the updated database
    fwrite(player_db, "player_db.csv")

    # Prepare messages for all recipients
    to_numbers <- list(df$from)  # Start with sender
    messages <- list(paste0(player_db[phone == df$from,]$initials, " achieved a big goal! +100 health points."))

    # Add other players
    other_phones <- player_db[phone != df$from,]$phone
    if (length(other_phones) > 0) {
        to_numbers <- c(to_numbers, as.list(other_phones))
        messages <- c(messages,
                     replicate(length(other_phones),
                             paste0(player_db[phone == df$from,]$initials, " just achieved a big goal! +100 health points."),
                             simplify = FALSE))
    }

    # Send all messages in one batch
    send_text(to_numbers, messages)
}

MULLIGAN_verb <- function(df) {
    # Load and prepare player database
    player_db <- data.table::fread("player_db.csv", colClasses = list(
        character = "phone",
        numeric = c("drinks", "health", "net", "drink_balance", "big_goal", "mulligan_balance", "mulligans_taken")
    ))
    player_db[, phone := as.character(phone)]
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]

    # Check if player has mulligans available
    if (player_db[phone == df$from,]$mulligan_balance <= 0) {
        msg <- paste0("You have no more mulligans to use. Negotiate with your bet mates or concede.")
        send_text(df$from, msg)
        return()
    }

    ## Decrement mulligan balance and increment mulligans taken
    player_db[phone == df$from, `:=`(
        mulligan_balance = mulligan_balance - 1,
        mulligans_taken = mulligans_taken + 1
    )]

    # Save the updated database
    fwrite(player_db, "player_db.csv")

    # Prepare messages for all recipients
    to_numbers <- list(df$from)  # Start with sender
    messages <- list(paste0(player_db[phone == df$from,]$initials, " used a mulligan."))

    # Add other players
    other_phones <- player_db[phone != df$from,]$phone
    if (length(other_phones) > 0) {
        to_numbers <- c(to_numbers, as.list(other_phones))
        messages <- c(messages,
                     replicate(length(other_phones),
                             paste0(player_db[phone == df$from,]$initials, " just used a mulligan."),
                             simplify = FALSE))
    }

    # Send all messages in one batch
    send_text(to_numbers, messages)
}

ADD_QUOTE_verb <- function(df) {
    print("=== ADD_QUOTE_VERB DEBUG ===")
    print("\nInput data frame:")
    print(df)
    
    # Load the current quotes database
    print("\nLoading quotes database...")
    quotes <- tryCatch({
        db <- readr::read_rds("stoic_quotes.rds")
        print("Current quotes database structure:")
        print(str(db))
        db
    }, error = function(e) {
        print("Error loading quotes database:")
        print(e$message)
        return(NULL)
    })
    
    print("\nValidating quote data...")
    # Parse the count string back into quote and author
    if (is.null(df$count) || !is.character(df$count)) {
        print("df$count is NULL or not a character")
        msg <- "Invalid format. Use: ADD_QUOTE \"Your quote here\" Author Name"
        send_text(df$from, msg)
        return(NULL)
    }
    
    # Extract quote and author from the count string
    # Format is: "quote" --author
    quote_pattern <- '^"([^"]+)"\\s*--\\s*(.+)$'
    matches <- regexec(quote_pattern, df$count)
    extracted <- regmatches(df$count, matches)[[1]]
    
    if (length(extracted) != 3) {
        print("Failed to extract quote and author from count string")
        msg <- "Invalid format. Use: ADD_QUOTE \"Your quote here\" Author Name"
        send_text(df$from, msg)
        return(NULL)
    }
    
    quote <- extracted[2]
    author <- trimws(extracted[3])
    
    print("\nCreating new quote entry...")
    # Create new quote entry
    new_quote <- data.table(
        quote = quote,
        author = author,
        work = NA_character_,
        Haiku = NA_character_,
        prop = 0.5
    )
    
    print("New quote entry:")
    print(new_quote)
    
    # Append to existing quotes
    print("\nAppending new quote to database...")
    quotes <- rbind(quotes, new_quote)
    
    # Save back to RDS
    print("\nSaving updated quotes database...")
    tryCatch({
        readr::write_rds(quotes, "stoic_quotes.rds")
        print("Successfully saved quotes database")
        
        # Prepare messages for all recipients
        to_numbers <- list(df$from)  # Start with sender
        messages <- list(paste0("Added new quote:\n\n",
                              '"', quote, '"\n\n',
                              " --", capitalize_title(author)))
        
        # Add other players
        player_db <- data.table::fread("player_db.csv", colClasses = list(
            character = "phone",
            numeric = c("drinks", "health", "net")
        ))
        other_phones <- player_db[phone != df$from,]$phone
        if (length(other_phones) > 0) {
            to_numbers <- c(to_numbers, as.list(other_phones))
            messages <- c(messages, 
                         replicate(length(other_phones),
                                 paste0(player_db[phone == df$from,]$initials, 
                                      " added a new quote to the database!"),
                                 simplify = FALSE))
        }
        
        # Send all messages in one batch
        send_text(to_numbers, messages)
        
    }, error = function(e) {
        print("Error saving quotes database:")
        print(e$message)
        msg <- "Error saving quote to database. Please try again."
        send_text(df$from, msg)
        return(NULL)
    })
    
    print("ADD_QUOTE_verb completed successfully")
}

######### Utility / helper functions below
# 1. send_text()
# 2. format_quote()
# 3. capitalize_title()
# 4. function_map()
# 5. dispatch_function()
# 6. parser()

send_text <- function(to_numbers, message_bodies) {
    # Base URL for Twilio API
    base_url <- sprintf("https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json", account_sid)
    
    # If single message/number, convert to vectors
    if (!is.list(to_numbers)) to_numbers <- list(to_numbers)
    if (!is.list(message_bodies)) message_bodies <- list(message_bodies)
    
    # Create a list to store responses
    responses <- list()
    
    # Send messages in parallel using future
    for (i in seq_along(to_numbers)) {
        req <- request(base_url) |>
            httr2::req_auth_basic(account_sid, auth_token) |>
            httr2::req_body_form(
                To = to_numbers[[i]],
                From = account_phone,
                Body = message_bodies[[i]]
            )
        
        # Perform the HTTP POST request
        responses[[i]] <- tryCatch({
            resp <- httr2::req_perform(req)
            if (resp$status_code == 201) {
                cat("Message sent successfully!\n")
            } else {
                cat("Failed to send message:\n")
                print(httr2::response_content(resp, as = "text"))
            }
            resp
        }, error = function(e) {
            cat("Error sending message:", e$message, "\n")
            NULL
        })
    }
    
    return(responses)
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
    "ADD_QUOTE" = ADD_QUOTE_verb,
    "GOALED" = GOALED_verb,
    "MULLIGAN" = MULLIGAN_verb
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
    print("=== PARSER DEBUG ===")
    print("Raw input string:")
    print(string)
    
    # Special handling for ADD_QUOTE which has a different format
    if (grepl("^ADD_QUOTE\\s+", string, ignore.case = TRUE)) {
        print("\nDetected ADD_QUOTE command")
        # Extract everything after ADD_QUOTE
        content <- sub("^ADD_QUOTE\\s+", "", string)
        print("\nContent after ADD_QUOTE removal:")
        print(content)
        
        # Convert curly quotes to straight quotes for easier parsing
        content <- gsub("[\u201C\u201D]", '"', content)
        print("\nContent after quote conversion:")
        print(content)
        
        # Simple pattern: everything between quotes is the quote, rest is author
        quote_pattern <- '^"([^"]+)"\\s+(.+)$'
        matches <- regexec(quote_pattern, content)
        extracted <- regmatches(content, matches)[[1]]
        
        print("\nRegex matches:")
        print(extracted)
        
        if (length(extracted) == 3) {  # Full match + 2 capture groups
            quote <- extracted[2]  # First capture group is the quote
            author <- trimws(extracted[3])  # Second capture group is the author
            
            print("\nExtracted quote:")
            print(quote)
            print("\nExtracted author:")
            print(author)
            
            if (nchar(quote) > 0 && nchar(author) > 0) {
                print("\nValidation passed, returning quote data")
                return(list("ADD_QUOTE", list(quote = quote, author = author)))
            }
        }
        print("\nQuote format validation failed")
        return(list("ADD_QUOTE", NA))
    }

    # Regular verb pattern for other commands
    pattern <- "(?i)\\b(STATS|MENU|DRINKS?|HEALTH|QUOTE|GOALED|MULLIGAN)\\b(?:\\s+(\\d+(?:\\.\\d+)?)?)?"

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



