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

    bet_days <- difftime(Sys.Date(), as.Date(player_db$date_start[[1]]))
    drink_bal <- player_db[,c("initials", "drink_balance")]
    drinks_consumed <- player_db[,c("initials", "drinks_consumed")]
    mul_balance <- player_db[,c("initials", "mulligan_balance")]
    foregone_drinks <- player_db[,c("initials", "foregone_drinks")]
    current_stakes <- min(player_db$start_stakes[[1]], player_db$start_stakes[[1]] - bet_days * player_db$bet_decrement[[1]])
    current_stakes <- max(player_db$bet_min, current_stakes)
    ##

    msg <- paste("STATS","\n","\n",
          "Bet duration","\n", as.integer(bet_days)," days","\n","\n",
          "Drink balance","\n",format_dataframe_to_string(drink_bal),"\n","\n",
          "Drinks consumed","\n",format_dataframe_to_string(drinks_consumed),"\n","\n",
          "Remaining mulligans","\n",format_dataframe_to_string(mul_balance),"\n","\n",
          "Foregone drinks","\n",format_dataframe_to_string(foregone_drinks),"\n","\n",
          "Bet balance","\n",scales::dollar_format(accuracy = 1)(current_stakes))
   send_text(df$from, msg)
}

#STATS_verb(log_entry2)

MENU_verb <- function(df) {
    ## say something useful
    msg <- paste0("Valid replies:","\n","MENU (show the menu verbs)","\n","\n",
                  "DRINK n","\n","(to log drink(s), where n is the number of drinks you've had. Can be done any number of times, but each text is additive, eg texting DRINK 1, followed by DRINKS 3 a few minutes later will debit you 4 total drinks)","\n","\n",
                  "MULLIGAN","\n","(when you wish to log a mulligan)","\n","\n",
                  "STATS","\n","(to see everyone's current standings / bet terms)","\n","\n",
                  "HAIKU","\n","(to see a haiku interpretation of today's quote)","\n","\n",
                  "BUY n","\n","(to buy n drinks, which costs $20 per drink, $10 to each bet mate)","\n","\n",
                  "UNSUBSCRIBE","\n","(does nothing: there's no way out)")

    ## send it
    send_text(df$from, msg)
}

#MENU_verb(log_entry2)

DRINK_verb <- function(df) {
    df <- as.data.table(df)
    df[, count := as.integer(count)]
    #df[, from = as.character(from)]

    player_db <- data.table::fread("player_db.csv", colClasses = list(character = "phone"))
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
    #work <- data.table::copy(player_db)
    ## decrement drinks db
    player_db[phone == df$from, `:=`(
       drink_balance = drink_balance - df$count,
       drinks_consumed = drinks_consumed + df$count
    )]

    str_output <- capture.output(str(player_db))
    log_message <- paste(Sys.time(), "Error:", paste(str_output, collapse = "\n"))
    writeLines(log_message, con = "drink_verb_log.txt")

    str_output <- capture.output(str(df))
    log_message <- paste(Sys.time(), "Error:", paste(str_output, collapse = "\n"))
    writeLines(log_message, con = "drink_verb2_log.txt")

    #fwrite(player_db[phone == df$from], "player_db_DELETE.csv")
    # #
    bet_days <- difftime(Sys.Date(), as.Date("2024-07-01"))
    current_stakes <- min(player_db$start_stakes, player_db$start_stakes - bet_days * player_db$bet_decrement)
    current_stakes <- max(player_db$bet_min, current_stakes)
    # save our work


    if (player_db[phone == df$from,]$drink_balance < 0 ) {

        msg <- paste0("You have lost the bet and owe ", scales::dollar_format()(current_stakes))
        send_text(df$from, msg)
    } else {
        fwrite(player_db, "player_db.csv")
        # send stats
        STATS_verb(df)
        # let_other_players_know
        msg2 <- paste0(player_db[phone == df$from,]$initial, " just logged ", df$count, " drink(s).")
        purrr::map(player_db[phone != df$from,]$phone, send_text, msg2)
    }
}

#DRINK_verb(log_entry2)

MULLIGAN_verb <- function(df) {
    df <- as.data.table(df)
    player_db <- data.table::fread("player_db.csv", colClasses = list(character = "phone"))
    player_db[, phone := ifelse(grepl("^\\+", phone), phone, paste0("+", phone))]
        ## decrement drinks db
    player_db[phone == df$from, `:=`(
        mulligan_balance = mulligan_balance - 1,
        mulligans_taken = mulligans_taken + 1
    )]

    # save our work

    if (player_db[phone == df$from,]$mulligan_balance < 0 ) {
        msg <- paste0("You have no more mullis to use.  Negotiate with your bet mates or concede.")
        send_text(df$from, msg)
    } else {
        fwrite(player_db, "player_db.csv")
        # let_other_players_know
        msg2 <- paste0(player_db[phone == df$from,]$initial, " just took a mulligan.")
        purrr::map(player_db[phone != df$from,]$phone, send_text, msg2)

    }
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

    # Increment drink_balance and handle foregone_drinks
    player_db[drink_balance >= 6, foregone_drinks := foregone_drinks + 1]
    player_db[, drink_balance := pmin(drink_balance + 1, 6)]
    
    # Check for mulligan awards based on foregone_drinks
    player_db[foregone_drinks %% 3 == 0 & foregone_drinks > 0 & drink_balance >= 6, new_mulligans := 1]
    player_db[new_mulligans > 0, `:=`(
        mulligan_balance = mulligan_balance + new_mulligans
        # Remove the line that was resetting foregone_drinks
    )]
    
    # Notify players who received new mulligans
    #player_db[new_mulligans > 0, {
    #    msg <- sprintf("Congratulations! You've been awarded %d new mulligan(s). Total foregone drinks: %d", new_mulligans, foregone_drinks)
    #    send_text(phone, msg)

    #}, by = .(phone, initials)]

    player_db[, new_mulligans := NULL]

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
        "STATS" = STATS_verb,
        "MENU" = MENU_verb,
        "DRINK" = DRINK_verb,   
        "BUY" = BUY_verb,
        "MULLIGAN" = MULLIGAN_verb,
        "HAIKU" = HAIKU_verb,
        "HOGS" = HOGS_verb
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
    # Updated regex pattern to include HOGS
    pattern <- "(?i)\\b(STATS|MENU|DRINKS?|MULLIGAN|HAIKU|HOGS|BUY)\\b(?:\\s+(\\d+))?"

    # Apply the regex pattern
    matches <- regmatches(string, gregexpr(pattern, string, perl = TRUE))

    # Simplify extraction and assignment
    if (length(matches[[1]]) == 0) {
        # Return NA if no matches are found
        return(c(NA, NA))
    } else {
        # Initialize result with NA for the number
        result <- list(NA, NA)

        # Process the first match (assuming the first match is what we want)
        parts <- strsplit(matches[[1]][1], "\\s+")[[1]]
        if (length(parts) == 2) {
            # Normalize "DRINKS" to "DRINK" if followed by a number
            result[[1]] <- ifelse(tolower(parts[1]) %in% c("drink", "drinks"), "DRINK", toupper(parts[1]))
            result[[2]] <- parts[2]
        } else {
            result[[1]] <- toupper(parts[1])
            result[[2]] <- NA  # Explicitly set NA if no number is present
        }

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



