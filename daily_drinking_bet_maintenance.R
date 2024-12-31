library(data.table)
library(httr2)
library(lubridate)
library(purrr)
library(suncalc)
source("twilio_verbs.R")


#system("scp -r ~/Dropbox/mc/Personal/twilio_no_drinking_app/daily_drinking_bet_maintenance.R cawthm@indra-investors:/home/cawthm/indra-investors/plumr/daily_drinking_bet_maintenance.R")


# Ensure consistent handling of quotes and newlines
quotes <- readr::read_rds("stoic_quotes.rds")
player_db <- fread("player_db.csv", colClasses = list(character = "phone"), encoding = "UTF-8")


# Select a quote for the day

if (player_db$date_start[[1]] == Sys.Date()) {
    my_sampled_row <- 140
} else if (Sys.Date() %in% as.Date(c("2024-12-25", "2025-12-25", "2026-12-25", "2027-12-25", "2028-12-25"))) {
    my_sampled_row <- 170
} else {
    my_sampled_row <- sample(seq_len(nrow(quotes)), size = 1, prob = quotes$prop)
}

# Print the sampled row number
print(my_sampled_row)

# Reduce the probability of that quote being selected
quotes$prop[my_sampled_row] <- quotes$prop[my_sampled_row] * 0.05

# Add to player_db
player_db[, sampled_row := my_sampled_row]
 # If it's Saturday or Sunday, credit drink_balance +1
  if (wday(Sys.Date()) %in% c(6,7,1)) {
    # First identify who will get a foregone drink
    player_db[, will_get_foregone := drink_balance >= 6]
    
    # Check for mulligan awards BEFORE incrementing foregone_drinks
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
    
    # Commented out notification code preserved for reference
    #player_db[new_mulligans > 0, {
    #    msg <- sprintf("Congratulations! You've been awarded %d new mulligan(s). Total foregone drinks: %d", new_mulligans, foregone_drinks)
    #    send_text(phone, msg)
    #}, by = .(phone, initials)]
}

# Save the updated data back to CSV files
readr::write_csv(player_db, "player_db.csv")
readr::write_rds(quotes, "stoic_quotes.rds")

# Figure out how many secs the day is changing

get_sec_change <- function() {
  df <- suncalc::getSunlightTimes(date = Sys.Date() + lubridate::days(-1:0),
                                  lat = 36.064983,
                                  lon = -94.149184, tz = "America/Chicago") |>
    dplyr::mutate(daylight_length_secs = difftime(sunset, sunrise, units = "secs"),
           change_in_secs = daylight_length_secs - dplyr::lag(daylight_length_secs))

 n <- df$change_in_secs[[2]]
 
 paste0(ifelse(n >= 0, "+", ""), n, " secs daylight ", "\u25B3")
 
}

# Format and send the daily message
daily_msg <- paste0(format_quote(quotes[my_sampled_row, ]), "\n\n", "Reply MENU for actions.", "\n\n", get_sec_change())

# Send the message only to the specific player(s) with initials "MC"
#map(player_db[initials == "MC",]$phone, .f = send_text, daily_msg)


purrr::map(player_db[]$phone, .f = send_text, daily_msg)

