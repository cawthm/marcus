library(data.table)
library(httr2)
library(lubridate)
library(purrr)
library(suncalc)
source("twilio_verbs.R")


#system("scp -r ~/Dropbox/mc/Personal/twilio_no_drinking_app/daily_drinking_bet_maintenance.R cawthm@indra-investors:/home/cawthm/indra-investors/plumr/daily_drinking_bet_maintenance.R")


# Ensure consistent handling of quotes and newlines
quotes <- readr::read_rds("stoic_quotes.rds")
player_db <- fread("player_db.csv", colClasses = list(
    character = "phone",
    numeric = c("drinks", "health", "net", "drink_balance", "big_goal")
), encoding = "UTF-8")


# Select a quote for the day

if (Sys.Date() %in% as.Date(c("2024-12-25", "2025-12-25", "2026-12-25", "2027-12-25", "2028-12-25"))) {
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

# Send the message to all players
purrr::map(player_db$phone, .f = send_text, daily_msg)

