library(tidyverse)
library(lubridate)
library(purrr)
fixtures <- read_csv("Snooker 18-19.csv") %>%
  filter(!is.na(Division)) %>%
  rename(division = Division, home_team = `Home Team`, away_team = `Away Team`,
         date = Date, time = Time, venue = Venue) %>%
  mutate(home_team = str_replace_all(home_team, "  ", " "),
         away_team = str_replace_all(away_team, "  ", " ")) %>%
  select(division, home_team, away_team, date, time, venue)
write_csv(fixtures, "BHDBSL Snooker 18-19.csv")

create_single_ics <- function(fixtures_df, chosen_team, home_away_suffix) {
  my_fixtures <- fixtures_df %>%
    filter(home_team == chosen_team | away_team == chosen_team)
  
  my_fixtures <- my_fixtures %>%
    mutate(UID = replicate(nrow(my_fixtures), ic_guid()),
           SUMMARY = if_else(home_team == chosen_team,
                             paste0(away_team,
                                    if_else(home_away_suffix,
                                           " (H)", "")),
                             paste0(home_team,
                                    if_else(home_away_suffix,
                                           " (A)", ""))),
           DTSTART = as_datetime(dmy_hms(paste(date, time))),
           DTEND = as_datetime(ymd_hms(DTSTART) + hours(3)),
           LOCATION = venue) %>%
    select(UID, SUMMARY, DTSTART, DTEND, LOCATION)
  
  ic <- ical(my_fixtures, "DTSTAMP")
  ic_write(ic, paste0(chosen_team, ".ics"))  
}

all_teams <- unique(c(fixtures$home_team, fixtures$away_team))
pmap(list(list(fixtures), all_teams, rep(TRUE, length(all_teams))),
     create_single_ics)
