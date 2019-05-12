library(tidyverse)
library(lubridate)
library(purrr)
fixtures <- read_csv("Snooker 18-19.csv") %>%
  filter(!is.na(Division)) %>%
  rename(division = Division, home_team = `Home Team`, away_team = `Away Team`,
         date = Date, time = Time, venue = Venue) %>%
  select(division, home_team, away_team, date, time, venue)
write_csv(fixtures, "BHDBSL Snooker 18-19.csv")

my_team <- "Castle Club  A"
my_fixtures <- fixtures %>%
  filter(home_team == my_team | away_team == my_team) %>%
  mutate(SUMMARY = ifelse(home_team == my_team,
                          paste(away_team, "(H)"),
                          paste(home_team, "(A)")),
         DTSTART = as_datetime(dmy_hms(paste(date, time))),
         DTEND = as_datetime(ymd_hms(DTSTART) + hours(3)),
         LOCATION = venue) %>%
  select(SUMMARY, DTSTART, DTEND, LOCATION)

add_uid <- function(SUMMARY, DTSTART, DTEND, LOCATION) {
  data.frame(UID = ic_guid(),
             SUMMARY = SUMMARY,
             DTSTART = DTSTART,
             DTEND = DTEND,
             LOCATION = LOCATION,
             stringsAsFactors = FALSE)
}

my_fixtures <- pmap_dfr(my_fixtures, add_uid)

ic <- ical(my_fixtures)
ic_write(ic, "my_fixtures.ics")
