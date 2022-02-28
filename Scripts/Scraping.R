message(paste0("Starting Script ", format.POSIXct(as.POSIXct(Sys.time(), tz = "GMT"), tz = "America/Chicago", usetz = TRUE)))
message("Loading Packages...")
# Load Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  charge_hist <- read_csv("Data/charge_history.csv") 
  activity_hist <- read_csv("Data/police_activity.csv") 
})
# Helper function to look into detail of arrest and extract date of birth, was arrested, and officer name
get_detail <- function(link){
  
  link <- read_html(link)
  
  tibble(dob = link %>% html_element("dd:nth-child(6)") %>% html_text(),
         jailed = link %>% html_element("dd:nth-child(8)") %>% html_text(),
         officer = link %>% html_element("dd:nth-child(14)") %>% html_text())
  
}

message(paste0("Scraping Charges... ", format.POSIXct(as.POSIXct(Sys.time(), tz = "GMT"), tz = "America/Chicago", usetz = TRUE)))
# Scrape arrests/charges and append to a table for future analysis
read_html("https://www.iowa-city.org/IcgovApps/Police/ArrestBlotter") %>% 
  html_element("table") %>% 
  html_table() %>% 
  rename_all(~tolower(.x) %>% str_replace_all(" ", "_")) %>% 
  bind_cols(tibble(details = read_html("https://www.iowa-city.org/IcgovApps/Police/ArrestBlotter") %>% 
                     html_elements(".body-content a") %>% 
                     html_attr("href"))) %>% 
  filter(!(case_number %in% charge_hist$case_number)) %>% 
  mutate(details = paste0("https://www.iowa-city.org", details),
         details = map(details, get_detail)) %>% 
  unnest(details) %>% 
  separate(offense_date, c("date", "time"), sep = " ", extra = "merge") %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  write_csv("Data/charge_history.csv", append = TRUE)

message(paste0("Scraping Activity... ", format.POSIXct(as.POSIXct(Sys.time(), tz = "GMT"), tz = "America/Chicago", usetz = TRUE)))
read_html("https://www.iowa-city.org/IcgovApps/Police/ActivityLog") %>% 
  html_element(".body-content") %>% 
  html_table() %>% 
  rename_with(~tolower(.x) %>% str_replace_all(" ", "_")) %>% 
  filter(!(dispatch_number %in% activity_hist$dispatch_number)) %>% 
  mutate(link = paste0("https://www.iowa-city.org/IcgovApps/Police/Details?dispatchNumber=", dispatch_number),
         link = map(link, read_html),
         dispatch_time = map_chr(link, ~html_element(.x, "dd:nth-child(4)") %>% html_text()),
         activity_medium = map_chr(link, ~html_element(.x, "dd:nth-child(8)") %>% html_text()),
         location = map_chr(link, ~html_element(.x, "dd:nth-child(12)") %>% html_text()),
         location_detail = map_chr(link, ~html_element(.x, "dd:nth-child(14)") %>% html_text()),
         details_text = map_chr(link, ~html_element(.x, "dd:nth-child(18)") %>% html_text())) %>% 
  select(-link) %>% 
  separate(dispatch_time, c("date", "time"), sep = " ", extra = "merge") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  write_csv("Data/police_activity.csv", append = TRUE)

message(paste0("Script Finished:", format.POSIXct(as.POSIXct(Sys.time(), tz = "GMT"), tz = "America/Chicago", usetz = TRUE)))
